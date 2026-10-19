;;; ollama-buddy-rewrite.el --- In-buffer rewrite mode for ollama-buddy -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides in-buffer streaming rewrite with accept/reject functionality.
;; When `ollama-buddy-in-buffer-replace' is enabled, commands that operate
;; on a region stream their response back into the source buffer instead of
;; the chat buffer, replacing the selection with green-highlighted new text.
;;
;; Both local Ollama models and registered provider models (prefixes such as
;; `n:' for OpenCode Go) are supported.  Remote providers stream over their
;; own API shape (OpenAI, Claude, or Gemini) using curl.
;;
;; After streaming completes:
;;   C-c C-c  accept  — keep the new text, clear highlight
;;   C-c C-k  reject  — restore original text
;;   C-c d    diff    — toggle an inline diff view beneath the new text
;;
;; The diff view inserts the original text immediately below the rewrite,
;; with word-level smerge highlighting: green on added words in the new
;; text, red/strikethrough on removed words in the original block.
;; Accepting or rejecting automatically removes the diff block.

;;; Code:

(require 'json)
(require 'ollama-buddy-core)

(declare-function ollama-buddy--get-real-model-name "ollama-buddy")
;; Provider module forward declarations
(declare-function ollama-buddy-provider--get-for-model "ollama-buddy-provider")
(declare-function ollama-buddy-provider--resolve-api-type "ollama-buddy-provider")
(declare-function ollama-buddy-provider--resolve-endpoint "ollama-buddy-provider")
(declare-function ollama-buddy-provider--resolve-key "ollama-buddy-provider")
(declare-function ollama-buddy-provider--resolve-extra-headers "ollama-buddy-provider")
(declare-function ollama-buddy-provider-prefix "ollama-buddy-provider")
(declare-function ollama-buddy-provider-temperature "ollama-buddy-provider")
(declare-function ollama-buddy-provider-max-tokens "ollama-buddy-provider")
;; Remote module forward declarations
(declare-function ollama-buddy-remote--get-real-model-name "ollama-buddy-remote")
(declare-function ollama-buddy-remote--openai-extract-content "ollama-buddy-remote")
(declare-function ollama-buddy-remote--claude-extract-content "ollama-buddy-remote")
(declare-function ollama-buddy-remote--gemini-extract-content "ollama-buddy-remote")

;;; Faces

(defface ollama-buddy-rewrite-face
  '((((class color) (background light)) :background "#d4edda" :extend t)
    (((class color) (background dark))  :background "#1a3a1a" :extend t))
  "Face for streamed rewrite content pending acceptance."
  :group 'ollama-buddy)

(defface ollama-buddy-rewrite-pending-face
  '((t :inherit shadow :slant italic))
  "Face for the [Rewriting...] placeholder shown during streaming."
  :group 'ollama-buddy)

(defface ollama-buddy-rewrite-original-face
  '((((class color) (background light)) :background "#fff3cd" :extend t)
    (((class color) (background dark))  :background "#3a3a10" :extend t))
  "Face for the original-text block shown during inline diff inspection."
  :group 'ollama-buddy)

(defface ollama-buddy-rewrite-changed-face
  '((((class color) (background light)) :background "#52be80")
    (((class color) (background dark))  :background "#1e8449"))
  "Face for added/changed words in the new text during diff inspection.
Applied as smerge overlays on top of `ollama-buddy-rewrite-face'."
  :group 'ollama-buddy)

(defface ollama-buddy-rewrite-removed-face
  '((((class color) (background light)) :background "#f5c6cb" :strike-through t)
    (((class color) (background dark))  :background "#5a1a1a" :strike-through t))
  "Face for removed words in the original text block during diff inspection."
  :group 'ollama-buddy)

;;; State variables

(defvar ollama-buddy--rewrite-state nil
  "Plist holding the current rewrite state, nil when idle.
Keys:
  :source-buffer   — buffer being edited
  :start-marker    — fixed anchor before the replaced text (type nil)
  :end-marker      — end of new text; type t during streaming, nil after
  :original-text   — full original region string (for reject)
  :first-content   — t until first real token arrives
  :diff-end-marker — end of the inline diff block when visible, else nil")

(defvar ollama-buddy--rewrite-process nil
  "Active network process for an in-progress rewrite, or nil.")

;;; Minor mode

(defvar ollama-buddy-rewrite-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ollama-buddy-rewrite-accept)
    (define-key map (kbd "C-c C-k") #'ollama-buddy-rewrite-reject)
    (define-key map (kbd "C-c d")   #'ollama-buddy-rewrite-show-diff)
    map)
  "Keymap for `ollama-buddy-rewrite-mode'.")

(define-minor-mode ollama-buddy-rewrite-mode
  "Active while a rewrite is pending acceptance or rejection.
\\{ollama-buddy-rewrite-mode-map}"
  :lighter " [Rewrite?]"
  :keymap ollama-buddy-rewrite-mode-map)

;;; Helper functions

(defun ollama-buddy--rewrite-cleanup ()
  "Kill the rewrite process and clean up all state."
  (when (and ollama-buddy--rewrite-process
             (process-live-p ollama-buddy--rewrite-process))
    (delete-process ollama-buddy--rewrite-process))
  (setq ollama-buddy--rewrite-process nil)
  (unwind-protect
      (when ollama-buddy--rewrite-state
        (let ((source-buf  (plist-get ollama-buddy--rewrite-state :source-buffer))
              (start-m     (plist-get ollama-buddy--rewrite-state :start-marker))
              (end-m       (plist-get ollama-buddy--rewrite-state :end-marker))
              (diff-end    (plist-get ollama-buddy--rewrite-state :diff-end-marker)))
          (when (markerp start-m)  (set-marker start-m nil))
          (when (markerp end-m)    (set-marker end-m nil))
          (when (markerp diff-end) (set-marker diff-end nil))
          (when (buffer-live-p source-buf)
            (with-current-buffer source-buf
              (ollama-buddy-rewrite-mode -1)))))
    (setq ollama-buddy--rewrite-state nil)))

(defun ollama-buddy--rewrite-hide-diff ()
  "Remove the inline diff block and word-level overlays, keeping state intact."
  (let* ((diff-end  (plist-get ollama-buddy--rewrite-state :diff-end-marker))
         (source-buf (plist-get ollama-buddy--rewrite-state :source-buffer))
         (start-m   (plist-get ollama-buddy--rewrite-state :start-marker))
         (end-m     (plist-get ollama-buddy--rewrite-state :end-marker)))
    (when (and (markerp diff-end) (buffer-live-p source-buf))
      (with-current-buffer source-buf
        (let ((inhibit-read-only t))
          ;; Remove the diff block (end-m..diff-end)
          (delete-region (marker-position end-m)
                         (marker-position diff-end))
          ;; Remove smerge overlays from new text region
          (remove-overlays (marker-position start-m)
                           (marker-position end-m)))))
    (when (markerp diff-end) (set-marker diff-end nil))
    (setq ollama-buddy--rewrite-state
          (plist-put ollama-buddy--rewrite-state :diff-end-marker nil))))

(defun ollama-buddy-rewrite-accept ()
  "Accept the pending rewrite, keeping the streamed text in place."
  (interactive)
  (unless ollama-buddy--rewrite-state
    (user-error "No rewrite pending"))
  ;; Remove diff view if open before finalising
  (when (plist-get ollama-buddy--rewrite-state :diff-end-marker)
    (ollama-buddy--rewrite-hide-diff))
  (let* ((source-buf (plist-get ollama-buddy--rewrite-state :source-buffer))
         (start-m    (plist-get ollama-buddy--rewrite-state :start-marker))
         (end-m      (plist-get ollama-buddy--rewrite-state :end-marker)))
    (when (buffer-live-p source-buf)
      (with-current-buffer source-buf
        (let ((inhibit-read-only t))
          (remove-text-properties (marker-position start-m)
                                  (marker-position end-m)
                                  '(face nil ollama-buddy-rewrite nil))))))
  (ollama-buddy--rewrite-cleanup)
  (message "Rewrite accepted."))

(defun ollama-buddy-rewrite-reject ()
  "Reject the pending rewrite, restoring the original selected text."
  (interactive)
  (unless ollama-buddy--rewrite-state
    (user-error "No rewrite pending"))
  ;; Stop the stream first
  (when (and ollama-buddy--rewrite-process
             (process-live-p ollama-buddy--rewrite-process))
    (delete-process ollama-buddy--rewrite-process)
    (setq ollama-buddy--rewrite-process nil))
  ;; Remove diff view if open before restoring
  (when (plist-get ollama-buddy--rewrite-state :diff-end-marker)
    (ollama-buddy--rewrite-hide-diff))
  (let* ((source-buf (plist-get ollama-buddy--rewrite-state :source-buffer))
         (start-m    (plist-get ollama-buddy--rewrite-state :start-marker))
         (end-m      (plist-get ollama-buddy--rewrite-state :end-marker))
         (original   (plist-get ollama-buddy--rewrite-state :original-text)))
    (when (buffer-live-p source-buf)
      (with-current-buffer source-buf
        (let ((inhibit-read-only t))
          (delete-region (marker-position start-m)
                         (marker-position end-m))
          (goto-char (marker-position start-m))
          (insert original)))))
  (ollama-buddy--rewrite-cleanup)
  (message "Rewrite rejected — original restored."))

(defun ollama-buddy--rewrite-insert-content (content)
  "Insert streaming CONTENT into the source buffer at the rewrite position."
  (when ollama-buddy--rewrite-state
    (let* ((source-buf (plist-get ollama-buddy--rewrite-state :source-buffer))
           (start-m    (plist-get ollama-buddy--rewrite-state :start-marker))
           (end-m      (plist-get ollama-buddy--rewrite-state :end-marker))
           (first-p    (plist-get ollama-buddy--rewrite-state :first-content)))
      (when (buffer-live-p source-buf)
        (with-current-buffer source-buf
          (let ((inhibit-read-only t))
            ;; On first real content token, remove the placeholder
            (when first-p
              (delete-region (marker-position start-m)
                             (marker-position end-m))
              (setq ollama-buddy--rewrite-state
                    (plist-put ollama-buddy--rewrite-state :first-content nil)))
            ;; Insert content at end-marker position; end-m (type t) advances
            (save-excursion
              (goto-char (marker-position end-m))
              (insert content))
            ;; Apply face across the entire rewrite range
            (add-text-properties (marker-position start-m)
                                 (marker-position end-m)
                                 '(face ollama-buddy-rewrite-face
                                   ollama-buddy-rewrite t))))))))

(defun ollama-buddy--rewrite-strip-fences ()
  "Strip markdown code-block fences from the rewrite region if present.
Handles opening fences with optional language specifier (e.g. ```lisp)
and the matching closing fence, replacing the region with the inner content."
  (when ollama-buddy--rewrite-state
    (let* ((source-buf (plist-get ollama-buddy--rewrite-state :source-buffer))
           (start-m    (plist-get ollama-buddy--rewrite-state :start-marker))
           (end-m      (plist-get ollama-buddy--rewrite-state :end-marker)))
      (when (buffer-live-p source-buf)
        (with-current-buffer source-buf
          (let* ((inhibit-read-only t)
                 (content (string-trim
                           (buffer-substring-no-properties
                            (marker-position start-m)
                            (marker-position end-m)))))
            (when (string-match "\\`[ \t]*```[^\n]*\n" content)
              (let* ((after-open (substring content (match-end 0)))
                     (inner (if (string-match "\n[ \t]*```[ \t]*\\'" after-open)
                                (substring after-open 0 (match-beginning 0))
                              after-open)))
                (delete-region (marker-position start-m)
                               (marker-position end-m))
                (goto-char (marker-position start-m))
                (insert inner)
                (add-text-properties (marker-position start-m)
                                     (marker-position end-m)
                                     '(face ollama-buddy-rewrite-face
                                       ollama-buddy-rewrite t))))))))))

(defun ollama-buddy--rewrite-complete ()
  "Called when the rewrite stream has finished."
  (ollama-buddy--rewrite-strip-fences)
  ;; Switch end-m to non-advancing so diff block insertions at its position
  ;; do not push it forward (streaming is complete; no more content arrives).
  (let ((end-m (plist-get ollama-buddy--rewrite-state :end-marker)))
    (when (markerp end-m)
      (set-marker-insertion-type end-m nil)))
  ;; Explicitly close the process so show-diff's liveness check passes immediately.
  (when (and ollama-buddy--rewrite-process
             (process-live-p ollama-buddy--rewrite-process))
    (delete-process ollama-buddy--rewrite-process))
  (setq ollama-buddy--rewrite-process nil)
  (message "Rewrite complete.  C-c C-c accept  ·  C-c C-k reject  ·  C-c d diff"))

;;; Inline diff view

(defun ollama-buddy-rewrite-show-diff ()
  "Toggle an inline diff view beneath the rewritten text.
The original text is inserted immediately below the new (green) region with a
yellow background.  `smerge-refine-regions' then highlights word-level
differences: green overlays on added/changed words in the new text, red
strikethrough overlays on removed words in the original block.

Calling this command again hides the diff block without accepting or rejecting.
Accepting (\\[ollama-buddy-rewrite-accept]) or rejecting
(\\[ollama-buddy-rewrite-reject]) also removes the diff block automatically."
  (interactive)
  (unless ollama-buddy--rewrite-state
    (user-error "No rewrite pending"))
  (when (and ollama-buddy--rewrite-process
             (process-live-p ollama-buddy--rewrite-process))
    (user-error "Rewrite still streaming — wait for completion before diffing"))
  (cond
   ;; Toggle off: diff block already showing — remove it
   ((plist-get ollama-buddy--rewrite-state :diff-end-marker)
    (ollama-buddy--rewrite-hide-diff)
    (message "Diff hidden.  C-c C-c accept  ·  C-c C-k reject"))
   ;; Toggle on: insert diff block beneath new text
   (t
    (let* ((source-buf (plist-get ollama-buddy--rewrite-state :source-buffer))
           (start-m    (plist-get ollama-buddy--rewrite-state :start-marker))
           (end-m      (plist-get ollama-buddy--rewrite-state :end-marker))
           (original   (plist-get ollama-buddy--rewrite-state :original-text)))
      (unless (buffer-live-p source-buf)
        (user-error "Source buffer no longer available"))
      (with-current-buffer source-buf
        (let* ((inhibit-read-only t)
               (insert-pos (marker-position end-m))
               orig-start orig-end diff-end-pos)
          (save-excursion
            (goto-char insert-pos)
            ;; Header separator
            (insert "\n\n── Original " (make-string 40 ?─) "\n")
            (setq orig-start (point))
            ;; Original text with its own background face
            (insert original)
            (setq orig-end (point))
            (add-text-properties orig-start orig-end
                                 '(face ollama-buddy-rewrite-original-face
                                   ollama-buddy-rewrite-diff t))
            ;; Footer separator
            (insert "\n" (make-string 53 ?─) "\n")
            (setq diff-end-pos (point))
            ;; Apply the original face to the separators too
            (add-text-properties insert-pos diff-end-pos
                                 '(face ollama-buddy-rewrite-original-face
                                   ollama-buddy-rewrite-diff t)))
          ;; Store the end marker for the whole diff block
          (setq ollama-buddy--rewrite-state
                (plist-put ollama-buddy--rewrite-state
                           :diff-end-marker
                           (copy-marker diff-end-pos nil)))
          ;; Apply word-level smerge diff between the two sections
          ;; Both regions are now in the same buffer, so plain positions work.
          (when (and (require 'smerge-mode nil t)
                     (fboundp 'smerge-refine-regions))
            (condition-case nil
                (smerge-refine-regions
                 (copy-marker orig-start nil)   ; original block start
                 (copy-marker orig-end nil)     ; original block end
                 (copy-marker (marker-position start-m) nil) ; new text start
                 (copy-marker (marker-position end-m) nil)   ; new text end
                 nil nil
                 '((face . ollama-buddy-rewrite-removed-face))  ; removals → original block
                 '((face . ollama-buddy-rewrite-changed-face))) ; additions → new text
              (error nil)))
          (message
           "Diff shown.  C-c C-c accept  ·  C-c C-k reject  ·  C-c d hide")))))))

;;; Stream filter

(defun ollama-buddy--rewrite-filter (proc data)
  "Process streaming PROC DATA from the rewrite network process."
  (save-match-data
    (let ((pending (concat (or (process-get proc :pending) "") data))
          (start 0))
      ;; Strip HTTP headers if present (first chunk only)
      (when (string-match "^HTTP/.*?\r?\n\r?\n" pending)
        (setq start (match-end 0)))
      ;; Process all complete newline-delimited JSON lines
      (while (string-match "\\([^\n]*\\)\n" pending start)
        (let* ((line (match-string 1 pending)))
          (setq start (match-end 0))
          (let* ((json-str (replace-regexp-in-string "^[^{]*" "" line))
                 (json-data (when (and (stringp json-str)
                                       (> (length json-str) 0))
                              (condition-case nil
                                  (json-read-from-string json-str)
                                (error nil)))))
            (when json-data
              (let* ((message-data (alist-get 'message json-data))
                     (content (when message-data
                                (alist-get 'content message-data)))
                     (done (alist-get 'done json-data)))
                (when (and content
                           (stringp content)
                           (not (string-empty-p content)))
                  (ollama-buddy--rewrite-insert-content content))
                (when (eq done t)
                  (ollama-buddy--rewrite-complete)))))))
      (process-put proc :pending (substring pending start)))))

;;; Sentinel

(defun ollama-buddy--rewrite-sentinel (proc event)
  "Handle PROC EVENT for the rewrite network process."
  (unless (or (process-live-p proc)
              (string-match-p "finished\\|deleted" event))
    (when ollama-buddy--rewrite-state
      (message "Rewrite: unexpected termination (%s). Use C-c C-k to restore original."
               (string-trim event)))))

;;; Local Ollama entry point

(defun ollama-buddy-rewrite--start-local (source-buf r-start r-end original-text
                                          system-prompt full-prompt model)
  "Stream a rewrite into SOURCE-BUF replacing R-START..R-END.
Uses the local Ollama server.  ORIGINAL-TEXT is saved for rejection.
SYSTEM-PROMPT and FULL-PROMPT are the message contents.  MODEL is the
local Ollama model name to use."
  ;; Guards
  (when ollama-buddy--rewrite-state
    (user-error "Rewrite already pending — accept or reject first (C-c C-c / C-c C-k)"))
  (unless (ollama-buddy--ollama-running)
    (user-error "Ollama is not running"))
  (deactivate-mark)
  (let* ((real-model (if (fboundp 'ollama-buddy--get-real-model-name)
                         (ollama-buddy--get-real-model-name model)
                       model))
         (messages (if (and system-prompt
                            (stringp system-prompt)
                            (not (string-empty-p system-prompt)))
                       (vector `((role . "system") (content . ,system-prompt))
                               `((role . "user")   (content . ,full-prompt)))
                     (vector `((role . "user") (content . ,full-prompt)))))
         (payload (json-encode `((model    . ,real-model)
                                 (messages . ,messages)
                                 (stream   . t))))
         (http-request (concat "POST /api/chat HTTP/1.1\r\n"
                               (format "Host: %s:%d\r\n"
                                       ollama-buddy-host ollama-buddy-port)
                               "Content-Type: application/json\r\n"
                               (format "Content-Length: %d\r\n\r\n"
                                       (string-bytes payload))
                               payload))
         placeholder-end start-marker end-marker)
    ;; Insert placeholder in source buffer
    (with-current-buffer source-buf
      (let ((inhibit-read-only t))
        (delete-region r-start r-end)
        (goto-char r-start)
        (insert "[Rewriting...]")
        (add-text-properties r-start (point)
                             '(face ollama-buddy-rewrite-pending-face))
        (setq placeholder-end (point)))
      (setq start-marker (copy-marker r-start nil)
            end-marker   (copy-marker placeholder-end t)))
    ;; Initialize state
    (setq ollama-buddy--rewrite-state
          (list :source-buffer source-buf
                :start-marker  start-marker
                :end-marker    end-marker
                :original-text original-text
                :first-content t))
    ;; Create network process
    (condition-case err
        (setq ollama-buddy--rewrite-process
              (make-network-process
               :name     "ollama-buddy-rewrite"
               :buffer   nil
               :host     ollama-buddy-host
               :service  ollama-buddy-port
               :coding   'utf-8
               :filter   #'ollama-buddy--rewrite-filter
               :sentinel #'ollama-buddy--rewrite-sentinel))
      (error
       ;; Restore original text if connection fails
       (with-current-buffer source-buf
         (let ((inhibit-read-only t))
           (delete-region (marker-position start-marker)
                          (marker-position end-marker))
           (goto-char (marker-position start-marker))
           (insert original-text)))
       (set-marker start-marker nil)
       (set-marker end-marker nil)
       (setq ollama-buddy--rewrite-state nil)
       (error "Failed to connect to Ollama: %s" (error-message-string err))))
    ;; Activate minor mode immediately so C-c C-k cancels during streaming
    (with-current-buffer source-buf
      (ollama-buddy-rewrite-mode 1))
    ;; Send the HTTP request
    (process-send-string ollama-buddy--rewrite-process http-request)
    (message "Rewriting with %s... (C-c C-k to cancel)" model)))

;;; Remote provider support

(defun ollama-buddy-rewrite--remote-request-parts
    (provider model system-prompt full-prompt)
  "Build the HTTP request pieces for a remote rewrite.
PROVIDER is the registered provider struct, MODEL is the prefixed
model name, SYSTEM-PROMPT and FULL-PROMPT are the message contents.
Returns a list (ENDPOINT HEADERS JSON-STR EXTRACTOR)."
  (let* ((api-type (ollama-buddy-provider--resolve-api-type provider model))
         (endpoint (ollama-buddy-provider--resolve-endpoint provider model))
         (api-key (ollama-buddy-provider--resolve-key provider))
         (extra-headers (ollama-buddy-provider--resolve-extra-headers provider))
         (prefix (ollama-buddy-provider-prefix provider))
         (bare-model (ollama-buddy-remote--get-real-model-name prefix model))
         (temperature (ollama-buddy-provider-temperature provider))
         (max-tokens (or (ollama-buddy-provider-max-tokens provider) 4096))
         (has-system (and system-prompt
                          (stringp system-prompt)
                          (not (string-empty-p system-prompt)))))
    (pcase api-type
      ('claude
       (let* ((payload `((model . ,bare-model)
                         (messages . [((role . "user")
                                       (content . ,full-prompt))])
                         (temperature . ,temperature)
                         (max_tokens . ,max-tokens)
                         (stream . t)))
              (payload (if has-system
                           (append payload `((system . ,system-prompt)))
                         payload)))
         (list endpoint
               (append `(("Content-Type" . "application/json")
                         ("Authorization" . ,(concat "Bearer " api-key))
                         ("X-API-Key" . ,api-key)
                         ("anthropic-version" . "2023-06-01"))
                       extra-headers)
               (json-encode payload)
               #'ollama-buddy-remote--claude-extract-content)))
      ('gemini
       (let* ((model-endpoint (format endpoint bare-model))
              (stream-endpoint
               (concat (replace-regexp-in-string
                        ":generateContent$" ":streamGenerateContent"
                        model-endpoint)
                       "?alt=sse&key=" api-key))
              (payload `((contents
                          . [((role . "user")
                              (parts . [((text . ,full-prompt))]))])
                         (generationConfig
                          . ((temperature . ,temperature)
                             (maxOutputTokens . ,max-tokens)))))
              (payload (if has-system
                           (append payload
                                   `((systemInstruction
                                      . ((parts . [((text . ,system-prompt))])))))
                         payload)))
         (list stream-endpoint
               (append '(("Content-Type" . "application/json"))
                       extra-headers)
               (json-encode payload)
               #'ollama-buddy-remote--gemini-extract-content)))
      (_
       (let* ((messages (append
                         (when has-system
                           (list `((role . "system")
                                   (content . ,system-prompt))))
                         (list `((role . "user")
                                 (content . ,full-prompt)))))
              (payload `((model . ,bare-model)
                         (messages . ,(vconcat [] messages))
                         (temperature . ,temperature)
                         (max_tokens . ,max-tokens)
                         (stream . t))))
         (list endpoint
               (append `(("Content-Type" . "application/json")
                         ("Authorization" . ,(concat "Bearer " api-key)))
                       extra-headers)
               (json-encode payload)
               #'ollama-buddy-remote--openai-extract-content))))))

(defun ollama-buddy-rewrite--format-error (body)
  "Extract a readable message from an API error BODY."
  (or (condition-case nil
          (let* ((json-object-type 'alist)
                 (json-key-type 'symbol)
                 (data (json-read-from-string body))
                 (err (alist-get 'error data)))
            (cond
             ((stringp err) err)
             ((listp err) (or (alist-get 'message err)
                              (alist-get 'type err)
                              (format "%S" err)))))
        (error nil))
      (if (> (length body) 200)
          (concat (substring body 0 200) "…")
        body)))

(defun ollama-buddy-rewrite--fail (reason)
  "Restore the original region after a failed rewrite.
REASON is a human-readable failure description."
  (when ollama-buddy--rewrite-state
    (let ((source-buf (plist-get ollama-buddy--rewrite-state :source-buffer))
          (start-m    (plist-get ollama-buddy--rewrite-state :start-marker))
          (end-m      (plist-get ollama-buddy--rewrite-state :end-marker))
          (original   (plist-get ollama-buddy--rewrite-state :original-text)))
      (when (buffer-live-p source-buf)
        (with-current-buffer source-buf
          (let ((inhibit-read-only t))
            (delete-region (marker-position start-m) (marker-position end-m))
            (goto-char (marker-position start-m))
            (insert original))))))
  (ollama-buddy--rewrite-cleanup)
  (message "Rewrite failed: %s" reason))

(defun ollama-buddy-rewrite--remote-process-buffer (proc)
  "Process any complete output currently in PROC's process buffer.
Called both from the process filter and once more from the sentinel, so
output that arrived before the filter was attached is not lost."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (condition-case err
          (with-current-buffer buffer
            ;; Strip HTTP headers (and any informational 1xx blocks) once.
            (unless (process-get proc :headers-done)
              (goto-char (point-min))
              (while (and (not (process-get proc :headers-done))
                          (re-search-forward "\r?\n\r?\n" nil t))
                (let* ((header-text (buffer-substring (point-min) (point)))
                       (status (and (string-match
                                     "^HTTP/[0-9.]+ \\([0-9]+\\)"
                                     header-text)
                                    (string-to-number (match-string 1 header-text)))))
                  (delete-region (point-min) (point))
                  (when (and status (>= status 200))
                    (unless (and (>= status 200) (< status 300))
                      (process-put proc :http-error status))
                    (process-put proc :headers-done t)))))
            ;; Process complete SSE data lines.  Leave partial output alone
            ;; until the HTTP headers have been fully received.
            (when (and (process-get proc :headers-done)
                       (not (process-get proc :http-error)))
              (goto-char (point-min))
              (while (re-search-forward "^\\(.*\\)\n" nil t)
                (let ((line (match-string 1)))
                  (delete-region (match-beginning 0) (match-end 0))
                  (when (string-prefix-p "data:" line)
                    (let* ((data-str (string-trim (substring line 5)))
                           (content (condition-case nil
                                        (funcall (process-get proc :extractor)
                                                 data-str)
                                      (error nil))))
                      (when (and content (stringp content)
                                 (not (string-empty-p content)))
                        (ollama-buddy--rewrite-insert-content content))))))))
        (error
         (message "Error in remote rewrite filter: %s"
                  (error-message-string err)))))))

(defun ollama-buddy-rewrite--remote-filter (proc output)
  "Process filter for the remote rewrite curl process PROC.
OUTPUT is the newly received chunk of the response."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert output))))
  (ollama-buddy-rewrite--remote-process-buffer proc))

(defun ollama-buddy-rewrite--remote-sentinel (proc event)
  "Sentinel for the remote rewrite curl process PROC.
EVENT is the process state change string."
  (let ((buffer (process-buffer proc))
        (temp-file (process-get proc :temp-file)))
    (unwind-protect
        (progn
          ;; Make sure output that arrived before the filter was attached is
          ;; processed too.
          (ollama-buddy-rewrite--remote-process-buffer proc)
          (cond
           ;; Cancelled by `ollama-buddy-rewrite-reject' — it restores the text.
           ((string-match-p "\\(?:killed\\|terminated\\)" event)
            nil)
           ;; Successful completion — finalise the pending rewrite.
           ((and (string-match-p "finished" event)
                 (zerop (or (process-exit-status proc) 0))
                 (null (process-get proc :http-error)))
            (when (and ollama-buddy--rewrite-state
                       (eq proc ollama-buddy--rewrite-process))
              (setq ollama-buddy--rewrite-process nil)
              (ollama-buddy--rewrite-complete)))
           (t
            (let ((body (and (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (string-trim (buffer-string))))))
              (ollama-buddy-rewrite--fail
               (cond
                ((process-get proc :http-error)
                 (format "HTTP %s: %s"
                         (process-get proc :http-error)
                         (if (or (null body) (string-empty-p body))
                             "request failed"
                           (ollama-buddy-rewrite--format-error body))))
                ((not (zerop (or (process-exit-status proc) 0)))
                 (format "curl exited with status %s"
                         (process-exit-status proc)))
                (t (string-trim event))))))))
      (when (and temp-file (file-exists-p temp-file))
        (ignore-errors (delete-file temp-file)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun ollama-buddy-rewrite--start-remote (source-buf r-start r-end
                                                       original-text
                                                       system-prompt full-prompt
                                                       model provider)
  "Start an in-buffer rewrite against remote PROVIDER for MODEL.
SOURCE-BUF, R-START, R-END and ORIGINAL-TEXT describe the region to
replace.  SYSTEM-PROMPT and FULL-PROMPT are the message contents."
  (pcase-let ((`(,endpoint ,headers ,json-str ,extractor)
               (ollama-buddy-rewrite--remote-request-parts
                provider model system-prompt full-prompt)))
    (let* ((temp-file (make-temp-file "ollama-buddy-rewrite-payload"))
           (process-buffer (generate-new-buffer " *ollama-buddy-rewrite*"))
           placeholder-end start-marker end-marker)
      (with-temp-file temp-file
        (insert json-str))
      ;; Replace the selection with the pending placeholder.
      (with-current-buffer source-buf
        (let ((inhibit-read-only t))
          (delete-region r-start r-end)
          (goto-char r-start)
          (insert "[Rewriting...]")
          (add-text-properties r-start (point)
                               '(face ollama-buddy-rewrite-pending-face))
          (setq placeholder-end (point)))
        (setq start-marker (copy-marker r-start nil)
              end-marker (copy-marker placeholder-end t)))
      (setq ollama-buddy--rewrite-state
            (list :source-buffer source-buf
                  :start-marker start-marker
                  :end-marker end-marker
                  :original-text original-text
                  :first-content t))
      (condition-case err
          (let* ((header-args
                  (apply #'append
                         (mapcar (lambda (h)
                                   (list "--header"
                                         (format "%s: %s" (car h) (cdr h))))
                                 headers)))
                 (curl-args
                  (append
                   (list "--silent" "--no-buffer" "--include"
                         "--max-time" (number-to-string ollama-buddy-curl-timeout)
                         "--request" "POST")
                   header-args
                   (list "--data-binary" (concat "@" temp-file)
                         endpoint)))
                 (proc (make-process
                        :name "ollama-buddy-rewrite"
                        :buffer process-buffer
                        :command (cons ollama-buddy-curl-executable curl-args)
                        :filter #'ollama-buddy-rewrite--remote-filter
                        :sentinel #'ollama-buddy-rewrite--remote-sentinel
                        :coding 'utf-8
                        :noquery t)))
            (process-put proc :temp-file temp-file)
            (process-put proc :extractor extractor)
            (setq ollama-buddy--rewrite-process proc))
        (error
         ;; Restore the original text if the process cannot start.
         (when (file-exists-p temp-file)
           (ignore-errors (delete-file temp-file)))
         (when (buffer-live-p process-buffer)
           (kill-buffer process-buffer))
         (with-current-buffer source-buf
           (let ((inhibit-read-only t))
             (delete-region (marker-position start-marker)
                            (marker-position end-marker))
             (goto-char (marker-position start-marker))
             (insert original-text)))
         (set-marker start-marker nil)
         (set-marker end-marker nil)
         (setq ollama-buddy--rewrite-state nil)
         (error "Failed to start remote rewrite: %s"
                (error-message-string err))))
      (with-current-buffer source-buf
        (ollama-buddy-rewrite-mode 1))
      (message "Rewriting with %s... (C-c C-k to cancel)" model))))

;;; Entry point

;;;###autoload
(defun ollama-buddy-rewrite-from-command (source-buf r-start r-end original-text
                                          system-prompt full-prompt model)
  "Stream a rewrite into SOURCE-BUF replacing R-START..R-END.
ORIGINAL-TEXT is saved for rejection.  SYSTEM-PROMPT and FULL-PROMPT
are the message contents.  MODEL is the model name to use — either a
local Ollama model or a provider-prefixed model such as
\"n:deepseek-v4.1-flash\"."
  (when ollama-buddy--rewrite-state
    (user-error
     "Rewrite already pending — accept or reject first (C-c C-c / C-c C-k)"))
  (deactivate-mark)
  (let ((provider (and (fboundp 'ollama-buddy-provider--get-for-model)
                       (ollama-buddy-provider--get-for-model model))))
    (if provider
        (ollama-buddy-rewrite--start-remote
         source-buf r-start r-end original-text
         system-prompt full-prompt model provider)
      (ollama-buddy-rewrite--start-local
       source-buf r-start r-end original-text
       system-prompt full-prompt model))))

(provide 'ollama-buddy-rewrite)
;;; ollama-buddy-rewrite.el ends here
