;;; ollama-buddy-rewrite.el --- In-buffer rewrite mode for ollama-buddy -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides in-buffer streaming rewrite with accept/reject functionality.
;; When `ollama-buddy-in-buffer-replace' is enabled, commands that operate
;; on a region stream their response back into the source buffer instead of
;; the chat buffer, replacing the selection with green-highlighted new text.
;; Accept with C-c C-c or reject (restore original) with C-c C-k.

;;; Code:

(require 'json)
(require 'ollama-buddy-core)

(declare-function ollama-buddy--get-real-model-name "ollama-buddy")

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

;;; State variables

(defvar ollama-buddy--rewrite-state nil
  "Plist holding the current rewrite state, nil when idle.
Keys:
  :source-buffer — buffer being edited
  :start-marker  — fixed anchor before the replaced text (type nil)
  :end-marker    — advances past each streaming insertion (type t)
  :original-text — full original region string (for reject)
  :first-content — t until first real token arrives")

(defvar ollama-buddy--rewrite-process nil
  "Active network process for an in-progress rewrite, or nil.")

;;; Minor mode

(defvar ollama-buddy-rewrite-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ollama-buddy-rewrite-accept)
    (define-key map (kbd "C-c C-k") #'ollama-buddy-rewrite-reject)
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
  (when ollama-buddy--rewrite-state
    (let ((source-buf (plist-get ollama-buddy--rewrite-state :source-buffer))
          (start-m    (plist-get ollama-buddy--rewrite-state :start-marker))
          (end-m      (plist-get ollama-buddy--rewrite-state :end-marker)))
      (when (markerp start-m) (set-marker start-m nil))
      (when (markerp end-m)   (set-marker end-m nil))
      (when (buffer-live-p source-buf)
        (with-current-buffer source-buf
          (ollama-buddy-rewrite-mode -1)))))
  (setq ollama-buddy--rewrite-state nil))

(defun ollama-buddy-rewrite-accept ()
  "Accept the pending rewrite, keeping the streamed text in place."
  (interactive)
  (unless ollama-buddy--rewrite-state
    (user-error "No rewrite pending"))
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
  (let* ((source-buf (plist-get ollama-buddy--rewrite-state :source-buffer))
         (start-m    (plist-get ollama-buddy--rewrite-state :start-marker))
         (end-m      (plist-get ollama-buddy--rewrite-state :end-marker))
         (original   (plist-get ollama-buddy--rewrite-state :original-text)))
    ;; Stop the stream first
    (when (and ollama-buddy--rewrite-process
               (process-live-p ollama-buddy--rewrite-process))
      (delete-process ollama-buddy--rewrite-process)
      (setq ollama-buddy--rewrite-process nil))
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

(defun ollama-buddy--rewrite-complete ()
  "Called when the rewrite stream has finished."
  (message "Rewrite complete.  C-c C-c accept  ·  C-c C-k reject"))

;;; Stream filter

(defun ollama-buddy--rewrite-filter (proc data)
  "Process streaming PROC DATA from the rewrite network process."
  (save-match-data
    (let ((pending (concat (or (process-get proc :pending) "") data)))
      ;; Strip HTTP headers if present (first chunk only)
      (when (string-match "^HTTP/.*?\r?\n\r?\n" pending)
        (setq pending (substring pending (match-end 0))))
      ;; Process all complete newline-delimited JSON lines
      (while (string-match "\\([^\n]*\\)\n" pending)
        (let* ((line (match-string 1 pending)))
          (setq pending (substring pending (match-end 0)))
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
      (process-put proc :pending pending))))

;;; Sentinel

(defun ollama-buddy--rewrite-sentinel (proc event)
  "Handle PROC EVENT for the rewrite network process."
  (unless (or (process-live-p proc)
              (string-match-p "finished\\|deleted" event))
    (when ollama-buddy--rewrite-state
      (message "Rewrite: unexpected termination (%s). Use C-c C-k to restore original."
               (string-trim event)))))

;;; Entry point

;;;###autoload
(defun ollama-buddy-rewrite-from-command (source-buf r-start r-end original-text
                                          system-prompt full-prompt model)
  "Stream a rewrite into SOURCE-BUF replacing R-START..R-END.
ORIGINAL-TEXT is saved for rejection.  SYSTEM-PROMPT and FULL-PROMPT
are the message contents.  MODEL is the model name to use."
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
        (setq placeholder-end (point))))
    (setq start-marker (copy-marker r-start nil)
          end-marker   (copy-marker placeholder-end t))
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

(provide 'ollama-buddy-rewrite)
;;; ollama-buddy-rewrite.el ends here
