;;; ollama-buddy-completion.el --- Inline ghost-text completions -*- lexical-binding: t; -*-

;;; Commentary:
;; Ghost-text code completions powered by Ollama's /api/generate endpoint.
;;
;; Enable `ollama-buddy-completion-mode' in a buffer, then press C-c TAB
;; to trigger a completion.  Press TAB to accept, anything else dismisses.
;;
;; Requires `ollama-buddy-completion-model' to be set (or a model already
;; selected in the ollama-buddy chat buffer).

;;; Code:

(require 'json)
(require 'ollama-buddy-core)

;;; Customisation

(defgroup ollama-buddy-completion nil
  "Inline ghost-text completions for ollama-buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-completion-")

(defcustom ollama-buddy-completion-model nil
  "Model to use for completions.
nil means fall back to `ollama-buddy--current-model'."
  :type '(choice (const :tag "Use current model" nil)
                 (string :tag "Model name"))
  :group 'ollama-buddy-completion)

(defcustom ollama-buddy-completion-max-tokens 80
  "Maximum tokens to generate per completion."
  :type 'integer
  :group 'ollama-buddy-completion)

(defcustom ollama-buddy-completion-context-lines 50
  "Lines of text before point to send as the prompt prefix."
  :type 'integer
  :group 'ollama-buddy-completion)

(defcustom ollama-buddy-completion-suffix-lines 20
  "Lines of text after point to send as the suffix for fill-in-the-middle."
  :type 'integer
  :group 'ollama-buddy-completion)

;;; Face

(defface ollama-buddy-completion-face
  '((((class color) (background dark))
     :foreground "#7a9ec2" :slant italic)
    (((class color) (background light))
     :foreground "#6272a4" :slant italic)
    (t :slant italic))
  "Face for ghost-text inline completions.
Uses a distinct blue-grey so the suggestion is clearly visible regardless
of theme, without conflicting with normal text or comments."
  :group 'ollama-buddy-completion)

;;; Buffer-local state

(defvar-local ollama-buddy-completion--overlay  nil)
(defvar-local ollama-buddy-completion--process  nil)
(defvar-local ollama-buddy-completion--position nil)
(defvar-local ollama-buddy-completion--text     "")

;;; Helpers

(defun ollama-buddy-completion--model ()
  "Return the actual model name to use (provider prefix stripped)."
  (let ((raw (or ollama-buddy-completion-model
                 (and (boundp 'ollama-buddy--current-model)
                      ollama-buddy--current-model)
                 "")))
    (ollama-buddy--get-real-model-name raw)))

(defun ollama-buddy-completion--dismiss ()
  "Remove the ghost-text overlay and kill any in-progress request."
  (when (and ollama-buddy-completion--process
             (process-live-p ollama-buddy-completion--process))
    (delete-process ollama-buddy-completion--process))
  (setq ollama-buddy-completion--process nil)
  (when (overlayp ollama-buddy-completion--overlay)
    (delete-overlay ollama-buddy-completion--overlay))
  (setq ollama-buddy-completion--overlay nil
        ollama-buddy-completion--text     ""))

(defun ollama-buddy-completion--clean-text (text)
  "Strip markdown code fences and leading/trailing blank lines from TEXT."
  (let ((s text))
    ;; Remove opening fence (```lang or ```)
    (when (string-match "\\`[ \t]*```[a-z]*[ \t]*\n?" s)
      (setq s (substring s (match-end 0))))
    ;; Remove closing fence
    (when (string-match "\n?[ \t]*```[ \t]*\\'" s)
      (setq s (substring s 0 (match-beginning 0))))
    ;; Trim leading blank lines
    (when (string-match "\\`\n+" s)
      (setq s (substring s (match-end 0))))
    s))

(defun ollama-buddy-completion--show (text)
  "Display TEXT as multi-line ghost text at the trigger position."
  (when (and (buffer-live-p (current-buffer))
             ollama-buddy-completion--position)
    (let ((display (ollama-buddy-completion--clean-text text)))
      (when (> (length display) 0)
        (unless (overlayp ollama-buddy-completion--overlay)
          (setq ollama-buddy-completion--overlay
                (make-overlay ollama-buddy-completion--position
                              ollama-buddy-completion--position
                              (current-buffer) t t)))
        (overlay-put ollama-buddy-completion--overlay
                     'after-string
                     (propertize display 'face 'ollama-buddy-completion-face))))))

;;; Response parsing

(defun ollama-buddy-completion--extract-text (raw)
  "Return (text . done) by parsing all complete JSON lines in RAW.
RAW is the full accumulated HTTP response including headers."
  (let* ((body-start (when (string-match "\r?\n\r?\n" raw)
                       (match-end 0)))
         (body       (if body-start (substring raw body-start) nil))
         (text       "")
         (thinking   "")
         (done       nil))
    (when body
      (dolist (line (split-string body "\n"))
        (let ((l (string-trim line)))
          (when (string-prefix-p "{" l)
            (condition-case nil
                (let* ((json-object-type 'alist)
                       (json-key-type    'symbol)
                       (obj (json-read-from-string l)))
                  (when-let ((e (alist-get 'error obj)))
                    (message "ollama-buddy-completion: %s" e))
                  (let ((r  (alist-get 'response obj))
                        (tk (alist-get 'thinking obj))
                        (d  (alist-get 'done     obj)))
                    (when (and (stringp r) (> (length r) 0))
                      (setq text (concat text r)))
                    (when (and (stringp tk) (> (length tk) 0))
                      (setq thinking (concat thinking tk)))
                    (when (eq d t)
                      (setq done t))))
              (error nil))))))
    ;; Use thinking tokens as fallback when response is empty.
    (when (string-empty-p text)
      (setq text thinking))
    (cons text done)))

;;; Network process

(defun ollama-buddy-completion--filter (proc data)
  "Accumulate DATA from PROC and update the ghost-text overlay."
  (process-put proc :raw (concat (or (process-get proc :raw) "") data))
  (when (buffer-live-p (process-get proc :target-buf))
    (let* ((result (ollama-buddy-completion--extract-text
                    (process-get proc :raw)))
           (text   (car result))
           (done   (cdr result)))
      (with-current-buffer (process-get proc :target-buf)
        (when (> (length text) 0)
          (setq ollama-buddy-completion--text text)
          (ollama-buddy-completion--show text))
        (when done
          (when (and ollama-buddy-completion--process
                     (process-live-p ollama-buddy-completion--process))
            (delete-process ollama-buddy-completion--process))
          (setq ollama-buddy-completion--process nil))))))

(defun ollama-buddy-completion--sentinel (proc event)
  "Clean up PROC on EVENT."
  (unless (process-live-p proc)
    (let ((buf (process-get proc :target-buf)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (eq ollama-buddy-completion--process proc)
            (setq ollama-buddy-completion--process nil)))))))

;;; Core trigger

(defun ollama-buddy-completion--do-trigger ()
  "Internal: initiate a completion request at point."
  (let ((model (ollama-buddy-completion--model)))
    (unless (string-empty-p model)
    (ollama-buddy-completion--dismiss)
    (let* ((pos       (point))
           (ctx-start (save-excursion
                        (forward-line (- ollama-buddy-completion-context-lines))
                        (point)))
           (ctx-end   (save-excursion
                        (forward-line ollama-buddy-completion-suffix-lines)
                        (point)))
           (prefix    (buffer-substring-no-properties ctx-start pos))
           (suffix    (buffer-substring-no-properties pos ctx-end))
           (system-msg "You are a code completion engine. Continue the code from exactly where it left off. Output ONLY the raw code that comes next. Never output markdown fences, backticks, explanations, or comments about the code. Do not repeat code that is already present.")
           (payload   (json-encode
                       `((model   . ,model)
                         (prompt  . ,prefix)
                         (suffix  . ,suffix)
                         (system  . ,system-msg)
                         (stream  . t)
                         (think   . :json-false)
                         (options . ((num_predict . ,ollama-buddy-completion-max-tokens)
                                     (temperature . 0))))))
           (request   (concat "POST /api/generate HTTP/1.1\r\n"
                              (format "Host: %s:%d\r\n"
                                      ollama-buddy-host ollama-buddy-port)
                              "Content-Type: application/json\r\n"
                              "Connection: close\r\n"
                              (format "Content-Length: %d\r\n\r\n"
                                      (string-bytes payload))
                              payload))
           (target    (current-buffer)))
      (setq ollama-buddy-completion--position pos
            ollama-buddy-completion--text     "")
      (condition-case err
          (let ((proc (make-network-process
                       :name     "ollama-buddy-completion"
                       :buffer   nil
                       :host     ollama-buddy-host
                       :service  ollama-buddy-port
                       :coding   'utf-8
                       :filter   #'ollama-buddy-completion--filter
                       :sentinel #'ollama-buddy-completion--sentinel)))
            (process-put proc :raw        "")
            (process-put proc :target-buf target)
            (setq ollama-buddy-completion--process proc)
            (process-send-string proc request))
        (error
         (message "ollama-buddy-completion: connection failed — %s"
                  (error-message-string err))))))))

;;; Pre-command hook — dismiss on any non-accept command

(defun ollama-buddy-completion--pre-command ()
  "Dismiss a visible ghost-text overlay on any non-accept command."
  (unless (memq this-command
                '(ollama-buddy-completion-accept-or-tab))
    (when (overlayp ollama-buddy-completion--overlay)
      (ollama-buddy-completion--dismiss))))

;;; Interactive commands

(defun ollama-buddy-completion-accept-or-tab ()
  "Accept the ghost-text completion, or run `indent-for-tab-command'."
  (interactive)
  (if (and (overlayp ollama-buddy-completion--overlay)
           (> (length ollama-buddy-completion--text) 0))
      (let ((text (ollama-buddy-completion--clean-text
                   ollama-buddy-completion--text)))
        (ollama-buddy-completion--dismiss)
        (insert text))
    (indent-for-tab-command)))

(defun ollama-buddy-completion-trigger ()
  "Trigger a completion at point immediately."
  (interactive)
  (ollama-buddy-completion--do-trigger))

;;; Minor mode

(defvar ollama-buddy-completion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB")     #'ollama-buddy-completion-accept-or-tab)
    (define-key map (kbd "C-c TAB") #'ollama-buddy-completion-trigger)
    map)
  "Keymap for `ollama-buddy-completion-mode'.")

(define-minor-mode ollama-buddy-completion-mode
  "Inline ghost-text completions powered by Ollama.
Trigger with \\[ollama-buddy-completion-trigger], accept with \\[ollama-buddy-completion-accept-or-tab].
\\{ollama-buddy-completion-mode-map}"
  :lighter " ⊡"
  :keymap ollama-buddy-completion-mode-map
  (if ollama-buddy-completion-mode
      (add-hook 'pre-command-hook #'ollama-buddy-completion--pre-command nil t)
    (remove-hook 'pre-command-hook #'ollama-buddy-completion--pre-command t)
    (ollama-buddy-completion--dismiss)))

;;;###autoload
(defun ollama-buddy-completion-toggle ()
  "Toggle `ollama-buddy-completion-mode' in the current buffer."
  (interactive)
  (ollama-buddy-completion-mode (if ollama-buddy-completion-mode -1 1))
  (message "Completion mode %s"
           (if ollama-buddy-completion-mode "enabled" "disabled")))

(provide 'ollama-buddy-completion)
;;; ollama-buddy-completion.el ends here
