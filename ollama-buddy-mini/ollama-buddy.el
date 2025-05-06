;;; ollama-buddy.el --- A mini version of ollama-buddy

;;; Commentary:
;; 

(require 'json)
(require 'subr-x)
(require 'url)
(require 'cl-lib)

;;; Code:

(defgroup ollama-buddy nil "Customization group for Ollama Buddy." :group 'applications :prefix "ollama-buddy-")
(defcustom ollama-buddy-host "localhost" "Host where Ollama server is running." :type 'string :group 'ollama-buddy)
(defcustom ollama-buddy-port 11434 "Port where Ollama server is running." :type 'integer :group 'ollama-buddy)
(defcustom ollama-buddy-default-model nil "Default Ollama model to use." :type 'string :group 'ollama-buddy)
(defvar ollama-buddy--conversation-history nil "History of messages for the current conversation.")
(defvar ollama-buddy--current-model nil "Timer for checking Ollama connection status.")
(defvar ollama-buddy--chat-buffer "*Ollama Buddy Chat*" "Chat interaction buffer.")
(defvar ollama-buddy--active-process nil "Active Ollama process.")
(defvar ollama-buddy--prompt-history nil "History of prompts used in ollama-buddy.")

(defun ollama-buddy--add-to-history (role content)
  "Add message with ROLE and CONTENT to conversation history."
  (push `((role . ,role)(content . ,content)) ollama-buddy--conversation-history))

(defun ollama-buddy-clear-history ()
  "Clear the current conversation history."
  (interactive)
  (setq ollama-buddy--conversation-history nil)
  (ollama-buddy--update-status "History cleared"))

(defun ollama-buddy--update-status (status &optional model)
  "Update the status with STATUS text and MODEL in the header-line."
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (let* ((model (or ollama-buddy--current-model ollama-buddy-default-model "No Model")))
      (setq header-line-format
            (concat (propertize (format " %s : %s" model status) 'face `(:weight bold)))))))

(defun ollama-buddy--stream-filter (_proc output)
  "Process stream OUTPUT while preserving cursor position."
  (when-let* ((json-str (replace-regexp-in-string "^[^\{]*" "" output))
              (json-data (and (> (length json-str) 0) (json-read-from-string json-str)))
              (text (alist-get 'content (alist-get 'message json-data))))
    (with-current-buffer ollama-buddy--chat-buffer
      (let* ((inhibit-read-only t)
             (window (get-buffer-window ollama-buddy--chat-buffer t))
             (old-point (and window (window-point window)))
             (at-end (and window (>= old-point (point-max))))
             (old-window-start (and window (window-start window))))
        (save-excursion
          (ollama-buddy--update-status "Processing...")
          (goto-char (point-max))
          (insert text)
          (when (boundp 'ollama-buddy--current-response)
            (setq ollama-buddy--current-response
                  (concat (or ollama-buddy--current-response "") text)))
          (unless (boundp 'ollama-buddy--current-response)
            (setq ollama-buddy--current-response text))
          (when (eq (alist-get 'done json-data) t)
            (ollama-buddy--add-to-history "assistant" ollama-buddy--current-response)
            (makunbound 'ollama-buddy--current-response)
            (insert "\n\n" (propertize (concat "[" ollama-buddy--current-model ": FINISHED]")
                                       'face '(:inherit bold)))
            (ollama-buddy--show-prompt)
            (ollama-buddy--update-status "Finished")))
        (when window
          (if at-end
              (set-window-point window (point-max))
            (set-window-point window old-point))
          (set-window-start window old-window-start t))))))

(defun ollama-buddy--stream-sentinel (_proc event)
  "Handle stream completion EVENT."
  (when-let* ((status (cond ((string-match-p "finished" event) "Completed")
                            ((string-match-p "\\(?:deleted\\|connection broken\\)" event)
                             "Interrupted"))))
    (with-current-buffer ollama-buddy--chat-buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "\n\n[Stream %s]" status) 'face '(:weight bold)))
        (ollama-buddy--show-prompt)))
    (ollama-buddy--update-status (concat "Stream " status))))

(defun ollama-buddy--swap-model ()
  "Swap ollama model."
  (interactive)
  (let ((new-model (completing-read "Model: " (ollama-buddy--get-models) nil t)))
    (setq ollama-buddy-default-model new-model ollama-buddy--current-model new-model)
    (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
    (ollama-buddy--show-prompt)
    (goto-char (point-max))
    (ollama-buddy--update-status "Idle")))

(defun ollama-buddy-menu ()
  "Open chat buffer and initialize if needed."
  (interactive)
  (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (when (= (buffer-size) 0)
      (ollama-buddy-mode 1)
      (insert "Send   : C-c C-c\nCancel : C-c C-k\nModel  : C-c m\n\n")
      (insert (mapconcat 'identity (ollama-buddy--get-models) "\n"))
      (ollama-buddy--show-prompt))
    (ollama-buddy--update-status "Idle"))
  (goto-char (point-max)))

(defun ollama-buddy--show-prompt ()
  "Show the prompt with optionally a MODEL."
  (interactive)
  (when (not ollama-buddy-default-model)
    ;; just get the first model
    (let ((model (car (ollama-buddy--get-models))))
      (setq ollama-buddy--current-model model)
      (setq ollama-buddy-default-model model)
      (insert (format "\n\n* NO DEFAULT MODEL : Using best guess : %s" model))))
  (let* ((model (or ollama-buddy--current-model ollama-buddy-default-model "Default:latest")))
    (insert (format "\n\n%s\n\n%s %s"
                    (propertize "------------------" 'face '(:inherit bold))
                    (propertize model 'face `(:weight bold))
                    (propertize ">> PROMPT: " 'face '(:inherit bold))))))

(defun ollama-buddy--send (&optional prompt model)
  "Send PROMPT with optional MODEL"
  (unless (> (length prompt) 0)
    (user-error "Ensure prompt is defined"))
  (let* ((messages (reverse ollama-buddy--conversation-history))
         (messages (append messages `(((role . "user")
                                       (content . ,prompt)))))
         (payload (json-encode
                   `((model . ,model)
                     (messages . ,(vconcat [] messages))
                     (stream . t)))))
    (setq ollama-buddy--current-model model)
    (ollama-buddy--add-to-history "user" prompt)
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (pop-to-buffer (current-buffer))
      (goto-char (point-max))
      (insert (format "\n\n%s\n\n%s %s\n\n%s\n\n"
                      (propertize "------------------" 'face '(:inherit bold))
                      (propertize "[User: PROMPT]" 'face '(:inherit bold))
                      prompt
                      (propertize (concat "[" model ": RESPONSE]") 'face `(:inherit bold))))
      (visual-line-mode 1))
    (ollama-buddy--update-status "Sending request..." model)
    (when (and ollama-buddy--active-process
               (process-live-p ollama-buddy--active-process))
      (set-process-sentinel ollama-buddy--active-process nil)
      (delete-process ollama-buddy--active-process)
      (setq ollama-buddy--active-process nil))
    (condition-case err
        (setq ollama-buddy--active-process
              (make-network-process
               :name "ollama-chat-stream"
               :buffer nil
               :host ollama-buddy-host
               :service ollama-buddy-port
               :coding 'utf-8
               :filter #'ollama-buddy--stream-filter
               :sentinel #'ollama-buddy--stream-sentinel))
      (error
       (ollama-buddy--update-status "OFFLINE - Connection failed")
       (error "Failed to connect to Ollama: %s" (error-message-string err))))
    (condition-case err
        (process-send-string
         ollama-buddy--active-process
         (concat "POST /api/chat HTTP/1.1\r\n"
                 (format "Host: %s:%d\r\n" ollama-buddy-host ollama-buddy-port)
                 "Content-Type: application/json\r\n"
                 (format "Content-Length: %d\r\n\r\n" (string-bytes payload))
                 payload))
      (error
       (ollama-buddy--update-status "OFFLINE - Send failed")
       (when (and ollama-buddy--active-process
                  (process-live-p ollama-buddy--active-process))
         (delete-process ollama-buddy--active-process))
       (error "Failed to send request to Ollama: %s" (error-message-string err))))))

(defun ollama-buddy--make-request (endpoint method &optional payload)
  "Generic request function for ENDPOINT with METHOD and optional PAYLOAD."
  (let* ((url (format "http://%s:%d%s" ollama-buddy-host ollama-buddy-port endpoint))
         (url-request-method method)
         (url-request-extra-headers '(("Content-Type" . "application/json")
                                      ("Connection" . "close")))
         (url-request-data (when payload
                             (encode-coding-string payload 'utf-8))))
    (with-temp-buffer
      (url-insert-file-contents url)
      (json-read-from-string (buffer-string)))))

(defun ollama-buddy--get-models ()
  "Get available Ollama models."
  (when-let ((response (ollama-buddy--make-request "/api/tags" "GET")))
    (mapcar (lambda (m) (alist-get 'name m))(alist-get 'models response))))

(defun ollama-buddy--send-prompt ()
  "Send the current prompt to a LLM.."
  (interactive)
  (let* ((bounds (save-excursion
                   (search-backward ">> PROMPT:")
                   (search-forward ":")
                   (point)))
         (model (or ollama-buddy--current-model ollama-buddy-default-model "Default:latest"))
         (query-text (string-trim (buffer-substring-no-properties bounds (point)))))
    (when (and query-text (not (string-empty-p query-text)))
      (add-to-history 'ollama-buddy--prompt-history query-text))
    (ollama-buddy--send query-text model)))

(defun ollama-buddy--cancel-request ()
  "Cancel the current request and clean up resources."
  (interactive)
  (when ollama-buddy--active-process
    (delete-process ollama-buddy--active-process)
    (setq ollama-buddy--active-process nil))
  (ollama-buddy--update-status "Cancelled"))

(defvar ollama-buddy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ollama-buddy--send-prompt)
    (define-key map (kbd "C-c C-k") #'ollama-buddy--cancel-request)
    (define-key map (kbd "C-c m") #'ollama-buddy--swap-model)
    map)
  "Keymap for ollama-buddy mode.")

(define-minor-mode ollama-buddy-mode
  "Minor mode for ollama-buddy keybindings."
  :lighter " OB" :keymap ollama-buddy-mode-map)

(provide 'ollama-buddy)

;;; ollama-buddy.el ends here
