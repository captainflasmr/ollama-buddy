;;; ollama-buddy-openai.el --- OpenAI integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; Package-Requires: ((emacs "28.1") (url "1.2"))

;;; Commentary:
;; This extension provides OpenAI (ChatGPT) integration for the ollama-buddy package.
;; It allows users to interact with OpenAI's language models using the same interface
;; as ollama-buddy, providing seamless switching between local Ollama models and
;; cloud-based OpenAI models.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'ollama-buddy-core)

(defgroup ollama-buddy-openai nil
  "OpenAI integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-openai-")

(defcustom ollama-buddy-openai-api-key ""
  "API key for accessing OpenAI services.
Get your key from https://platform.openai.com/api-keys."
  :type 'string
  :group 'ollama-buddy-openai)

(defcustom ollama-buddy-openai-default-model "gpt-3.5-turbo"
  "Default OpenAI model to use."
  :type 'string
  :group 'ollama-buddy-openai)

(defcustom ollama-buddy-openai-api-endpoint "https://api.openai.com/v1/chat/completions"
  "Endpoint for OpenAI chat completions API."
  :type 'string
  :group 'ollama-buddy-openai)

(defcustom ollama-buddy-openai-temperature 0.7
  "Temperature setting for OpenAI requests (0.0-2.0).
Lower values make the output more deterministic, higher values more creative."
  :type 'float
  :group 'ollama-buddy-openai)

(defcustom ollama-buddy-openai-max-tokens nil
  "Maximum number of tokens to generate in the response.
Use nil for API default behavior (adaptive)."
  :type '(choice integer (const nil))
  :group 'ollama-buddy-openai)

(defcustom ollama-buddy-openai-show-loading t
  "Whether to show a loading indicator during API requests."
  :type 'boolean
  :group 'ollama-buddy-openai)

;; Internal variables
(defvar ollama-buddy-openai--model-colors (make-hash-table :test 'equal)
  "Hash table for storing OpenAI model colors.")

(defvar ollama-buddy-openai--conversation-history-by-model (make-hash-table :test 'equal)
  "Hash table mapping model names to their conversation histories.")

(defvar ollama-buddy-openai--current-response nil
  "Accumulates the current response content.")

(defvar ollama-buddy-openai--current-prompt nil
  "The current prompt sent to OpenAI.")

(defvar ollama-buddy-openai--current-token-count 0
  "Counter for tokens in the current OpenAI response.")

;; Model display and management functions

(defun ollama-buddy-openai--get-model-color (model)
  "Get color for OpenAI MODEL."
  (or (gethash model ollama-buddy-openai--model-colors)
      (let ((color (ollama-buddy--hash-string-to-color model)))
        (puthash model color ollama-buddy-openai--model-colors)
        color)))

(defun ollama-buddy-openai--get-real-model-name (model)
  "Extract the actual model name from the prefixed MODEL string."
  (if (ollama-buddy-openai--is-openai-model model)
      (string-trim (substring model (length ollama-buddy-openai-marker-prefix)))
    model))

;; API interaction functions

(defun ollama-buddy-openai--verify-api-key ()
  "Verify that the API key is set."
  (if (string-empty-p ollama-buddy-openai-api-key)
      (progn
        (customize-variable 'ollama-buddy-openai-api-key)
        (error "Please set your OpenAI API key"))
    t))

(defun ollama-buddy-openai--send (prompt &optional model)
  "Send PROMPT to OpenAI's API using MODEL or default model asynchronously."
  (when (ollama-buddy-openai--verify-api-key)
    ;; Set up the current model
    (setq ollama-buddy-openai--current-model
          (or model
              ollama-buddy-openai--current-model
              (ollama-buddy-openai--get-full-model-name
               ollama-buddy-openai-default-model)))

    ;; Store the prompt and initialize response
    (setq ollama-buddy-openai--current-prompt prompt
          ollama-buddy-openai--current-response "")

    ;; Initialize token counter
    (setq ollama-buddy-openai--current-token-count 0)

    ;; Get history and system prompt
    (let* ((history (when ollama-buddy-history-enabled
                      (gethash ollama-buddy-openai--current-model
                               ollama-buddy-openai--conversation-history-by-model
                               nil)))
           (system-prompt ollama-buddy--current-system-prompt)
           (messages (vconcat []
                              (append
                               (when (and system-prompt (not (string-empty-p system-prompt)))
                                 `(((role . "system") (content . ,system-prompt))))
                               history
                               `(((role . "user") (content . ,prompt))))))
           (max-tokens (or ollama-buddy-openai-max-tokens 4096))
           (json-payload
            `((model . ,(ollama-buddy-openai--get-real-model-name
                         ollama-buddy-openai--current-model))
              (messages . ,messages)
              (temperature . ,ollama-buddy-openai-temperature)
              (max_tokens . ,max-tokens)))
           ;; Create the JSON string
           (json-str (let ((json-encoding-pretty-print nil))
                       (ollama-buddy-escape-unicode (json-encode json-payload)))))

      ;; Prepare chat buffer
      (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
        (pop-to-buffer (current-buffer))
        (goto-char (point-max))
        (let (start-point
              (inhibit-read-only t)
              (display-name ollama-buddy-openai--current-model))

          (insert (propertize (format "\n\n** [%s: RESPONSE]" display-name)
                              'face `(:inherit bold :foreground
                                               ,(ollama-buddy-openai--get-model-color display-name)))
                  "\n\n")
          
          (setq start-point (point))
          
          (insert "Loading response...")
          (ollama-buddy--update-status "Sending request to OpenAI...")

          (set-register ollama-buddy-default-register "")
          
          (let* ((url-request-method "POST")
                 (url-request-extra-headers
                  `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " ollama-buddy-openai-api-key))))
                 ;; Set JSON as raw data - let url library handle encoding
                 (url-request-data json-str))
            
            ;; Setting this makes url.el use binary for the request
            (let ((url-mime-charset-string "utf-8")
                  (url-mime-language-string nil)
                  (url-mime-encoding-string nil)
                  (url-mime-accept-string "application/json"))
              
              (url-retrieve
               ollama-buddy-openai-api-endpoint
               (lambda (status)
                 (if (plist-get status :error)
                     (progn
                       (with-current-buffer ollama-buddy--chat-buffer
                         (let ((inhibit-read-only t))
                           (goto-char start-point)
                           (delete-region start-point (point-max))
                           (insert "Error: URL retrieval failed\n")
                           (insert "Details: " (prin1-to-string (plist-get status :error)) "\n")
                           (insert "\n\n*** FAILED")
                           (ollama-buddy--prepare-prompt-area)
                           (ollama-buddy--update-status "Failed - URL retrieval error"))))
                   
                   ;; Success - process the response
                   (progn
                     (goto-char (point-min))
                     (when (re-search-forward "\n\n" nil t)
                       (let* ((json-response-raw (buffer-substring (point) (point-max)))
                              (json-object-type 'alist)
                              (json-array-type 'vector)
                              (json-key-type 'symbol))
                         
                         (condition-case err
                             (let* ((json-response (json-read-from-string json-response-raw))
                                    (error-message (alist-get 'error json-response))
                                    (content "")
                                    (choices (alist-get 'choices json-response)))
                               
                               ;; Extract the message content
                               (if error-message
                                   (setq content (format "Error: %s" (alist-get 'message error-message)))
                                 (when choices
                                   (setq content (ollama-buddy-fix-encoding-issues
                                                  (alist-get 'content (alist-get 'message (aref choices 0)))))))
                               
                               ;; Update the chat buffer
                               (with-current-buffer ollama-buddy--chat-buffer
                                 (let ((inhibit-read-only t))
                                   (goto-char start-point)
                                   (delete-region start-point (point-max))
                                   
                                   ;; Insert the content
                                   (insert content)
                                   
                                   ;; Convert markdown to org if enabled
                                   (when ollama-buddy-convert-markdown-to-org
                                     (ollama-buddy--md-to-org-convert-region start-point (point-max)))
                                   
                                   ;; Write to register
                                   (let* ((reg-char (if ollama-buddy--multishot-sequence
                                                        (if (< ollama-buddy--multishot-progress (length ollama-buddy--multishot-sequence))
                                                            (aref ollama-buddy--multishot-sequence ollama-buddy--multishot-progress)
                                                          ollama-buddy-default-register)
                                                      ollama-buddy-default-register))
                                          (current (get-register reg-char))
                                          (new-content (concat (if (stringp current) current "") content)))
                                     (set-register reg-char new-content))
                                   
                                   ;; Add to history
                                   (setq ollama-buddy-openai--current-response content)
                                   (when ollama-buddy-history-enabled
                                     (ollama-buddy-openai--add-to-history "user" prompt)
                                     (ollama-buddy-openai--add-to-history "assistant" content))
                                   
                                   ;; Calculate token count
                                   (setq ollama-buddy-openai--current-token-count
                                         (length (split-string content "\\b" t)))
                                   
                                   ;; Show token stats if enabled
                                   (when ollama-buddy-display-token-stats
                                     (insert (format "\n\n*** Token Stats\n[%d tokens]"
                                                     ollama-buddy-openai--current-token-count)))
                                   
                                   (insert "\n\n*** FINISHED")
                                   (ollama-buddy--prepare-prompt-area)
                                   (ollama-buddy--update-status
                                    (format "Finished [%d tokens]"
                                            ollama-buddy-openai--current-token-count)))))
                           (error
                            (with-current-buffer ollama-buddy--chat-buffer
                              (let ((inhibit-read-only t))
                                (goto-char start-point)
                                (delete-region start-point (point-max))
                                (insert "Error: Failed to parse OpenAI response\n")
                                (insert "Details: " (error-message-string err) "\n")
                                (insert "\n\n*** FAILED")
                                (ollama-buddy--prepare-prompt-area)
                                (ollama-buddy--update-status "Failed - JSON parse error"))))))))))))))))))

;; History management functions

(defun ollama-buddy-openai--add-to-history (role content)
  "Add message with ROLE and CONTENT to OpenAI conversation history."
  (when ollama-buddy-history-enabled
    (let* ((model ollama-buddy-openai--current-model)
           (history (gethash model ollama-buddy-openai--conversation-history-by-model nil)))
      
      ;; Create new history entry for this model if it doesn't exist
      (unless history
        (setq history nil))
      
      ;; Add the new message to this model's history
      (setq history
            (append history
                    (list `((role . ,role)
                            (content . ,content)))))
      
      ;; Truncate history if needed
      (when (and (boundp 'ollama-buddy-max-history-length)
                 (> (length history) (* 2 ollama-buddy-max-history-length)))
        (setq history (seq-take history (* 2 ollama-buddy-max-history-length))))
      
      ;; Update the hash table with the modified history
      (puthash model history ollama-buddy-openai--conversation-history-by-model))))

(defun ollama-buddy-openai-select-model ()
  "Select an OpenAI model to use."
  (interactive)
  (let* ((models (mapcar (lambda (m) (ollama-buddy-openai--get-full-model-name m))
                         ollama-buddy-openai-models))
         (selected (completing-read "Select OpenAI model: " models nil t)))
    (setq ollama-buddy-openai--current-model selected)
    (setq ollama-buddy--current-model selected) ; Share with main package
    
    ;; Update the chat buffer
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (pop-to-buffer (current-buffer))
      (ollama-buddy--prepare-prompt-area)
      (goto-char (point-max)))
    
    (message "Selected OpenAI model: %s"
             (ollama-buddy-openai--get-real-model-name selected))))

(defun ollama-buddy-openai-clear-history ()
  "Clear the conversation history for the current OpenAI model."
  (interactive)
  (let ((model ollama-buddy-openai--current-model))
    (remhash model ollama-buddy-openai--conversation-history-by-model)
    (message "OpenAI conversation history cleared for %s" model)))

(defun ollama-buddy-openai-display-history ()
  "Display the conversation history for the current OpenAI model."
  (interactive)
  (let* ((model ollama-buddy-openai--current-model)
         (history (gethash model ollama-buddy-openai--conversation-history-by-model nil))
         (buf (get-buffer-create "*OpenAI Conversation History*")))
    
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "OpenAI Conversation History for %s:\n\n" model))
        
        (if (null history)
            (insert "No conversation history available.")
          (let ((history-count (/ (length history) 2)))
            (insert (format "Current history: %d message pairs\n\n" history-count))
            
            ;; Display the history in chronological order
            (dolist (msg history)
              (let* ((role (alist-get 'role msg))
                     (content (alist-get 'content msg))
                     (role-face (if (string= role "user")
                                    '(:inherit bold)
                                  '(:inherit bold))))
                (insert (propertize (format "[%s]: " (upcase role)) 'face role-face))
                (insert (format "%s\n\n" content))))))
        
        (view-mode 1)))
    
    (display-buffer buf)))

(defun ollama-buddy-openai-configure ()
  "Configure OpenAI integration settings."
  (interactive)
  (customize-group 'ollama-buddy-openai))

;; Integration with the main package

(defun ollama-buddy-openai-initialize ()
  "Initialize OpenAI integration for ollama-buddy."
  (interactive)
  
  ;; Add OpenAI models to the available models list for completion
  (dolist (model ollama-buddy-openai-models)
    (let ((full-name (ollama-buddy-openai--get-full-model-name model)))
      ;; Generate and store a color for this model
      (puthash full-name (ollama-buddy--hash-string-to-color full-name)
               ollama-buddy-openai--model-colors)))
  
  ;; Set up the key for API authentication if not set
  (when (string-empty-p ollama-buddy-openai-api-key)
    (message "OpenAI API key not set. Use M-x ollama-buddy-openai-configure to set it"))
  
  (message "OpenAI integration initialized. Use M-x ollama-buddy-openai-select-model to choose a model"))

;; User commands to expose

;;;###autoload
(defun ollama-buddy-openai-setup ()
  "Setup the OpenAI integration."
  (interactive)
  (ollama-buddy-openai-initialize)
  (ollama-buddy-openai-configure))

(provide 'ollama-buddy-openai)
;;; ollama-buddy-openai.el ends here
