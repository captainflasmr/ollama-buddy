;;; ollama-buddy-claude.el --- Anthropic Claude integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This extension provides Anthropic Claude integration for the ollama-buddy package.
;; It allows users to interact with Anthropic's Claude language models using the same interface
;; as ollama-buddy, providing seamless switching between local Ollama models and
;; cloud-based Claude models.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'ollama-buddy-core)

(defgroup ollama-buddy-claude nil
  "Anthropic Claude integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-claude-")

(defcustom ollama-buddy-claude-marker-prefix "c:"
  "Prefix used to identify Claude models in the model list."
  :type 'string
  :group 'ollama-buddy-claude)

(defcustom ollama-buddy-claude-api-key ""
  "API key for accessing Anthropic Claude services.
Get your key from https://console.anthropic.com/."
  :type 'string
  :group 'ollama-buddy-claude)

(defcustom ollama-buddy-claude-default-model "claude-3-7-sonnet-20250219"
  "Default Claude model to use."
  :type 'string
  :group 'ollama-buddy-claude)

(defcustom ollama-buddy-claude-api-endpoint "https://api.anthropic.com/v1/messages"
  "Endpoint for Anthropic Claude API."
  :type 'string
  :group 'ollama-buddy-claude)

(defcustom ollama-buddy-claude-temperature 0.7
  "Temperature setting for Claude requests (0.0-1.0).
Lower values make the output more deterministic, higher values more creative."
  :type 'float
  :group 'ollama-buddy-claude)

(defcustom ollama-buddy-claude-max-tokens nil
  "Maximum number of tokens to generate in the response.
Use nil for API default behavior (adaptive)."
  :type '(choice integer (const nil))
  :group 'ollama-buddy-claude)

;; Internal variables

(defvar ollama-buddy-claude--current-token-count 0
  "Counter for tokens in the current Claude response.")

(defvar ollama-buddy-remote-models nil
  "List of available remote models.")

;; Helper functions

(defun ollama-buddy-claude--is-claude-model (model)
  "Check if MODEL is a Claude model by checking for the prefix."
  (and model (string-prefix-p ollama-buddy-claude-marker-prefix model)))

(defun ollama-buddy-claude--get-full-model-name (model)
  "Get the full model name with prefix for MODEL."
  (concat ollama-buddy-claude-marker-prefix model))

(defun ollama-buddy-claude--get-real-model-name (model)
  "Extract the actual model name from the prefixed MODEL string."
  (if (ollama-buddy-claude--is-claude-model model)
      (string-trim (substring model (length ollama-buddy-claude-marker-prefix)))
    model))

;; API interaction functions

(defun ollama-buddy-claude--verify-api-key ()
  "Verify that the API key is set."
  (if (string-empty-p ollama-buddy-claude-api-key)
      (progn
        (customize-variable 'ollama-buddy-claude-api-key)
        (error "Please set your Anthropic Claude API key"))
    t))

(defun ollama-buddy-claude--send (prompt &optional model)
  "Send PROMPT to Claude's API using MODEL."
  (when (ollama-buddy-claude--verify-api-key)
    ;; Set up the current model
    (setq ollama-buddy--current-model
          (or model
              ollama-buddy--current-model
              (ollama-buddy-claude--get-full-model-name
               ollama-buddy-claude-default-model)))

    ;; Initialize token counter
    (setq ollama-buddy-claude--current-token-count 0)

    ;; Get history and system prompt
    (let* ((history (when ollama-buddy-history-enabled
                      (gethash ollama-buddy--current-model
                               ollama-buddy--conversation-history-by-model
                               nil)))
           (system-prompt (ollama-buddy--effective-system-prompt))
           (attachment-context
            (when ollama-buddy--current-attachments
              (concat "\n\n## Attached Files Context:\n\n"
                      (mapconcat
                       (lambda (attachment)
                         (let ((file (plist-get attachment :file))
                               (content (plist-get attachment :content)))
                           (format "### File: %s\n\n#+end_src%s\n%s\n#+begin_src \n\n"
                                   (file-name-nondirectory file)
                                   (or (plist-get attachment :type) "")
                                   content)))
                       ollama-buddy--current-attachments
                       ""))))
           (messages (vconcat []
                              (append
                               ;; Don't include system prompt in messages array
                               history
                               `(((role . "user")
                                  (content . ,(if attachment-context
                                                  (concat prompt attachment-context)
                                                prompt)))))))
           (max-tokens (or ollama-buddy-claude-max-tokens 4096))
           (json-payload
            `((model . ,(ollama-buddy-claude--get-real-model-name
                         ollama-buddy--current-model))
              (messages . ,messages)
              (temperature . ,ollama-buddy-claude-temperature)
              (max_tokens . ,max-tokens)))
           (json-payload (if (and system-prompt (not (string-empty-p system-prompt)))
                             (append json-payload `((system . ,system-prompt)))
                           json-payload))
           (json-str (let ((json-encoding-pretty-print nil))
                       (ollama-buddy-escape-unicode (json-encode json-payload))))
           (endpoint ollama-buddy-claude-api-endpoint))

      ;; Prepare chat buffer
      (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
        (pop-to-buffer (current-buffer))
        (goto-char (point-max))

        (unless (> (buffer-size) 0)
          (insert (ollama-buddy--create-intro-message)))

        ;; Show any attached files
        (when ollama-buddy--current-attachments
          (insert (format "\n\n[Including %d attached file(s) in context]"
                          (length ollama-buddy--current-attachments))))
        
        (let (start-point
              (inhibit-read-only t))

          (insert (format "\n\n** [%s: RESPONSE]\n\n" ollama-buddy--current-model))
          
          (setq start-point (point))
          
          (insert "Loading response...")
          (ollama-buddy--update-status "Sending request to Claude...")

          (set-register ollama-buddy-default-register "")
          
          (let* ((url-request-method "POST")
                 (url-request-extra-headers
                  `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " ollama-buddy-claude-api-key))
                    ("X-API-Key" . ,ollama-buddy-claude-api-key)
                    ("anthropic-version" . "2023-06-01")))
                 (url-request-data json-str))
            
            ;; Setting this makes url.el use binary for the request
            (let ((url-mime-charset-string "utf-8")
                  (url-mime-language-string nil)
                  (url-mime-encoding-string nil)
                  (url-mime-accept-string "application/json"))
              
              (url-retrieve
               endpoint
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
                              (json-response-decoded (decode-coding-string json-response-raw 'utf-8))
                              (json-object-type 'alist)
                              (json-array-type 'vector)
                              (json-key-type 'symbol))
                         
                         (condition-case err
                             (let* ((response (json-read-from-string json-response-decoded))
                                    (error-message (alist-get 'error response))
                                    (content ""))

                               ;; Extract the message
                               (if error-message
                                   (setq content (format "Error: %s" (alist-get 'message error-message)))
                                 ;; Extract the message
                                 (let ((extracted-text ""))
                                   ;; Get the content array from the response
                                   (let ((content-obj (alist-get 'content response)))
                                     (if (listp content-obj)
                                         ;; Handle new API format where content is an object containing an array
                                         (let ((content-array (alist-get 'content content-obj)))
                                           (if (vectorp content-array)
                                               ;; Process each content item in the array
                                               (dotimes (i (length content-array))
                                                 (let* ((item (aref content-array i))
                                                        (item-type (alist-get 'type item))
                                                        (item-text (alist-get 'text item)))
                                                   (when (and (string= item-type "text") item-text)
                                                     (setq extracted-text (concat extracted-text item-text)))))
                                             ;; Handle case where content.content is not a vector
                                             (message "Unexpected content format: %S" content-array)))
                                       ;; Handle case where content is already the array
                                       (if (vectorp content-obj)
                                           (dotimes (i (length content-obj))
                                             (let* ((item (aref content-obj i))
                                                    (item-type (alist-get 'type item))
                                                    (item-text (alist-get 'text item)))
                                               (when (and (string= item-type "text") item-text)
                                                 (setq extracted-text (concat extracted-text item-text)))))
                                         (message "Unexpected response format: %S" content-obj))))
                                   (setq content extracted-text)))
                               
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
                                   (let* ((reg-char ollama-buddy-default-register)
                                          (current (get-register reg-char))
                                          (new-content (concat (if (stringp current) current "") content)))
                                     (set-register reg-char new-content))
                                   
                                   ;; Add to history
                                   (when ollama-buddy-history-enabled
                                     (ollama-buddy--add-to-history "user" prompt)
                                     (ollama-buddy--add-to-history "assistant" content))
                                   
                                   ;; Calculate token count
                                   (setq ollama-buddy-claude--current-token-count
                                         (length (split-string content "\\b" t)))
                                   
                                   ;; Show token stats if enabled
                                   (when ollama-buddy-display-token-stats
                                     (insert (format "\n\n*** Token Stats\n[%d tokens]"
                                                     ollama-buddy-claude--current-token-count)))
                                   
                                   (insert "\n\n*** FINISHED")
                                   (ollama-buddy--prepare-prompt-area)
                                   (ollama-buddy--update-status
                                    (format "Finished [%d tokens]"
                                            ollama-buddy-claude--current-token-count)))))
                           (error
                            (with-current-buffer ollama-buddy--chat-buffer
                              (let ((inhibit-read-only t))
                                (goto-char start-point)
                                (delete-region start-point (point-max))
                                (insert "Error: Failed to parse Claude response\n")
                                (insert "Details: " (error-message-string err) "\n")
                                (insert "\n\n*** FAILED")
                                (ollama-buddy--prepare-prompt-area)
                                (ollama-buddy--update-status "Failed - JSON parse error"))))))))))))))))))

(defun ollama-buddy-claude--fetch-models ()
  "Fetch available models from Anthropic's Claude API."
  (when (ollama-buddy-claude--verify-api-key)
    (ollama-buddy--update-status "Fetching Claude models...")
    (let* ((url-request-method "GET")
           (url-request-extra-headers
            `(("x-api-key" . ,ollama-buddy-claude-api-key)
              ("anthropic-version" . "2023-06-01"))))
      
      (url-retrieve
       "https://api.anthropic.com/v1/models"
       (lambda (status)
         (if (plist-get status :error)
             (progn
               (message "Error fetching Claude models: %s" (prin1-to-string (plist-get status :error)))
               (ollama-buddy--update-status "Failed to fetch Claude models"))
           
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
                            (models-data (alist-get 'data json-response))
                            (models (mapcar (lambda (model-info)
                                              (alist-get 'id model-info))
                                            (append models-data nil)))
                            ;; Filter to only include Claude models (should be all of them)
                            (chat-models (cl-remove-if-not
                                          (lambda (model)
                                            (string-match-p "claude" model))
                                          models))
                            ;; Prepend the marker prefix to each model name
                            (prefixed-models (mapcar (lambda (model-name)
                                                       (concat ollama-buddy-claude-marker-prefix model-name))
                                                     chat-models)))
                       ;; Register the Claude handler with ollama-buddy
                       (when (fboundp 'ollama-buddy-register-model-handler)
                         (ollama-buddy-register-model-handler 
                          ollama-buddy-claude-marker-prefix 
                          #'ollama-buddy-claude--send))
                       ;; Store models and update status
                       (setq ollama-buddy-remote-models (append ollama-buddy-remote-models prefixed-models)))
                   (error
                    (message "Error parsing Claude models response: %s" (error-message-string err))
                    (ollama-buddy--update-status "Failed to parse Claude models response"))))))))))))

(ollama-buddy-claude--fetch-models)

(provide 'ollama-buddy-claude)
;;; ollama-buddy-claude.el ends here
