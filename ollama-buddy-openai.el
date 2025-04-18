;;; ollama-buddy-openai.el --- OpenAI integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
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

;; Internal variables

(defvar ollama-buddy-openai--current-token-count 0
  "Counter for tokens in the current OpenAI response.")

;; Helper functions

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
    (setq ollama-buddy--current-model
          (or model
              ollama-buddy--current-model
              (ollama-buddy-openai--get-full-model-name
               ollama-buddy-openai-default-model)))

    ;; Initialize token counter
    (setq ollama-buddy-openai--current-token-count 0)

    ;; Get history and system prompt
    (let* ((model ollama-buddy--current-model)
           (history (when ollama-buddy-history-enabled
                      (gethash ollama-buddy--current-model
                               ollama-buddy--conversation-history-by-model
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
                         ollama-buddy--current-model))
              (messages . ,messages)
              (temperature . ,ollama-buddy-openai-temperature)
              (max_tokens . ,max-tokens)))
           (json-str (let ((json-encoding-pretty-print nil))
                       (ollama-buddy-escape-unicode (json-encode json-payload)))))

      ;; Prepare chat buffer
      (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
        (pop-to-buffer (current-buffer))
        (goto-char (point-max))

        (unless (> (buffer-size) 0)
          (insert (ollama-buddy--create-intro-message)))
        
        (let (start-point
              (inhibit-read-only t))
          
          (insert (propertize (format "\n\n** [%s: RESPONSE]" model) 'face
                              `(:inherit bold :foreground ,(ollama-buddy--get-model-color model))) "\n\n")
          
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
                                   (when ollama-buddy-history-enabled
                                     (ollama-buddy--add-to-history "user" prompt)
                                     (ollama-buddy--add-to-history "assistant" content))
                                   
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

(defun ollama-buddy-openai--fetch-models ()
  "Fetch available models from OpenAI API."
  (when (ollama-buddy-openai--verify-api-key)
    (ollama-buddy--update-status "Fetching OpenAI models...")
    (let* ((url-request-method "GET")
           (url-request-extra-headers
            `(("Authorization" . ,(concat "Bearer " ollama-buddy-openai-api-key)))))
      
      (url-retrieve
       "https://api.openai.com/v1/models"
       (lambda (status)
         (if (plist-get status :error)
             (progn
               (message "Error fetching OpenAI models: %s" (prin1-to-string (plist-get status :error)))
               (ollama-buddy--update-status "Failed to fetch OpenAI models"))
           
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
                            (chat-models (cl-remove-if-not
                                          (lambda (model)
                                            (string-match-p "\\(gpt\\|claude\\)" model))
                                          models)))
                       
                       ;; Store models and update status
                       (setq ollama-buddy-openai-models chat-models))
                       ;; (ollama-buddy--update-status (format "Fetched %d OpenAI models" (length chat-models)))
                   (error
                    (message "Error parsing OpenAI models response: %s" (error-message-string err))
                    (ollama-buddy--update-status "Failed to parse OpenAI models response"))))))))))))

(ollama-buddy-openai--fetch-models)

(provide 'ollama-buddy-openai)
;;; ollama-buddy-openai.el ends here
