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

(defcustom ollama-buddy-openai-max-tokens 800
  "Maximum number of tokens to generate in the response.
Use nil for API default behavior (adaptive)."
  :type '(choice integer (const nil))
  :group 'ollama-buddy-openai)

(defcustom ollama-buddy-openai-marker-prefix "GPT"
  "Prefix to indicate that a model is from OpenAI rather than Ollama."
  :type 'string
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

(defun ollama-buddy-openai--is-openai-model (model)
  "Check if MODEL is an OpenAI model based on prefix."
  (and model
       (string-match-p (concat "^" (regexp-quote ollama-buddy-openai-marker-prefix)) model)))

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
  "Send PROMPT to OpenAI's API using MODEL or default model.
This uses proper encoding for multibyte characters."
  (when (ollama-buddy-openai--verify-api-key)
    ;; Set up the current model
    (setq ollama-buddy-openai--current-model
          (or model 
              ollama-buddy-openai--current-model
              (ollama-buddy-openai--get-full-model-name ollama-buddy-openai-default-model)))
    
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
           (json-object-type 'alist)
           (json-array-type 'vector)
           (json-key-type 'symbol)
           (json-encoding-pretty-print nil)
           (json-encoding-default-indentation "")
           (json-payload
            `((model . ,(ollama-buddy-openai--get-real-model-name ollama-buddy-openai--current-model))
              (messages . ,(vconcat [] 
                                   (append
                                    (when (and system-prompt (not (string-empty-p system-prompt)))
                                      `(((role . "system")
                                         (content . ,system-prompt))))
                                    history
                                    `(((role . "user")
                                       (content . ,prompt))))))
              (temperature . ,ollama-buddy-openai-temperature)
              ,@(when ollama-buddy-openai-max-tokens
                  `((max_tokens . ,ollama-buddy-openai-max-tokens)))))
           (payload (json-encode json-payload)))
      
      ;; Prepare the chat buffer
      (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
        (pop-to-buffer (current-buffer))
        (goto-char (point-max))
        (let ((inhibit-read-only t)
              (model-name (ollama-buddy-openai--get-real-model-name 
                           ollama-buddy-openai--current-model))
              (display-name ollama-buddy-openai--current-model))
          
          ;; Add model info to response header
          (insert (propertize (format "\n\n** [%s: RESPONSE]" display-name) 
                              'face `(:inherit bold :foreground 
                                              ,(ollama-buddy-openai--get-model-color display-name))) 
                  "\n\n"))
        
        ;; Show loading message
        (let ((inhibit-read-only t)
              (start-point (point)))
          (when ollama-buddy-openai-show-loading
            (insert "Loading response..."))
          
          ;; Update status
          (ollama-buddy--update-status "Sending request to OpenAI...")
          
          ;; Use curl directly rather than url-retrieve for better encoding control
          (condition-case err
              (let* ((temp-file (make-temp-file "openai-request"))
                     (output-buffer (generate-new-buffer " *openai-output*"))
                     curl-status)
                ;; Write the payload to a temporary file
                (with-temp-file temp-file
                  (insert payload))
                
                ;; Execute curl command
                (setq curl-status
                      (call-process 
                       "curl" nil output-buffer nil
                       "-s" 
                       "-X" "POST"
                       "-H" "Content-Type: application/json"
                       "-H" (concat "Authorization: Bearer " ollama-buddy-openai-api-key)
                       "-d" (format "@%s" temp-file)
                       ollama-buddy-openai-api-endpoint))
                
                ;; Process the response
                (if (= curl-status 0)
                    (with-current-buffer output-buffer
                      (goto-char (point-min))
                      (condition-case json-err
                          (let* ((json-response (json-read))
                                 (error-message (alist-get 'error json-response))
                                 (choices (alist-get 'choices json-response)))
                            
                            ;; Check for errors in response
                            (if error-message
                                (let ((error-text (alist-get 'message error-message)))
                                  (with-current-buffer ollama-buddy--chat-buffer
                                    (let ((inhibit-read-only t))
                                      (delete-region start-point (point-max))
                                      (goto-char start-point)
                                      (insert (propertize (format "Error: %s" error-text)
                                                          'face '(:foreground "red")))
                                      (ollama-buddy--update-status "Error from OpenAI API")
                                      (ollama-buddy--prepare-prompt-area))))
                              
                              ;; Process successful response
                              (when choices
                                (let* ((first-choice (aref choices 0))
                                       (message (alist-get 'message first-choice))
                                       (content (alist-get 'content message)))
                                  
                                  ;; Replace loading message with actual content
                                  (with-current-buffer ollama-buddy--chat-buffer
                                    (let ((inhibit-read-only t))
                                      (delete-region start-point (point-max))
                                      (goto-char start-point)
                                      (insert content)

                                      ;; Now convert from markdown to org if enabled
                                      (when ollama-buddy-convert-markdown-to-org
                                        (ollama-buddy--md-to-org-convert-region start-point (point-max)))

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
                                       (format "Finished [%d tokens]" ollama-buddy-openai--current-token-count))))))))
                        
                        (error
                         (with-current-buffer ollama-buddy--chat-buffer
                           (let ((inhibit-read-only t))
                             (delete-region start-point (point-max))
                             (goto-char start-point)
                             (insert (propertize (format "Error parsing JSON response: %s" 
                                                         (error-message-string json-err))
                                                 'face '(:foreground "red")))
                             (ollama-buddy--update-status "Failed to Parse Response")
                             (ollama-buddy--prepare-prompt-area))))))
                  
                  ;; Handle curl process error
                  (with-current-buffer ollama-buddy--chat-buffer
                    (let ((inhibit-read-only t))
                      (delete-region start-point (point-max))
                      (goto-char start-point)
                      (insert (propertize (format "Error: curl process failed with status %d" curl-status)
                                          'face '(:foreground "red")))
                      (ollama-buddy--update-status "API Request Failed")
                      (ollama-buddy--prepare-prompt-area))))
                
                ;; Clean up
                (delete-file temp-file)
                (when (buffer-live-p output-buffer)
                  (kill-buffer output-buffer)))
            
            ;; Handle Emacs errors during execution
            (error
             (with-current-buffer ollama-buddy--chat-buffer
               (let ((inhibit-read-only t))
                 (delete-region start-point (point-max))
                 (goto-char start-point)
                 (insert (propertize (format "Error: %s" (error-message-string err))
                                     'face '(:foreground "red")))
                 (ollama-buddy--update-status "API Request Failed")
                 (ollama-buddy--prepare-prompt-area))))))))))

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
