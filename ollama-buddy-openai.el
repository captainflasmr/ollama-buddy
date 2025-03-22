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

(defgroup ollama-buddy-openai nil
  "OpenAI integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-openai-")

(defcustom ollama-buddy-openai-api-key ""
  "API key for accessing OpenAI services.
Get your key from https://platform.openai.com/api-keys."
  :type 'string
  :group 'ollama-buddy-openai)

(defcustom ollama-buddy-openai-models
  '("gpt-4o-mini" "gpt-4o" "gpt-3.5-turbo")
  "List of available OpenAI models."
  :type '(repeat string)
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

;; Internal variables
(defvar ollama-buddy-openai--stream-buffer nil
  "Buffer for handling OpenAI stream data.")

(defvar ollama-buddy-openai--active-process nil
  "Current active curl process for OpenAI streaming.")

(defvar ollama-buddy-openai--current-token-count 0
  "Counter for tokens in the current OpenAI response.")

(defvar ollama-buddy-openai--model-colors (make-hash-table :test 'equal)
  "Hash table for storing OpenAI model colors.")

(defvar ollama-buddy-openai--conversation-history-by-model (make-hash-table :test 'equal)
  "Hash table mapping model names to their conversation histories.")

(defvar ollama-buddy-openai--current-response nil
  "Accumulates the current response content.")

(defvar ollama-buddy-openai--current-prompt nil
  "The current prompt sent to OpenAI.")

(defvar ollama-buddy-openai--response-start-position nil
  "Marker for the start position of the current response.")

(defvar ollama-buddy-openai--current-model nil
  "The currently active OpenAI model.")

;; Model display and management functions

(defun ollama-buddy-openai--get-full-model-name (model)
  "Get the full display name for MODEL with prefix."
  (concat ollama-buddy-openai-marker-prefix " " model))

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

(defun ollama-buddy-openai--prepare-message-payload (prompt history system-prompt)
  "Prepare OpenAI API payload for PROMPT, HISTORY, and SYSTEM-PROMPT."
  (let ((messages (list)))
    ;; Add system prompt if provided
    (when (and system-prompt (not (string-empty-p system-prompt)))
      (push `((role . "system")
              (content . ,system-prompt))
            messages))
    
    ;; Add conversation history
    (when history
      (setq messages (append messages history)))
    
    ;; Add the current prompt
    (push `((role . "user")
            (content . ,prompt))
          messages)
    
    ;; Create the final payload
    (json-encode
     `((model . ,(ollama-buddy-openai--get-real-model-name ollama-buddy-openai--current-model))
       (messages . ,(vconcat [] (reverse messages)))
       (temperature . ,ollama-buddy-openai-temperature)
       (stream . t)
       ,@(when ollama-buddy-openai-max-tokens
           `((max_tokens . ,ollama-buddy-openai-max-tokens)))))))

(defun ollama-buddy-openai--parse-stream-data (data start-marker)
  "Parse streaming DATA from OpenAI API and insert at START-MARKER."
  (with-current-buffer ollama-buddy--chat-buffer
    (save-excursion
      (let ((inhibit-read-only t)
            (lines (split-string data "\n" t)))
        
        (dolist (line lines)
          (when (string-prefix-p "data: " line)
            (let* ((json-string (substring line 6))
                   (json-data (if (string= json-string "[DONE]")
                                 '((done . t))
                               (json-read-from-string json-string))))
              
              ;; Extract content delta if present
              (when-let* ((choices (alist-get 'choices json-data))
                          (first-choice (elt choices 0))
                          (delta (alist-get 'delta first-choice))
                          (content (alist-get 'content delta nil)))
                
                ;; Update token count
                (setq ollama-buddy-openai--current-token-count 
                      (1+ ollama-buddy-openai--current-token-count))
                
                ;; Insert content
                (goto-char (point-max))
                (insert content)
                
                ;; Accumulate for history
                (setq ollama-buddy-openai--current-response
                      (concat (or ollama-buddy-openai--current-response "") content))
                
                ;; Update token display
                (ollama-buddy--update-status 
                 (format "Typing... [%d tokens]" ollama-buddy-openai--current-token-count)))
              
              ;; Handle completion
              (when (alist-get 'done json-data)
                ;; Add message pair to history
                (when ollama-buddy-history-enabled
                  (ollama-buddy-openai--add-to-history 
                   "user" ollama-buddy-openai--current-prompt)
                  (ollama-buddy-openai--add-to-history 
                   "assistant" ollama-buddy-openai--current-response))
                
                ;; Reset
                (setq ollama-buddy-openai--current-response nil
                      ollama-buddy-openai--current-prompt nil)
                
                ;; Display completion message and token stats
                (goto-char (point-max))
                (when ollama-buddy-display-token-stats
                  (insert (format "\n\n*** Token Stats\n[%d tokens]"
                                  ollama-buddy-openai--current-token-count)))
                
                (insert "\n\n*** FINISHED")
                (ollama-buddy--prepare-prompt-area)
                (ollama-buddy--update-status 
                 (format "Finished [%d tokens]" ollama-buddy-openai--current-token-count))
                (setq ollama-buddy-openai--current-token-count 0)))))))))

(defun ollama-buddy-openai--stream-filter (proc output)
  "Process stream OUTPUT from PROC."
  (when (buffer-live-p ollama-buddy-openai--stream-buffer)
    (with-current-buffer ollama-buddy-openai--stream-buffer
      (goto-char (point-max))
      (insert output)
      
      ;; Parse the accumulated data
      (ollama-buddy-openai--parse-stream-data
       (buffer-substring-no-properties (point-min) (point-max))
       ollama-buddy-openai--response-start-position)
      
      ;; Keep buffer size manageable
      (when (> (buffer-size) 10000)
        (delete-region (point-min) 5000)))))

(defun ollama-buddy-openai--stream-sentinel (proc event)
  "Handle PROC completion EVENT."
  (let ((status (cond ((string-match-p "finished" event) "Completed")
                     ((string-match-p "\\(?:deleted\\|connection broken\\)" event) "Interrupted"))))
    
    (with-current-buffer ollama-buddy--chat-buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "\n\n[OpenAI Stream %s]" status) 'face '(:weight bold)))
        (ollama-buddy--prepare-prompt-area)))
    
    (when (buffer-live-p ollama-buddy-openai--stream-buffer)
      (kill-buffer ollama-buddy-openai--stream-buffer))
    
    (ollama-buddy--update-status (concat "Stream " status))))

(defun ollama-buddy-openai--send (prompt &optional model)
  "Send PROMPT to OpenAI's API using MODEL or default model."
  (when (ollama-buddy-openai--verify-api-key)
    ;; Set up the current model
    (setq ollama-buddy-openai--current-model
          (or model 
              ollama-buddy-openai--current-model
              (ollama-buddy-openai--get-full-model-name ollama-buddy-openai-default-model)))
    
    ;; Store the prompt
    (setq ollama-buddy-openai--current-prompt prompt)
    
    ;; Initialize token counter
    (setq ollama-buddy-openai--current-token-count 0)

    ;; Create a buffer for the streaming response
    (when (buffer-live-p ollama-buddy-openai--stream-buffer)
      (kill-buffer ollama-buddy-openai--stream-buffer))
    (setq ollama-buddy-openai--stream-buffer (generate-new-buffer " *openai-stream*"))
    
    ;; Get history and system prompt
    (let* ((history (when ollama-buddy-history-enabled
                     (gethash ollama-buddy-openai--current-model
                              ollama-buddy-openai--conversation-history-by-model
                              nil)))
           (system-prompt ollama-buddy--current-system-prompt)
           (payload (ollama-buddy-openai--prepare-message-payload
                     prompt history system-prompt)))
      
      ;; Prepare the chat buffer
      (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
        (pop-to-buffer (current-buffer))
        (goto-char (point-max))
        (let ((model-name (ollama-buddy-openai--get-real-model-name 
                           ollama-buddy-openai--current-model))
              (display-name ollama-buddy-openai--current-model))
          
          ;; Add model info to response header
          (insert (propertize (format "\n\n** [%s: RESPONSE]" display-name) 
                              'face `(:inherit bold :foreground 
                                              ,(ollama-buddy-openai--get-model-color display-name))) 
                  "\n\n"))
        
        ;; Store the start position of the response
        (setq ollama-buddy-openai--response-start-position (point-max)))
      
      ;; Kill previous process if active
      (when (and ollama-buddy-openai--active-process
                 (process-live-p ollama-buddy-openai--active-process))
        (delete-process ollama-buddy-openai--active-process))
      
      ;; Update status
      (ollama-buddy--update-status "Sending request to OpenAI...")
      
      ;; Create curl process for streaming
      (setq ollama-buddy-openai--active-process
            (start-process 
             "openai-chat-stream" 
             ollama-buddy-openai--stream-buffer
             "curl" 
             "-s" "--no-buffer"
             "-X" "POST"
             "-H" "Content-Type: application/json"
             "-H" (concat "Authorization: Bearer " ollama-buddy-openai-api-key)
             "-d" payload
             ollama-buddy-openai-api-endpoint))
      
      ;; Set up process handlers
      (set-process-filter ollama-buddy-openai--active-process #'ollama-buddy-openai--stream-filter)
      (set-process-sentinel ollama-buddy-openai--active-process #'ollama-buddy-openai--stream-sentinel))))

(defun ollama-buddy-openai--cancel-request ()
  "Cancel the current OpenAI request."
  (interactive)
  (when (and ollama-buddy-openai--active-process
             (process-live-p ollama-buddy-openai--active-process))
    (delete-process ollama-buddy-openai--active-process)
    (setq ollama-buddy-openai--active-process nil))
  
  (when (buffer-live-p ollama-buddy-openai--stream-buffer)
    (kill-buffer ollama-buddy-openai--stream-buffer))
  
  (with-current-buffer ollama-buddy--chat-buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n\n[OpenAI Request Canceled]")
      (ollama-buddy--prepare-prompt-area)))
  
  (ollama-buddy--update-status "OpenAI request canceled"))

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
      (when (> (length history) (* 2 ollama-buddy-max-history-length))
        (setq history (seq-take history (* 2 ollama-buddy-max-history-length))))
      
      ;; Update the hash table with the modified history
      (puthash model history ollama-buddy-openai--conversation-history-by-model))))

;; Command functions

(defun ollama-buddy-openai-send (&optional prompt model)
  "Send PROMPT to OpenAI API using MODEL.
If not provided, uses the current prompt and default model."
  (interactive)
  (let ((prompt-to-send (or prompt (ollama-buddy--text-after-prompt))))
    (if (string-empty-p prompt-to-send)
        (message "Please enter a prompt")
      (ollama-buddy-openai--send prompt-to-send model))))

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

;;;###autoload
(defun ollama-buddy-openai-send-prompt ()
  "Send the current prompt to OpenAI API."
  (interactive)
  (ollama-buddy-openai-send))

(provide 'ollama-buddy-openai)
;;; ollama-buddy-openai.el ends here
