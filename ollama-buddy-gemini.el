;;; ollama-buddy-gemini.el --- Google Gemini integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This extension provides Google Gemini integration for the ollama-buddy package.
;; It allows users to interact with Google's Gemini language models using the same interface
;; as ollama-buddy, providing seamless switching between local Ollama models and
;; cloud-based Gemini models.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'ollama-buddy-core)

(defgroup ollama-buddy-gemini nil
  "Google Gemini integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-gemini-")

(defcustom ollama-buddy-gemini-marker-prefix "g:"
  "Prefix used to identify Gemini models in the ollama-buddy interface."
  :type 'string
  :group 'ollama-buddy-gemini)

(defcustom ollama-buddy-gemini-api-key ""
  "API key for accessing Google Gemini services.
Get your key from https://ai.google.dev/."
  :type 'string
  :group 'ollama-buddy-gemini)

(defcustom ollama-buddy-gemini-default-model "gemini-1.5-pro"
  "Default Gemini model to use."
  :type 'string
  :group 'ollama-buddy-gemini)

(defcustom ollama-buddy-gemini-api-endpoint "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent"
  "Endpoint format for Google Gemini API.
The %s will be replaced with the model name."
  :type 'string
  :group 'ollama-buddy-gemini)

(defcustom ollama-buddy-gemini-temperature 0.7
  "Temperature setting for Gemini requests (0.0-1.0).
Lower values make the output more deterministic, higher values more creative."
  :type 'float
  :group 'ollama-buddy-gemini)

(defcustom ollama-buddy-gemini-max-tokens nil
  "Maximum number of tokens to generate in the response.
Use nil for API default behavior (adaptive)."
  :type '(choice integer (const nil))
  :group 'ollama-buddy-gemini)

;; Internal variables

(defvar ollama-buddy-gemini--current-token-count 0
  "Counter for tokens in the current Gemini response.")

(defvar ollama-buddy-remote-models nil
  "List of available remote models.")

;; Helper functions

(defun ollama-buddy-gemini--is-gemini-model (model)
  "Check if MODEL is a Gemini model (starts with the marker prefix)."
  (and model (string-prefix-p ollama-buddy-gemini-marker-prefix model)))

(defun ollama-buddy-gemini--get-full-model-name (model)
  "Get the full model name with prefix for MODEL."
  (concat ollama-buddy-gemini-marker-prefix model))

(defun ollama-buddy-gemini--get-real-model-name (model)
  "Extract the actual model name from the prefixed MODEL string."
  (if (ollama-buddy-gemini--is-gemini-model model)
      (string-trim (substring model (length ollama-buddy-gemini-marker-prefix)))
    model))

;; API interaction functions

(defun ollama-buddy-gemini--verify-api-key ()
  "Verify that the API key is set."
  (if (string-empty-p ollama-buddy-gemini-api-key)
      (progn
        (customize-variable 'ollama-buddy-gemini-api-key)
        (error "Please set your Google Gemini API key"))
    t))

(defun ollama-buddy-gemini--format-messages (messages)
  "Format chat MESSAGES for the Gemini API."
  (let ((formatted-contents '()))
    (dolist (msg messages)
      (let ((role (alist-get 'role msg))
            (content (alist-get 'content msg)))
        (when (string= role "system")
          ;; For system messages, we add it as a user message with a system prefix
          (push `((role . "user")
                  (parts . [((text . ,(format "[System Instruction] %s" content)))]))
                formatted-contents))
        (when (string= role "user")
          (push `((role . "user")
                  (parts . [((text . ,content))]))
                formatted-contents))
        (when (string= role "assistant")
          (push `((role . "model")
                  (parts . [((text . ,content))]))
                formatted-contents))))
    (vconcat [] (reverse formatted-contents))))

(defun ollama-buddy-gemini--send (prompt &optional model)
  "Send PROMPT to Gemini's API using MODEL or default model asynchronously."
  (when (ollama-buddy-gemini--verify-api-key)
    ;; Set up the current model
    (setq ollama-buddy--current-model
          (or model
              ollama-buddy--current-model
              (ollama-buddy-gemini--get-full-model-name
               ollama-buddy-gemini-default-model)))

    ;; Initialize token counter
    (setq ollama-buddy-gemini--current-token-count 0)

    ;; Get history and system prompt
    (let* ((model-name (ollama-buddy-gemini--get-real-model-name
                        ollama-buddy--current-model))
           (history (when ollama-buddy-history-enabled
                      (gethash ollama-buddy--current-model
                               ollama-buddy--conversation-history-by-model
                               nil)))
           (system-prompt ollama-buddy--current-system-prompt)
           ;; For Gemini, we need to handle the system prompt differently
           (messages-with-system
            (if (and system-prompt (not (string-empty-p system-prompt)))
                (append `(((role . "system") (content . ,system-prompt)))
                        history)
              history))
           ;; Add the current prompt to the messages
           (messages-all (append messages-with-system
                                `(((role . "user") (content . ,prompt)))))
           ;; Build the API endpoint with the model name
           (api-endpoint (format ollama-buddy-gemini-api-endpoint model-name))
           ;; Add API key to the endpoint
           (api-endpoint-with-key (concat api-endpoint "?key=" ollama-buddy-gemini-api-key))
           ;; Format messages for Gemini API
           (formatted-contents (ollama-buddy-gemini--format-messages messages-all))
           ;; Build the payload for Gemini
           (json-payload
            `((contents . ,formatted-contents)
              (generationConfig . ((temperature . ,ollama-buddy-gemini-temperature)))))
           ;; Add max tokens if specified
           (json-payload-with-max-tokens
            (if ollama-buddy-gemini-max-tokens
                (let ((generation-config (alist-get 'generationConfig json-payload)))
                  (setf (alist-get 'generationConfig json-payload)
                        (append generation-config
                                `((maxOutputTokens . ,ollama-buddy-gemini-max-tokens))))
                  json-payload)
              json-payload))
           ;; Convert to JSON string
           (json-str (let ((json-encoding-pretty-print nil))
                       (ollama-buddy-escape-unicode (json-encode json-payload-with-max-tokens)))))

      ;; Prepare chat buffer
      (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
        (pop-to-buffer (current-buffer))
        (goto-char (point-max))

        (unless (> (buffer-size) 0)
          (insert (ollama-buddy--create-intro-message)))
        
        (let (start-point
              (inhibit-read-only t))
          
          (insert (propertize (format "\n\n** [%s: RESPONSE]" 
                                      ollama-buddy--current-model) 
                              'face
                              `(:inherit bold :foreground 
                                         ,(ollama-buddy--get-model-color 
                                           ollama-buddy--current-model))) 
                  "\n\n")
          
          (setq start-point (point))
          
          (insert "Loading response...")
          (ollama-buddy--update-status "Sending request to Google Gemini...")

          (set-register ollama-buddy-default-register "")
          
          (let* ((url-request-method "POST")
                 (url-request-extra-headers
                  `(("Content-Type" . "application/json")))
                 ;; Set JSON as raw data - let url library handle encoding
                 (url-request-data json-str))
            
            ;; Setting this makes url.el use binary for the request
            (let ((url-mime-charset-string "utf-8")
                  (url-mime-language-string nil)
                  (url-mime-encoding-string nil)
                  (url-mime-accept-string "application/json"))
              
              (url-retrieve
               api-endpoint-with-key
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
                             (let* ((json-response (json-read-from-string json-response-decoded))
                                    (error-message (alist-get 'error json-response))
                                    (content ""))
                               
                               ;; Extract the message content
                               (if error-message
                                   (setq content (format "Error: %s" (alist-get 'message error-message)))
                                 ;; Parse the Gemini response structure
                                 (let* ((candidates (alist-get 'candidates json-response))
                                        (first-candidate (when (and candidates (> (length candidates) 0))
                                                          (aref candidates 0)))
                                        (content-obj (when first-candidate
                                                      (alist-get 'content first-candidate)))
                                        (parts (when content-obj
                                                (alist-get 'parts content-obj))))
                                   (when (and parts (> (length parts) 0))
                                     (let ((part (aref parts 0)))
                                       (setq content (alist-get 'text part))))))
                               
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
                                   
                                   ;; Calculate token count (approximate)
                                   (setq ollama-buddy-gemini--current-token-count
                                         (length (split-string content "\\b" t)))
                                   
                                   ;; Show token stats if enabled
                                   (when ollama-buddy-display-token-stats
                                     (insert (format "\n\n*** Token Stats\n[%d tokens]"
                                                     ollama-buddy-gemini--current-token-count)))
                                   
                                   (insert "\n\n*** FINISHED")
                                   (ollama-buddy--prepare-prompt-area)
                                   (ollama-buddy--update-status
                                    (format "Finished [%d tokens]"
                                            ollama-buddy-gemini--current-token-count)))))
                           (error
                            (with-current-buffer ollama-buddy--chat-buffer
                              (let ((inhibit-read-only t))
                                (goto-char start-point)
                                (delete-region start-point (point-max))
                                (insert "Error: Failed to parse Gemini response\n")
                                (insert "Details: " (error-message-string err) "\n")
                                (insert "Raw response: " json-response-raw "\n")
                                (insert "\n\n*** FAILED")
                                (ollama-buddy--prepare-prompt-area)
                                (ollama-buddy--update-status "Failed - JSON parse error"))))))))))))))))))

(defun ollama-buddy-gemini--fetch-models ()
  "Fetch available models from Google Gemini API."
  (when (ollama-buddy-gemini--verify-api-key)
    (ollama-buddy--update-status "Fetching Gemini models...")
    (let* ((url-request-method "GET")
           (url-request-extra-headers
            `(("x-goog-api-key" . ,ollama-buddy-gemini-api-key))))
      
      (url-retrieve
       "https://generativelanguage.googleapis.com/v1/models"
       (lambda (status)
         (if (plist-get status :error)
             (progn
               (message "Error fetching Gemini models: %s" (prin1-to-string (plist-get status :error)))
               (ollama-buddy--update-status "Failed to fetch Gemini models"))
           
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
                            (models-data (alist-get 'models json-response))
                            (models (mapcar (lambda (model-info)
                                              (alist-get 'name model-info))
                                            (append models-data nil)))
                            ;; Extract just the model name part, removing the "models/" prefix
                            (processed-models (mapcar (lambda (model)
                                                        (if (string-match "models/\\(.*\\)" model)
                                                            (match-string 1 model)
                                                          model))
                                                      models))
                            ;; Filter to include only gemini models
                            (chat-models (cl-remove-if-not
                                           (lambda (model)
                                             (string-match-p "gemini" model))
                                           processed-models))
                            ;; Prepend the marker prefix to each model name
                            (prefixed-models (mapcar (lambda (model-name)
                                                       (concat ollama-buddy-gemini-marker-prefix model-name))
                                                     chat-models)))

                       ;; Register the Claude handler with ollama-buddy
                       (when (fboundp 'ollama-buddy-register-model-handler)
                         (ollama-buddy-register-model-handler 
                          ollama-buddy-gemini-marker-prefix 
                          #'ollama-buddy-gemini--send))
                       ;; Store models and update status
                       (setq ollama-buddy-remote-models (append ollama-buddy-remote-models prefixed-models)))
                   (error
                    (message "Error parsing Gemini models response: %s" (error-message-string err))
                    (ollama-buddy--update-status "Failed to parse Gemini models response"))))))))))))

(ollama-buddy-gemini--fetch-models)

;; Initialize
(provide 'ollama-buddy-gemini)
;;; ollama-buddy-gemini.el ends here
