;;; ollama-buddy-gemini.el --- Google Gemini integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.9.23
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

;; Helper functions

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

;; API interaction functions

(defun ollama-buddy-gemini--verify-api-key ()
  "Verify that the API key is set."
  (if (string-empty-p ollama-buddy-gemini-api-key)
      (progn
        (customize-variable 'ollama-buddy-gemini-api-key)
        (error "Please set your Google Gemini API key"))
    t))

(defun ollama-buddy-gemini--send (prompt &optional model)
  "Send PROMPT to Gemini's API using MODEL or default model asynchronously."
  (when (ollama-buddy-gemini--verify-api-key)
    ;; Set up the current model
    (setq ollama-buddy-gemini--current-model
          (or model
              ollama-buddy-gemini--current-model
              (ollama-buddy-gemini--get-full-model-name
               ollama-buddy-gemini-default-model)))

    ;; Initialize token counter
    (setq ollama-buddy-gemini--current-token-count 0)

    ;; Get history and system prompt
    (let* ((model-name (ollama-buddy-gemini--get-real-model-name
                        ollama-buddy-gemini--current-model))
           (history (when ollama-buddy-history-enabled
                      (gethash ollama-buddy-gemini--current-model
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
                                      ollama-buddy-gemini--current-model) 
                              'face
                              `(:inherit bold :foreground 
                                         ,(ollama-buddy--get-model-color 
                                           ollama-buddy-gemini--current-model))) 
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
                              (json-object-type 'alist)
                              (json-array-type 'vector)
                              (json-key-type 'symbol))
                         
                         (condition-case err
                             (let* ((json-response (json-read-from-string json-response-raw))
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
                                       (setq content (ollama-buddy-fix-encoding-issues
                                                     (alist-get 'text part)))))))
                               
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

;; Integration with ollama-buddy

(defun ollama-buddy-gemini--get-models ()
  "Return list of Gemini models with proper prefix."
  (mapcar (lambda (model)
            (ollama-buddy-gemini--get-full-model-name model))
          ollama-buddy-gemini--available-models))

;; Hook to add Gemini models to the available models list
(defun ollama-buddy-gemini--add-models-to-list ()
  "Add Gemini models to the available models list."
  (setq ollama-buddy-available-models 
        (append ollama-buddy-available-models 
                (ollama-buddy-gemini--get-models))))

;; Load hook
(add-hook 'ollama-buddy-after-init-hook #'ollama-buddy-gemini--add-models-to-list)

;; Initialize
(provide 'ollama-buddy-gemini)
;;; ollama-buddy-gemini.el ends here
