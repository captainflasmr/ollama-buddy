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
(require 'ollama-buddy-remote)

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

;; Gemini-specific helper

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

(defun ollama-buddy-gemini--send (prompt &optional model)
  "Send PROMPT to Gemini's API using MODEL or default model asynchronously."
  (when (ollama-buddy-remote--verify-api-key
         ollama-buddy-gemini-api-key
         'ollama-buddy-gemini-api-key
         "Google Gemini")
    ;; Process inline features
    (setq prompt (ollama-buddy-remote--process-inline-features prompt))

    ;; Set up the current model
    (setq ollama-buddy--current-model
          (or model
              ollama-buddy--current-model
              (ollama-buddy-remote--get-full-model-name
               ollama-buddy-gemini-marker-prefix
               ollama-buddy-gemini-default-model)))

    ;; Initialize token counter
    (setq ollama-buddy-gemini--current-token-count 0)

    ;; Get history and system prompt
    (let* ((model-name (ollama-buddy-remote--get-real-model-name
                        ollama-buddy-gemini-marker-prefix
                        ollama-buddy--current-model))
           (history (when ollama-buddy-history-enabled
                      (gethash ollama-buddy--current-model
                               ollama-buddy--conversation-history-by-model
                               nil)))
           (system-prompt (ollama-buddy--effective-system-prompt))
           (full-context (ollama-buddy-remote--build-context))
           ;; For Gemini, we need to handle the system prompt differently
           (messages-with-system
            (if (and system-prompt (not (string-empty-p system-prompt)))
                (append `(((role . "system") (content . ,system-prompt)))
                        history)
              history))
           ;; Add the current prompt to the messages
           (messages-all (append messages-with-system
                                 `(((role . "user")
                                    (content . ,(if full-context
                                                    (concat prompt "\n\n" full-context)
                                                  prompt))))))
           ;; Build the API endpoint with the model name
           (api-endpoint (format ollama-buddy-gemini-api-endpoint model-name))
           ;; Add API key to the endpoint (non-streaming uses generateContent)
           (api-endpoint-with-key (concat api-endpoint "?key=" ollama-buddy-gemini-api-key))
           ;; Streaming endpoint uses streamGenerateContent with alt=sse
           (stream-api-endpoint
            (concat (replace-regexp-in-string
                     ":generateContent$" ":streamGenerateContent" api-endpoint)
                    "?alt=sse&key=" ollama-buddy-gemini-api-key))
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
                       (ollama-buddy-escape-unicode (json-encode json-payload-with-max-tokens))))
           (start-point (ollama-buddy-remote--prepare-chat-buffer "Google Gemini"))
           (gemini-headers `(("Content-Type" . "application/json"))))

      (if ollama-buddy-streaming-enabled
          ;; Streaming path: use curl with SSE via streamGenerateContent
          (ollama-buddy-remote--start-streaming-request
           stream-api-endpoint
           gemini-headers
           json-str
           #'ollama-buddy-remote--gemini-extract-content
           "Google Gemini"
           prompt
           start-point)
        ;; Non-streaming path: use url-retrieve
        (let* ((url-request-method "POST")
               (url-request-extra-headers gemini-headers)
               (url-request-data json-str)
               (url-mime-charset-string "utf-8")
               (url-mime-language-string nil)
               (url-mime-encoding-string nil)
               (url-mime-accept-string "application/json"))

          (url-retrieve
           api-endpoint-with-key
           (lambda (status)
             (if (plist-get status :error)
                 (ollama-buddy-remote--handle-http-error
                  start-point (plist-get status :error))
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

                           ;; Extract the message content (Gemini-specific format)
                           (if error-message
                               (setq content (format "Error: %s"
                                                     (ollama-buddy-remote--format-api-error
                                                      error-message)))
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

                           ;; Finalize the response
                           (ollama-buddy-remote--finalize-response
                            start-point content prompt
                            'ollama-buddy-gemini--current-token-count))
                       (error
                        (ollama-buddy-remote--handle-error
                         start-point "Gemini"
                         (error-message-string err)))))))))))))))

(defun ollama-buddy-gemini--fetch-models ()
  "Fetch available models from Google Gemini API."
  (when (ollama-buddy-remote--verify-api-key
         ollama-buddy-gemini-api-key
         'ollama-buddy-gemini-api-key
         "Google Gemini")
    (ollama-buddy--update-status "Fetching Gemini models...")
    (let* ((url-request-method "GET")
           (url-request-extra-headers
            `(("x-goog-api-key" . ,ollama-buddy-gemini-api-key))))

      (url-retrieve
       "https://generativelanguage.googleapis.com/v1/models"
       (lambda (status)
         (if (plist-get status :error)
             (ollama-buddy-remote--friendly-fetch-error status "Gemini")

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
                            (models-data (append (alist-get 'models json-response) nil))
                            ;; Build list of (short-name ctx display-name) entries
                            (model-triples
                             (delq nil
                                   (mapcar (lambda (model-info)
                                             (let* ((raw     (alist-get 'name model-info))
                                                    (short   (if (string-match "models/\\(.*\\)" raw)
                                                                 (match-string 1 raw)
                                                               raw))
                                                    (ctx     (alist-get 'inputTokenLimit model-info))
                                                    (display (alist-get 'displayName model-info)))
                                               (when (string-match-p "gemini" short)
                                                 (list short ctx display))))
                                           models-data)))
                            (chat-models (mapcar #'car model-triples)))
                       (ollama-buddy-remote--register-models
                        ollama-buddy-gemini-marker-prefix
                        chat-models
                        #'ollama-buddy-gemini--send)
                       ;; Store context windows and display names in the shared metadata cache
                       (dolist (entry model-triples)
                         (let ((full-name (concat ollama-buddy-gemini-marker-prefix (car entry))))
                           (puthash full-name
                                    `((context-window . ,(cadr entry))
                                      (display-name   . ,(caddr entry)))
                                    ollama-buddy--models-metadata-cache))))
                   (error
                    (message "Error parsing Gemini models response: %s" (error-message-string err))
                    (ollama-buddy--update-status "Failed to parse Gemini models response"))))))))))))

(ollama-buddy-gemini--fetch-models)

;; Initialize
(provide 'ollama-buddy-gemini)
;;; ollama-buddy-gemini.el ends here
