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
(require 'ollama-buddy-remote)

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

;; API interaction functions

(defun ollama-buddy-claude--extract-content (response)
  "Extract text content from a Claude API RESPONSE."
  (let ((extracted-text ""))
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
    extracted-text))

(defun ollama-buddy-claude--send (prompt &optional model)
  "Send PROMPT to Claude's API using MODEL."
  (when (ollama-buddy-remote--verify-api-key
         ollama-buddy-claude-api-key
         'ollama-buddy-claude-api-key
         "Anthropic Claude")
    ;; Process inline features
    (setq prompt (ollama-buddy-remote--process-inline-features prompt))

    ;; Set up the current model
    (setq ollama-buddy--current-model
          (or model
              ollama-buddy--current-model
              (ollama-buddy-remote--get-full-model-name
               ollama-buddy-claude-marker-prefix
               ollama-buddy-claude-default-model)))

    ;; Initialize token counter
    (setq ollama-buddy-claude--current-token-count 0)

    ;; Get history and system prompt
    (let* ((history (when ollama-buddy-history-enabled
                      (gethash ollama-buddy--current-model
                               ollama-buddy--conversation-history-by-model
                               nil)))
           (system-prompt (ollama-buddy--effective-system-prompt))
           (full-context (ollama-buddy-remote--build-context))
           ;; Claude: system prompt is NOT in messages array, it's a top-level key
           (messages (vconcat []
                              (append
                               history
                               `(((role . "user")
                                  (content . ,(if full-context
                                                  (concat prompt "\n\n" full-context)
                                                prompt)))))))
           (max-tokens (or ollama-buddy-claude-max-tokens 4096))
           (json-payload
            `((model . ,(ollama-buddy-remote--get-real-model-name
                         ollama-buddy-claude-marker-prefix
                         ollama-buddy--current-model))
              (messages . ,messages)
              (temperature . ,ollama-buddy-claude-temperature)
              (max_tokens . ,max-tokens)))
           (json-payload (if (and system-prompt (not (string-empty-p system-prompt)))
                             (append json-payload `((system . ,system-prompt)))
                           json-payload))
           (stream-json-payload
            (if ollama-buddy-streaming-enabled
                (append json-payload '((stream . t)))
              json-payload))
           (json-str (let ((json-encoding-pretty-print nil))
                       (ollama-buddy-escape-unicode (json-encode stream-json-payload))))
           (start-point (ollama-buddy-remote--prepare-chat-buffer "Claude"))
           (claude-headers `(("Content-Type" . "application/json")
                             ("Authorization" . ,(concat "Bearer " ollama-buddy-claude-api-key))
                             ("X-API-Key" . ,ollama-buddy-claude-api-key)
                             ("anthropic-version" . "2023-06-01"))))

      (if ollama-buddy-streaming-enabled
          ;; Streaming path: use curl with SSE
          (ollama-buddy-remote--start-streaming-request
           ollama-buddy-claude-api-endpoint
           claude-headers
           json-str
           #'ollama-buddy-remote--claude-extract-content
           "Claude"
           prompt
           start-point)
        ;; Non-streaming path: use url-retrieve
        (let* ((url-request-method "POST")
               (url-request-extra-headers claude-headers)
               (url-request-data json-str)
               (url-mime-charset-string "utf-8")
               (url-mime-language-string nil)
               (url-mime-encoding-string nil)
               (url-mime-accept-string "application/json"))

          (url-retrieve
           ollama-buddy-claude-api-endpoint
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
                         (let* ((response (json-read-from-string json-response-decoded))
                                (error-message (alist-get 'error response))
                                (content ""))

                           ;; Extract the message content (Claude-specific format)
                           (if error-message
                               (setq content (format "Error: %s"
                                                     (ollama-buddy-remote--format-api-error
                                                      error-message)))
                             (setq content (ollama-buddy-claude--extract-content response)))

                           ;; Finalize the response
                           (ollama-buddy-remote--finalize-response
                            start-point content prompt
                            'ollama-buddy-claude--current-token-count))
                       (error
                        (ollama-buddy-remote--handle-error
                         start-point "Claude"
                         (error-message-string err)))))))))))))))

(defun ollama-buddy-claude--fetch-models ()
  "Fetch available models from Anthropic's Claude API."
  (when (ollama-buddy-remote--verify-api-key
         ollama-buddy-claude-api-key
         'ollama-buddy-claude-api-key
         "Anthropic Claude")
    (ollama-buddy--update-status "Fetching Claude models...")
    (let* ((url-request-method "GET")
           (url-request-extra-headers
            `(("x-api-key" . ,ollama-buddy-claude-api-key)
              ("anthropic-version" . "2023-06-01"))))

      (url-retrieve
       "https://api.anthropic.com/v1/models"
       (lambda (status)
         (if (plist-get status :error)
             (ollama-buddy-remote--friendly-fetch-error status "Claude")

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
                            (models-data (append (alist-get 'data json-response) nil))
                            ;; Build list of (id . display_name) pairs for claude models
                            (model-pairs
                             (delq nil
                                   (mapcar (lambda (model-info)
                                             (let ((id      (alist-get 'id model-info))
                                                   (display (alist-get 'display_name model-info)))
                                               (when (string-match-p "claude" id)
                                                 (cons id display))))
                                           models-data)))
                            (chat-models (mapcar #'car model-pairs)))
                       (ollama-buddy-remote--register-models
                        ollama-buddy-claude-marker-prefix
                        chat-models
                        #'ollama-buddy-claude--send)
                       ;; Store display names and context windows in the shared metadata cache
                       (dolist (pair model-pairs)
                         (let* ((full-name (concat ollama-buddy-claude-marker-prefix (car pair)))
                                (ctx (ollama-buddy--get-context-window full-name)))
                           (puthash full-name
                                    `((display-name    . ,(cdr pair))
                                      (context-window  . ,ctx))
                                    ollama-buddy--models-metadata-cache))))
                   (error
                    (message "Error parsing Claude models response: %s" (error-message-string err))
                    (ollama-buddy--update-status "Failed to parse Claude models response"))))))))))))

(ollama-buddy-claude--fetch-models)

(provide 'ollama-buddy-claude)
;;; ollama-buddy-claude.el ends here
