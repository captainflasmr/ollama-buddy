;;; ollama-buddy-grok.el --- Grok integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This extension provides Grok integration for the ollama-buddy package.
;; It allows users to interact with Grok's language models using the same interface
;; as ollama-buddy, providing seamless switching between local Ollama models and
;; cloud-based Grok models.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'ollama-buddy-core)
(require 'ollama-buddy-remote)

(defgroup ollama-buddy-grok nil
  "Grok integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-grok-")

(defcustom ollama-buddy-grok-marker-prefix "k:"
  "Prefix used to identify Grok models in the ollama-buddy interface."
  :type 'string
  :group 'ollama-buddy-grok)

(defcustom ollama-buddy-grok-api-key ""
  "API key for accessing Grok services.
Get your key from the Grok API dashboard."
  :type 'string
  :group 'ollama-buddy-grok)

(defcustom ollama-buddy-grok-default-model "grok-1"
  "Default Grok model to use."
  :type 'string
  :group 'ollama-buddy-grok)

(defcustom ollama-buddy-grok-api-endpoint "https://api.x.ai/v1/chat/completions"
  "Endpoint for Grok chat completions API."
  :type 'string
  :group 'ollama-buddy-grok)

(defcustom ollama-buddy-grok-temperature 0.7
  "Temperature setting for Grok requests (0.0-1.0).
Lower values make the output more deterministic, higher values more creative."
  :type 'float
  :group 'ollama-buddy-grok)

(defcustom ollama-buddy-grok-max-tokens nil
  "Maximum number of tokens to generate in the response.
Use nil for API default behavior (adaptive)."
  :type '(choice integer (const nil))
  :group 'ollama-buddy-grok)

;; Internal variables

(defvar ollama-buddy-grok--current-token-count 0
  "Counter for tokens in the current Grok response.")

;; API interaction functions

(defun ollama-buddy-grok--send (prompt &optional model)
  "Send PROMPT to Grok's API using MODEL or default model asynchronously."
  (when (ollama-buddy-remote--verify-api-key
         ollama-buddy-grok-api-key
         'ollama-buddy-grok-api-key
         "Grok")
    (ollama-buddy-remote--openai-send
     prompt model
     (list :prefix ollama-buddy-grok-marker-prefix
           :api-key ollama-buddy-grok-api-key
           :endpoint ollama-buddy-grok-api-endpoint
           :temperature ollama-buddy-grok-temperature
           :max-tokens ollama-buddy-grok-max-tokens
           :default-model ollama-buddy-grok-default-model
           :provider-name "Grok"
           :token-count-var 'ollama-buddy-grok--current-token-count))))

(defun ollama-buddy-grok--fetch-models ()
  "Fetch available models from Grok API."
  (when (ollama-buddy-remote--verify-api-key
         ollama-buddy-grok-api-key
         'ollama-buddy-grok-api-key
         "Grok")
    (ollama-buddy--update-status "Fetching Grok models...")
    (let* ((url-request-method "GET")
           (url-request-extra-headers
            `(("Authorization" . ,(concat "Bearer " ollama-buddy-grok-api-key)))))

      (url-retrieve
       "https://api.x.ai/v1/models"
       (lambda (status)
         (if (plist-get status :error)
             (progn
               (message "Error fetching Grok models: %s" (prin1-to-string (plist-get status :error)))
               (ollama-buddy--update-status "Failed to fetch Grok models"))

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
                                          (append models-data nil))))
                     (ollama-buddy-remote--register-models
                      ollama-buddy-grok-marker-prefix
                      models
                      #'ollama-buddy-grok--send))
                 (error
                  (message "Error parsing Grok models response: %s" (error-message-string err))
                  (ollama-buddy--update-status "Failed to parse Grok models response"))))))))))))

;; Initialize the Grok integration by fetching available models
(ollama-buddy-grok--fetch-models)

(provide 'ollama-buddy-grok)
;;; ollama-buddy-grok.el ends here
