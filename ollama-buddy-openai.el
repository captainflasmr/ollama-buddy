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
(require 'ollama-buddy-remote)

(defgroup ollama-buddy-openai nil
  "OpenAI integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-openai-")

(defcustom ollama-buddy-openai-marker-prefix "a:"
  "Prefix to indicate that a model is from OpenAI rather than Ollama."
  :type 'string
  :group 'ollama-buddy-openai)

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

;; API interaction functions

(defun ollama-buddy-openai--send (prompt &optional model)
  "Send PROMPT to OpenAI's API using MODEL or default model asynchronously."
  (when (ollama-buddy-remote--verify-api-key
         ollama-buddy-openai-api-key
         'ollama-buddy-openai-api-key
         "OpenAI")
    (ollama-buddy-remote--openai-send
     prompt model
     (list :prefix ollama-buddy-openai-marker-prefix
           :api-key ollama-buddy-openai-api-key
           :endpoint ollama-buddy-openai-api-endpoint
           :temperature ollama-buddy-openai-temperature
           :max-tokens ollama-buddy-openai-max-tokens
           :default-model ollama-buddy-openai-default-model
           :provider-name "OpenAI"
           :token-count-var 'ollama-buddy-openai--current-token-count))))

(defun ollama-buddy-openai--fetch-models ()
  "Fetch available models from OpenAI API."
  (when (ollama-buddy-remote--verify-api-key
         ollama-buddy-openai-api-key
         'ollama-buddy-openai-api-key
         "OpenAI")
    (ollama-buddy--update-status "Fetching OpenAI models...")
    (let* ((url-request-method "GET")
           (url-request-extra-headers
            `(("Authorization" . ,(concat "Bearer " ollama-buddy-openai-api-key)))))

      (url-retrieve
       "https://api.openai.com/v1/models"
       (lambda (status)
         (if (plist-get status :error)
             (ollama-buddy-remote--friendly-fetch-error status "OpenAI")

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
                     (ollama-buddy-remote--register-models
                      ollama-buddy-openai-marker-prefix
                      chat-models
                      #'ollama-buddy-openai--send))
                 (error
                  (message "Error parsing OpenAI models response: %s" (error-message-string err))
                  (ollama-buddy--update-status "Failed to parse OpenAI models response"))))))))))))

(ollama-buddy-openai--fetch-models)

(provide 'ollama-buddy-openai)
;;; ollama-buddy-openai.el ends here
