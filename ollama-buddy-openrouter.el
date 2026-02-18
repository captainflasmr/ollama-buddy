;;; ollama-buddy-openrouter.el --- OpenRouter integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This extension provides OpenRouter integration for the ollama-buddy package.
;; OpenRouter is a unified API that provides access to 400+ models from all
;; major providers (OpenAI, Anthropic, Google, Meta, Mistral, etc.) through a
;; single OpenAI-compatible endpoint.
;;
;; Usage:
;; (require 'ollama-buddy-openrouter)
;; (setq ollama-buddy-openrouter-api-key (getenv "OPENROUTER_API_KEY"))

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'ollama-buddy-core)
(require 'ollama-buddy-remote)

(defgroup ollama-buddy-openrouter nil
  "OpenRouter integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-openrouter-")

(defcustom ollama-buddy-openrouter-marker-prefix "r:"
  "Prefix used to identify OpenRouter models in the interface."
  :type 'string
  :group 'ollama-buddy-openrouter)

(defcustom ollama-buddy-openrouter-api-key ""
  "API key for accessing OpenRouter services.
Get your key from https://openrouter.ai/keys."
  :type 'string
  :group 'ollama-buddy-openrouter)

(defcustom ollama-buddy-openrouter-default-model "openai/gpt-4o"
  "Default OpenRouter model to use.
Models use provider/name format (e.g. openai/gpt-4o)."
  :type 'string
  :group 'ollama-buddy-openrouter)

(defcustom ollama-buddy-openrouter-api-endpoint
  "https://openrouter.ai/api/v1/chat/completions"
  "Endpoint for OpenRouter chat completions API."
  :type 'string
  :group 'ollama-buddy-openrouter)

(defcustom ollama-buddy-openrouter-temperature 0.7
  "Temperature setting for OpenRouter requests (0.0-2.0).
Lower values make output more deterministic."
  :type 'float
  :group 'ollama-buddy-openrouter)

(defcustom ollama-buddy-openrouter-max-tokens nil
  "Maximum number of tokens to generate in the response.
Use nil for API default behavior (adaptive)."
  :type '(choice integer (const nil))
  :group 'ollama-buddy-openrouter)

(defcustom ollama-buddy-openrouter-model-filter nil
  "Regex filter for OpenRouter models.
When non-nil, only models matching this regex will be shown.
Use nil to show all models."
  :type '(choice (const nil) string)
  :group 'ollama-buddy-openrouter)

;; Internal variables

(defvar ollama-buddy-openrouter--current-token-count 0
  "Counter for tokens in the current OpenRouter response.")

;; API interaction functions

(defun ollama-buddy-openrouter--send (prompt &optional model)
  "Send PROMPT to OpenRouter API using MODEL asynchronously."
  (when (ollama-buddy-remote--verify-api-key
         ollama-buddy-openrouter-api-key
         'ollama-buddy-openrouter-api-key
         "OpenRouter")
    (ollama-buddy-remote--openai-send
     prompt model
     (list
      :prefix ollama-buddy-openrouter-marker-prefix
      :api-key ollama-buddy-openrouter-api-key
      :endpoint ollama-buddy-openrouter-api-endpoint
      :temperature ollama-buddy-openrouter-temperature
      :max-tokens ollama-buddy-openrouter-max-tokens
      :default-model ollama-buddy-openrouter-default-model
      :provider-name "OpenRouter"
      :extra-headers
      '(("HTTP-Referer" .
         "https://github.com/captainflasmr/ollama-buddy")
        ("X-Title" . "ollama-buddy"))
      :token-count-var
      'ollama-buddy-openrouter--current-token-count))))

(defun ollama-buddy-openrouter--fetch-models ()
  "Fetch available models from OpenRouter API."
  (when (ollama-buddy-remote--verify-api-key
         ollama-buddy-openrouter-api-key
         'ollama-buddy-openrouter-api-key
         "OpenRouter")
    (ollama-buddy--update-status "Fetching OpenRouter models...")
    (let* ((url-request-method "GET")
           (url-request-extra-headers
            `(("Authorization"
               . ,(concat "Bearer "
                          ollama-buddy-openrouter-api-key)))))
      (url-retrieve
       "https://openrouter.ai/api/v1/models"
       (lambda (status)
         (if (plist-get status :error)
             (ollama-buddy-remote--friendly-fetch-error status "OpenRouter")
           (goto-char (point-min))
           (when (re-search-forward "\n\n" nil t)
             (let* ((json-response-raw
                     (buffer-substring (point) (point-max)))
                    (json-object-type 'alist)
                    (json-array-type 'vector)
                    (json-key-type 'symbol))
               (condition-case err
                   (let* ((json-response
                           (json-read-from-string
                            json-response-raw))
                          (models-data
                           (alist-get 'data json-response))
                          (models
                           (mapcar
                            (lambda (m)
                              (alist-get 'id m))
                            (append models-data nil)))
                          (filtered-models
                           (if ollama-buddy-openrouter-model-filter
                               (cl-remove-if-not
                                (lambda (model)
                                  (string-match-p
                                   ollama-buddy-openrouter-model-filter
                                   model))
                                models)
                             models)))
                     (ollama-buddy-remote--register-models
                      ollama-buddy-openrouter-marker-prefix
                      filtered-models
                      #'ollama-buddy-openrouter--send))
                 (error
                  (message
                   "Error parsing OpenRouter models: %s"
                   (error-message-string err))
                  (ollama-buddy--update-status
                   "Failed to parse OpenRouter models")))))))))))

(ollama-buddy-openrouter--fetch-models)

(provide 'ollama-buddy-openrouter)
;;; ollama-buddy-openrouter.el ends here
