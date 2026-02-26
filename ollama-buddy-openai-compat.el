;;; ollama-buddy-openai-compat.el --- Generic OpenAI-compatible provider for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This extension provides a generic OpenAI-compatible provider for
;; ollama-buddy.  Unlike the dedicated ollama-buddy-openai module (which is
;; hard-wired to api.openai.com), this module lets you point ollama-buddy at
;; ANY server that speaks the OpenAI chat-completions API, for example:
;;
;;   - LM Studio    (http://localhost:1234)
;;   - llama.cpp    (http://localhost:8080)
;;   - vLLM         (http://localhost:8000)
;;   - Ollama's own OpenAI-compat layer (http://localhost:11434/v1 → remove /v1)
;;   - Jan          (http://localhost:1337)
;;   - Any remote OpenAI-API-compatible service
;;
;; Usage:
;;   (require 'ollama-buddy-openai-compat)
;;   (setq ollama-buddy-openai-compat-base-url "http://localhost:1234")
;;
;; The module attempts to discover available models by calling GET /v1/models.
;; If that fails (server offline, unsupported endpoint) it falls back to the
;; user-defined `ollama-buddy-openai-compat-models' list.
;;
;; An API key is optional.  Leave it empty ("") for local servers that do not
;; require authentication.  When non-empty it is sent as a Bearer token.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'ollama-buddy-core)
(require 'ollama-buddy-remote)

;;; Customization

(defgroup ollama-buddy-openai-compat nil
  "Generic OpenAI-compatible provider for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-openai-compat-")

(defcustom ollama-buddy-openai-compat-base-url "http://localhost:1234"
  "Base URL of the OpenAI-compatible server (no trailing slash).
The module appends /v1/chat/completions for chat and /v1/models for
model discovery.  Examples:
  \"http://localhost:1234\"  -- LM Studio
  \"http://localhost:8080\"  -- llama.cpp
  \"http://localhost:8000\"  -- vLLM"
  :type 'string
  :group 'ollama-buddy-openai-compat)

(defcustom ollama-buddy-openai-compat-api-key ""
  "API key sent as a Bearer token to the server.
Leave empty for local servers that do not require authentication."
  :type 'string
  :group 'ollama-buddy-openai-compat)

(defcustom ollama-buddy-openai-compat-models nil
  "Fallback list of model names when automatic discovery fails.
Each entry is a plain model name without the provider prefix,
e.g. \"llama-3.2-3b-instruct\" or \"mistral-7b-instruct\".
If `ollama-buddy-openai-compat-fetch-models' is nil these are used directly."
  :type '(repeat string)
  :group 'ollama-buddy-openai-compat)

(defcustom ollama-buddy-openai-compat-default-model ""
  "Default model to use when no model is explicitly selected.
If empty the first model returned by the server (or the first entry in
`ollama-buddy-openai-compat-models') is used."
  :type 'string
  :group 'ollama-buddy-openai-compat)

(defcustom ollama-buddy-openai-compat-provider-name "LocalAI"
  "Display name shown in the status line and chat buffer headings."
  :type 'string
  :group 'ollama-buddy-openai-compat)

(defcustom ollama-buddy-openai-compat-marker-prefix "l:"
  "Prefix used to distinguish this provider's models in the model list.
Change this only if it conflicts with another loaded provider."
  :type 'string
  :group 'ollama-buddy-openai-compat)

(defcustom ollama-buddy-openai-compat-temperature 0.7
  "Temperature for requests (0.0–2.0).
Lower values produce more deterministic output."
  :type 'float
  :group 'ollama-buddy-openai-compat)

(defcustom ollama-buddy-openai-compat-max-tokens nil
  "Maximum tokens to generate, or nil for the server default."
  :type '(choice integer (const nil))
  :group 'ollama-buddy-openai-compat)

(defcustom ollama-buddy-openai-compat-fetch-models t
  "When non-nil, attempt to discover models via GET /v1/models on load.
Falls back to `ollama-buddy-openai-compat-models' if the request fails."
  :type 'boolean
  :group 'ollama-buddy-openai-compat)

;;; Internal state

(defvar ollama-buddy-openai-compat--current-token-count 0
  "Token counter for the current response.")

;;; Send function

(defun ollama-buddy-openai-compat--send (prompt &optional model)
  "Send PROMPT to the configured OpenAI-compatible server using MODEL."
  (let* ((key ollama-buddy-openai-compat-api-key)
         (prefix ollama-buddy-openai-compat-marker-prefix)
         (endpoint (concat ollama-buddy-openai-compat-base-url
                           "/v1/chat/completions"))
         (default-model
          (if (and ollama-buddy-openai-compat-default-model
                   (not (string-empty-p
                         ollama-buddy-openai-compat-default-model)))
              ollama-buddy-openai-compat-default-model
            (car ollama-buddy-openai-compat-models))))
    (ollama-buddy-remote--openai-send
     prompt model
     (list :prefix    prefix
           :api-key   key
           :endpoint  endpoint
           :temperature  ollama-buddy-openai-compat-temperature
           :max-tokens   ollama-buddy-openai-compat-max-tokens
           :default-model default-model
           :provider-name ollama-buddy-openai-compat-provider-name
           :token-count-var 'ollama-buddy-openai-compat--current-token-count))))

;;; Model registration helpers

(defun ollama-buddy-openai-compat--register (models)
  "Register MODELS (list of plain name strings) with the provider prefix."
  (when models
    (ollama-buddy-remote--register-models
     ollama-buddy-openai-compat-marker-prefix
     models
     #'ollama-buddy-openai-compat--send)))

;;; Model discovery

(defun ollama-buddy-openai-compat--fetch-models ()
  "Discover models from the server and register them.
Tries GET /v1/models; on failure falls back to
`ollama-buddy-openai-compat-models'."
  (if ollama-buddy-openai-compat-fetch-models
      (let* ((url (concat ollama-buddy-openai-compat-base-url "/v1/models"))
             (url-request-method "GET")
             (url-request-extra-headers
              (when (and ollama-buddy-openai-compat-api-key
                         (not (string-empty-p
                               ollama-buddy-openai-compat-api-key)))
                `(("Authorization" .
                   ,(concat "Bearer "
                            ollama-buddy-openai-compat-api-key))))))
        (url-retrieve
         url
         (lambda (status)
           (if (plist-get status :error)
               ;; Server unreachable or endpoint unsupported — use fallback
               (progn
                 (message "ollama-buddy-openai-compat: Could not fetch models \
from %s, using configured model list" ollama-buddy-openai-compat-base-url)
                 (ollama-buddy-openai-compat--register
                  ollama-buddy-openai-compat-models))
             (condition-case err
                 (progn
                   (goto-char (point-min))
                   (when (re-search-forward "\n\n" nil t)
                     (let* ((json-object-type 'alist)
                            (json-array-type 'vector)
                            (json-key-type 'symbol)
                            (raw (buffer-substring (point) (point-max)))
                            (response (json-read-from-string raw))
                            (data (alist-get 'data response))
                            (ids (mapcar (lambda (entry)
                                           (alist-get 'id entry))
                                         (append data nil))))
                       (if ids
                           (ollama-buddy-openai-compat--register ids)
                         ;; Empty data array — fall back
                         (ollama-buddy-openai-compat--register
                          ollama-buddy-openai-compat-models)))))
               (error
                (message "ollama-buddy-openai-compat: Error parsing model list: %s"
                         (error-message-string err))
                (ollama-buddy-openai-compat--register
                 ollama-buddy-openai-compat-models)))))
         nil t))
    ;; Fetch disabled — register configured list directly
    (ollama-buddy-openai-compat--register
     ollama-buddy-openai-compat-models)))

;;;###autoload
(defun ollama-buddy-openai-compat-refresh-models ()
  "Re-discover models from the server and update the model list.
Useful after changing `ollama-buddy-openai-compat-base-url' at runtime."
  (interactive)
  ;; Remove any previously registered models for this prefix so we don't
  ;; accumulate duplicates across refreshes.
  (setq ollama-buddy-remote-models
        (cl-remove-if
         (lambda (m)
           (string-prefix-p ollama-buddy-openai-compat-marker-prefix m))
         ollama-buddy-remote-models))
  (ollama-buddy-openai-compat--fetch-models)
  (message "ollama-buddy-openai-compat: refreshing model list from %s"
           ollama-buddy-openai-compat-base-url))

;;; Register with core provider tables

;; Add to the provider-labels table so model names are displayed correctly
;; in the completing-read UI and stripped properly elsewhere.
(add-to-list 'ollama-buddy--provider-labels
             (cons ollama-buddy-openai-compat-marker-prefix
                   ollama-buddy-openai-compat-provider-name))

;;; Initialise on load — deferred so loading this file never blocks Emacs

(run-with-idle-timer 0.5 nil #'ollama-buddy-openai-compat--fetch-models)

(provide 'ollama-buddy-openai-compat)
;;; ollama-buddy-openai-compat.el ends here
