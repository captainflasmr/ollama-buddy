;;; ollama-buddy-openai-compat.el --- Generic OpenAI-compatible provider for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "29.1"))

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
;;   - Ollama's own OpenAI-compat layer (http://localhost:11434)
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
;;
;; This is a thin wrapper around `ollama-buddy-provider-create'.  The
;; defcustom variables below are preserved for backward compatibility so
;; existing configurations continue to work.

;;; Code:

(require 'ollama-buddy-provider)

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
Leave empty for local servers that do not require authentication.
Consider using `auth-source' instead of setting this directly."
  :type 'string
  :risky t
  :group 'ollama-buddy-openai-compat)

(defcustom ollama-buddy-openai-compat-models nil
  "Fallback list of model names when automatic discovery fails.
Each entry is a plain model name without the provider prefix,
e.g. \"llama-3.2-3b-instruct\" or \"mistral-7b-instruct\"."
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
  "Temperature for requests (0.0-2.0).
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

;;; Register via the generic provider system

(ollama-buddy-provider-create
 :name ollama-buddy-openai-compat-provider-name
 :prefix ollama-buddy-openai-compat-marker-prefix
 :api-key (lambda ()
            (let ((key ollama-buddy-openai-compat-api-key))
              (if (and key (not (string-empty-p key))) key nil)))
 :endpoint (concat ollama-buddy-openai-compat-base-url "/v1/chat/completions")
 :models-endpoint (when ollama-buddy-openai-compat-fetch-models
                    (concat ollama-buddy-openai-compat-base-url "/v1/models"))
 :models ollama-buddy-openai-compat-models
 :default-model (unless (string-empty-p ollama-buddy-openai-compat-default-model)
                  ollama-buddy-openai-compat-default-model)
 :temperature ollama-buddy-openai-compat-temperature
 :max-tokens ollama-buddy-openai-compat-max-tokens)

;;;###autoload
(defun ollama-buddy-openai-compat-refresh-models ()
  "Re-discover models from the server and update the model list.
Useful after changing `ollama-buddy-openai-compat-base-url' at runtime."
  (interactive)
  ;; Re-register with updated URLs
  (ollama-buddy-provider-remove ollama-buddy-openai-compat-marker-prefix)
  (ollama-buddy-provider-create
   :name ollama-buddy-openai-compat-provider-name
   :prefix ollama-buddy-openai-compat-marker-prefix
   :api-key (lambda ()
              (let ((key ollama-buddy-openai-compat-api-key))
                (if (and key (not (string-empty-p key))) key nil)))
   :endpoint (concat ollama-buddy-openai-compat-base-url "/v1/chat/completions")
   :models-endpoint (when ollama-buddy-openai-compat-fetch-models
                      (concat ollama-buddy-openai-compat-base-url "/v1/models"))
   :models ollama-buddy-openai-compat-models
   :default-model (unless (string-empty-p ollama-buddy-openai-compat-default-model)
                    ollama-buddy-openai-compat-default-model)
   :temperature ollama-buddy-openai-compat-temperature
   :max-tokens ollama-buddy-openai-compat-max-tokens)
  (message "ollama-buddy-openai-compat: refreshing model list from %s"
           ollama-buddy-openai-compat-base-url))

(provide 'ollama-buddy-openai-compat)
;;; ollama-buddy-openai-compat.el ends here
