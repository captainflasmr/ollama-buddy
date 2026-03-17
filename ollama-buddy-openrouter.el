;;; ollama-buddy-openrouter.el --- OpenRouter integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; This extension provides OpenRouter integration for the ollama-buddy package.
;; OpenRouter is a unified API that provides access to 400+ models from all
;; major providers (OpenAI, Anthropic, Google, Meta, Mistral, etc.) through a
;; single OpenAI-compatible endpoint.
;;
;; This is a thin wrapper around `ollama-buddy-provider-create'.  The
;; defcustom variables below are preserved for backward compatibility so
;; existing configurations continue to work.
;;
;; Usage:
;; (require 'ollama-buddy-openrouter)
;; (setq ollama-buddy-openrouter-api-key (getenv "OPENROUTER_API_KEY"))

;;; Code:

(require 'ollama-buddy-provider)

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
Get your key from https://openrouter.ai/keys.
Consider using `auth-source' instead of setting this directly."
  :type 'string
  :risky t
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

;; Register via the generic provider system
(ollama-buddy-provider-create
 :name "OpenRouter"
 :prefix ollama-buddy-openrouter-marker-prefix
 :api-key (lambda () ollama-buddy-openrouter-api-key)
 :endpoint ollama-buddy-openrouter-api-endpoint
 :default-model ollama-buddy-openrouter-default-model
 :temperature ollama-buddy-openrouter-temperature
 :max-tokens ollama-buddy-openrouter-max-tokens
 :models-endpoint "https://openrouter.ai/api/v1/models"
 :models-filter (lambda (id)
                  (or (null ollama-buddy-openrouter-model-filter)
                      (string-match-p ollama-buddy-openrouter-model-filter id)))
 :extra-headers '(("HTTP-Referer" . "https://github.com/captainflasmr/ollama-buddy")
                  ("X-Title" . "ollama-buddy")))

(provide 'ollama-buddy-openrouter)
;;; ollama-buddy-openrouter.el ends here
