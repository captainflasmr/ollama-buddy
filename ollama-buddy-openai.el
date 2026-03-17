;;; ollama-buddy-openai.el --- OpenAI integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; This extension provides OpenAI (ChatGPT) integration for the ollama-buddy package.
;; It allows users to interact with OpenAI's language models using the same interface
;; as ollama-buddy, providing seamless switching between local Ollama models and
;; cloud-based OpenAI models.
;;
;; This is a thin wrapper around `ollama-buddy-provider-create'.  The
;; defcustom variables below are preserved for backward compatibility so
;; existing configurations continue to work.

;;; Code:

(require 'ollama-buddy-provider)

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
Get your key from https://platform.openai.com/api-keys.
Consider using `auth-source' instead of setting this directly."
  :type 'string
  :risky t
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

;; Register via the generic provider system
(ollama-buddy-provider-create
 :name "OpenAI"
 :prefix ollama-buddy-openai-marker-prefix
 :api-key (lambda () ollama-buddy-openai-api-key)
 :endpoint ollama-buddy-openai-api-endpoint
 :default-model ollama-buddy-openai-default-model
 :temperature ollama-buddy-openai-temperature
 :max-tokens ollama-buddy-openai-max-tokens
 :models-endpoint "https://api.openai.com/v1/models"
 :models-filter (lambda (id) (string-match-p "\\(gpt\\|claude\\)" id)))

(provide 'ollama-buddy-openai)
;;; ollama-buddy-openai.el ends here
