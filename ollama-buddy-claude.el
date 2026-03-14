;;; ollama-buddy-claude.el --- Anthropic Claude integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This extension provides Anthropic Claude integration for the ollama-buddy package.
;; It allows users to interact with Anthropic's Claude language models using the same
;; interface as ollama-buddy, providing seamless switching between local Ollama models
;; and cloud-based Claude models.
;;
;; This is a thin wrapper around `ollama-buddy-provider-create'.  The
;; defcustom variables below are preserved for backward compatibility so
;; existing configurations continue to work.

;;; Code:

(require 'ollama-buddy-provider)

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
Get your key from https://console.anthropic.com/.
Consider using `auth-source' instead of setting this directly."
  :type 'string
  :risky t
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

;; Register via the generic provider system
(ollama-buddy-provider-create
 :name "Claude"
 :prefix ollama-buddy-claude-marker-prefix
 :api-type 'claude
 :api-key (lambda () ollama-buddy-claude-api-key)
 :endpoint ollama-buddy-claude-api-endpoint
 :default-model ollama-buddy-claude-default-model
 :temperature ollama-buddy-claude-temperature
 :max-tokens ollama-buddy-claude-max-tokens
 :models-endpoint "https://api.anthropic.com/v1/models")

(provide 'ollama-buddy-claude)
;;; ollama-buddy-claude.el ends here
