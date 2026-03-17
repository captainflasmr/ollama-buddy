;;; ollama-buddy-grok.el --- Grok integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; This extension provides Grok integration for the ollama-buddy package.
;; It allows users to interact with Grok's language models using the same
;; interface as ollama-buddy, providing seamless switching between local Ollama
;; models and cloud-based Grok models.
;;
;; This is a thin wrapper around `ollama-buddy-provider-create'.  The
;; defcustom variables below are preserved for backward compatibility so
;; existing configurations continue to work.

;;; Code:

(require 'ollama-buddy-provider)

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
Get your key from the Grok API dashboard.
Consider using `auth-source' instead of setting this directly."
  :type 'string
  :risky t
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

;; Register via the generic provider system
(ollama-buddy-provider-create
 :name "Grok"
 :prefix ollama-buddy-grok-marker-prefix
 :api-key (lambda () ollama-buddy-grok-api-key)
 :endpoint ollama-buddy-grok-api-endpoint
 :default-model ollama-buddy-grok-default-model
 :temperature ollama-buddy-grok-temperature
 :max-tokens ollama-buddy-grok-max-tokens
 :models-endpoint "https://api.x.ai/v1/models")

(provide 'ollama-buddy-grok)
;;; ollama-buddy-grok.el ends here
