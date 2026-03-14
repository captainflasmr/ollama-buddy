;;; ollama-buddy-codestral.el --- Mistral Codestral integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This extension provides Mistral Codestral integration for the ollama-buddy package.
;; It allows users to interact with Mistral's Codestral language models using the same
;; interface as ollama-buddy, providing seamless switching between local Ollama models
;; and cloud-based Mistral Codestral model.
;;
;; This is a thin wrapper around `ollama-buddy-provider-create'.  The
;; defcustom variables below are preserved for backward compatibility so
;; existing configurations continue to work.

;;; Usage:
;;
;; (require 'ollama-buddy-codestral)
;; (setq ollama-buddy-codestral-api-key (getenv "CODESTRAL_API_KEY"))

;;; Code:

(require 'ollama-buddy-provider)

(defgroup ollama-buddy-codestral nil
  "Mistral Codestral integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-codestral-")

(defcustom ollama-buddy-codestral-marker-prefix "s:"
  "Prefix to indicate that a model is from Mistral Codestral rather than Ollama."
  :type 'string
  :group 'ollama-buddy-codestral)

(defcustom ollama-buddy-codestral-api-key ""
  "API key for accessing Mistral Codestral services.
Consider using `auth-source' instead of setting this directly."
  :type 'string
  :risky t
  :group 'ollama-buddy-codestral)

(defcustom ollama-buddy-codestral-default-model "codestral-latest"
  "Default Mistral Codestral model to use."
  :type 'string
  :group 'ollama-buddy-codestral)

(defcustom ollama-buddy-codestral-api-endpoint "https://api.mistral.ai/v1/chat/completions"
  "Endpoint for Mistral Codestral API."
  :type 'string
  :group 'ollama-buddy-codestral)

(defcustom ollama-buddy-codestral-temperature 0.7
  "Temperature setting for Mistral Codestral requests (0.0-2.0).
Lower values make the output more deterministic, higher values more creative."
  :type 'float
  :group 'ollama-buddy-codestral)

(defcustom ollama-buddy-codestral-max-tokens nil
  "Maximum number of tokens to generate in the response.
Use nil for API default behavior (adaptive)."
  :type '(choice integer (const nil))
  :group 'ollama-buddy-codestral)

;; Register via the generic provider system
(ollama-buddy-provider-create
 :name "Codestral"
 :prefix ollama-buddy-codestral-marker-prefix
 :api-key (lambda () ollama-buddy-codestral-api-key)
 :endpoint ollama-buddy-codestral-api-endpoint
 :default-model ollama-buddy-codestral-default-model
 :temperature ollama-buddy-codestral-temperature
 :max-tokens ollama-buddy-codestral-max-tokens
 :models '("codestral-latest"))

(provide 'ollama-buddy-codestral)
;;; ollama-buddy-codestral.el ends here
