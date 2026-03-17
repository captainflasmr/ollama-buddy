;;; ollama-buddy-deepseek.el --- DeepSeek integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; This extension provides DeepSeek integration for the ollama-buddy package.
;; It allows users to interact with DeepSeek's language models using the same
;; interface as ollama-buddy, providing seamless switching between local Ollama
;; models and cloud-based DeepSeek models.
;;
;; This is a thin wrapper around `ollama-buddy-provider-create'.  The
;; defcustom variables below are preserved for backward compatibility so
;; existing configurations continue to work.
;;
;; Usage:
;; (require 'ollama-buddy-deepseek)
;; (setq ollama-buddy-deepseek-api-key (getenv "DEEPSEEK_API_KEY"))

;;; Code:

(require 'ollama-buddy-provider)

(defgroup ollama-buddy-deepseek nil
  "DeepSeek integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-deepseek-")

(defcustom ollama-buddy-deepseek-marker-prefix "d:"
  "Prefix used to identify DeepSeek models in the ollama-buddy interface."
  :type 'string
  :group 'ollama-buddy-deepseek)

(defcustom ollama-buddy-deepseek-api-key ""
  "API key for accessing DeepSeek services.
Get your key from https://platform.deepseek.com/api_keys.
Consider using `auth-source' instead of setting this directly."
  :type 'string
  :risky t
  :group 'ollama-buddy-deepseek)

(defcustom ollama-buddy-deepseek-default-model "deepseek-chat"
  "Default DeepSeek model to use."
  :type 'string
  :group 'ollama-buddy-deepseek)

(defcustom ollama-buddy-deepseek-api-endpoint "https://api.deepseek.com/chat/completions"
  "Endpoint for DeepSeek chat completions API."
  :type 'string
  :group 'ollama-buddy-deepseek)

(defcustom ollama-buddy-deepseek-temperature 0.7
  "Temperature setting for DeepSeek requests (0.0-2.0).
Lower values make the output more deterministic, higher values more creative."
  :type 'float
  :group 'ollama-buddy-deepseek)

(defcustom ollama-buddy-deepseek-max-tokens nil
  "Maximum number of tokens to generate in the response.
Use nil for API default behavior (adaptive)."
  :type '(choice integer (const nil))
  :group 'ollama-buddy-deepseek)

;; Register via the generic provider system
(ollama-buddy-provider-create
 :name "DeepSeek"
 :prefix ollama-buddy-deepseek-marker-prefix
 :api-key (lambda () ollama-buddy-deepseek-api-key)
 :endpoint ollama-buddy-deepseek-api-endpoint
 :default-model ollama-buddy-deepseek-default-model
 :temperature ollama-buddy-deepseek-temperature
 :max-tokens ollama-buddy-deepseek-max-tokens
 :models '("deepseek-chat" "deepseek-reasoner"))

(provide 'ollama-buddy-deepseek)
;;; ollama-buddy-deepseek.el ends here
