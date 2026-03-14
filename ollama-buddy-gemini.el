;;; ollama-buddy-gemini.el --- Google Gemini integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This extension provides Google Gemini integration for the ollama-buddy package.
;; It allows users to interact with Google's Gemini language models using the same
;; interface as ollama-buddy, providing seamless switching between local Ollama models
;; and cloud-based Gemini models.
;;
;; This is a thin wrapper around `ollama-buddy-provider-create'.  The
;; defcustom variables below are preserved for backward compatibility so
;; existing configurations continue to work.

;;; Code:

(require 'ollama-buddy-provider)

(defgroup ollama-buddy-gemini nil
  "Google Gemini integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-gemini-")

(defcustom ollama-buddy-gemini-marker-prefix "g:"
  "Prefix used to identify Gemini models in the ollama-buddy interface."
  :type 'string
  :group 'ollama-buddy-gemini)

(defcustom ollama-buddy-gemini-api-key ""
  "API key for accessing Google Gemini services.
Get your key from https://ai.google.dev/.
Consider using `auth-source' instead of setting this directly."
  :type 'string
  :risky t
  :group 'ollama-buddy-gemini)

(defcustom ollama-buddy-gemini-default-model "gemini-1.5-pro"
  "Default Gemini model to use."
  :type 'string
  :group 'ollama-buddy-gemini)

(defcustom ollama-buddy-gemini-api-endpoint
  "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent"
  "Endpoint format for Google Gemini API.
The %s will be replaced with the model name."
  :type 'string
  :group 'ollama-buddy-gemini)

(defcustom ollama-buddy-gemini-temperature 0.7
  "Temperature setting for Gemini requests (0.0-1.0).
Lower values make the output more deterministic, higher values more creative."
  :type 'float
  :group 'ollama-buddy-gemini)

(defcustom ollama-buddy-gemini-max-tokens nil
  "Maximum number of tokens to generate in the response.
Use nil for API default behavior (adaptive)."
  :type '(choice integer (const nil))
  :group 'ollama-buddy-gemini)

;; Register via the generic provider system
(ollama-buddy-provider-create
 :name "Gemini"
 :prefix ollama-buddy-gemini-marker-prefix
 :api-type 'gemini
 :api-key (lambda () ollama-buddy-gemini-api-key)
 :endpoint ollama-buddy-gemini-api-endpoint
 :default-model ollama-buddy-gemini-default-model
 :temperature ollama-buddy-gemini-temperature
 :max-tokens ollama-buddy-gemini-max-tokens
 :models-endpoint "https://generativelanguage.googleapis.com/v1/models"
 :models-filter (lambda (id) (string-match-p "gemini" id)))

(provide 'ollama-buddy-gemini)
;;; ollama-buddy-gemini.el ends here
