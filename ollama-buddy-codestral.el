;;; ollama-buddy-codestral.el --- Mistral Codestral integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This extension provides Mistral Codestral integration for the ollama-buddy package.
;; It allows users to interact with Mistral's Codestral language models using the same interface
;; as ollama-buddy, providing seamless switching between local Ollama models and
;; cloud-based Mistral Codestral model.

;;; Usage:
;;
;; (require 'ollama-buddy-codestral)
;; (setq ollama-buddy-codestral-api-key (getenv "CODESTRAL_API_KEY"))

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'ollama-buddy-core)
(require 'ollama-buddy-remote)

(defgroup ollama-buddy-codestral nil
  "Mistral Codestral integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-codestral-")

(defcustom ollama-buddy-codestral-marker-prefix "s:"
  "Prefix to indicate that a model is from Mistral Codestral rather than Ollama."
  :type 'string
  :group 'ollama-buddy-codestral)

(defcustom ollama-buddy-codestral-api-key ""
  "API key for accessing Mistral Codestral services."
  :type 'string
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

;; Internal variables

(defvar ollama-buddy-codestral--current-token-count 0
  "Counter for tokens in the current Mistral Codestral response.")

;; API interaction functions

(defun ollama-buddy-codestral--send (prompt &optional model)
  "Send PROMPT to Codestral API using MODEL asynchronously."
  (when (ollama-buddy-remote--verify-api-key
         ollama-buddy-codestral-api-key
         'ollama-buddy-codestral-api-key
         "Mistral Codestral")
    (ollama-buddy-remote--openai-send
     prompt model
     (list :prefix ollama-buddy-codestral-marker-prefix
           :api-key ollama-buddy-codestral-api-key
           :endpoint ollama-buddy-codestral-api-endpoint
           :temperature ollama-buddy-codestral-temperature
           :max-tokens ollama-buddy-codestral-max-tokens
           :default-model ollama-buddy-codestral-default-model
           :provider-name "Mistral Codestral"
           :token-count-var 'ollama-buddy-codestral--current-token-count))))

(defun ollama-buddy-codestral--fetch-models ()
  "Fetch available Codestral models and register them."
  (ollama-buddy-remote--register-models
   ollama-buddy-codestral-marker-prefix
   '("codestral-latest")
   #'ollama-buddy-codestral--send))

(ollama-buddy-codestral--fetch-models)

(provide 'ollama-buddy-codestral)
;;; ollama-buddy-codestral.el ends here
