;;; ollama-buddy-deepseek.el --- DeepSeek integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This extension provides DeepSeek integration for the ollama-buddy package.
;; It allows users to interact with DeepSeek's language models using the same
;; interface as ollama-buddy, providing seamless switching between local Ollama
;; models and cloud-based DeepSeek models.
;;
;; Usage:
;; (require 'ollama-buddy-deepseek)
;; (setq ollama-buddy-deepseek-api-key (getenv "DEEPSEEK_API_KEY"))

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'ollama-buddy-core)
(require 'ollama-buddy-remote)

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
Get your key from https://platform.deepseek.com/api_keys."
  :type 'string
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

;; Internal variables

(defvar ollama-buddy-deepseek--current-token-count 0
  "Counter for tokens in the current DeepSeek response.")

;; API interaction functions

(defun ollama-buddy-deepseek--send (prompt &optional model)
  "Send PROMPT to DeepSeek's API using MODEL or default model asynchronously."
  (when (ollama-buddy-remote--verify-api-key
         ollama-buddy-deepseek-api-key
         'ollama-buddy-deepseek-api-key
         "DeepSeek")
    (ollama-buddy-remote--openai-send
     prompt model
     (list :prefix ollama-buddy-deepseek-marker-prefix
           :api-key ollama-buddy-deepseek-api-key
           :endpoint ollama-buddy-deepseek-api-endpoint
           :temperature ollama-buddy-deepseek-temperature
           :max-tokens ollama-buddy-deepseek-max-tokens
           :default-model ollama-buddy-deepseek-default-model
           :provider-name "DeepSeek"
           :token-count-var 'ollama-buddy-deepseek--current-token-count))))

(defun ollama-buddy-deepseek--fetch-models ()
  "Register available DeepSeek models."
  (ollama-buddy-remote--register-models
   ollama-buddy-deepseek-marker-prefix
   '("deepseek-chat" "deepseek-reasoner")
   #'ollama-buddy-deepseek--send))

(ollama-buddy-deepseek--fetch-models)

(provide 'ollama-buddy-deepseek)
;;; ollama-buddy-deepseek.el ends here
