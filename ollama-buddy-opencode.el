;;; ollama-buddy-opencode.el --- OpenCode Go subscription integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; This extension integrates the OpenCode Go subscription with
;; ollama-buddy.  Note: OpenCode Go is the flat-rate subscription product
;; (the /go endpoints under opencode.ai), distinct from "OpenCode Zen"
;; which is OpenCode's pay-as-you-go API.  Only the subscription /go
;; endpoints are wired up here.
;;
;; A single OpenCode Go subscription exposes models behind two API
;; surfaces sharing one API key:
;;
;;   - https://opencode.ai/zen/go/v1/chat/completions  (OpenAI-compatible)
;;   - https://opencode.ai/zen/go/v1/messages          (Anthropic-compatible)
;;
;; (The /zen/ segment is part of OpenCode's URL routing — these are the
;; Go subscription endpoints, not the Zen pay-as-you-go API.)
;;
;; All 14 models are exposed under a single prefix (`n:' by default) and
;; one provider entry labelled \"OpenCode Go\".  Per-model dispatch sends
;; the request to the correct endpoint and uses the matching API shape.
;; Usage budget ($12/5h, $30/wk, $60/mo as of April 2026) is shared
;; across the whole subscription.
;;
;; Usage:
;;   (require 'ollama-buddy-opencode)
;;   ;; with auth-source (~/.authinfo):
;;   ;; machine ollama-buddy-opencode login apikey password <YOUR_KEY>
;;
;; NOTE: model identifiers below are best-effort based on the published
;; OpenCode Go documentation.  If the API rejects a name, adjust the
;; relevant `ollama-buddy-opencode-chat-models' or
;; `ollama-buddy-opencode-msg-models' entry.

;;; Code:

(require 'ollama-buddy-provider)

(defgroup ollama-buddy-opencode nil
  "OpenCode Go subscription integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-opencode-")

(defcustom ollama-buddy-opencode-api-key ""
  "API key for the OpenCode Go subscription.
A single key authorises both underlying endpoints.  Get one from the
OpenCode Go console.  Consider using `auth-source' instead of setting
this directly, e.g.:

  (setq ollama-buddy-opencode-api-key
        (auth-source-pick-first-password
         :host \"ollama-buddy-opencode\" :user \"apikey\"))"
  :type 'string
  :risky t
  :group 'ollama-buddy-opencode)

(defcustom ollama-buddy-opencode-marker-prefix "n:"
  "Prefix used to identify OpenCode Go models in the model list."
  :type 'string
  :group 'ollama-buddy-opencode)

(defcustom ollama-buddy-opencode-chat-endpoint
  "https://opencode.ai/zen/go/v1/chat/completions"
  "Endpoint for the OpenCode Go OpenAI-compatible chat API."
  :type 'string
  :group 'ollama-buddy-opencode)

(defcustom ollama-buddy-opencode-msg-endpoint
  "https://opencode.ai/zen/go/v1/messages"
  "Endpoint for the OpenCode Go Anthropic-compatible messages API."
  :type 'string
  :group 'ollama-buddy-opencode)

(defcustom ollama-buddy-opencode-chat-models
  '("glm-5.1"
    "glm-5"
    "kimi-k2.6"
    "kimi-k2.5"
    "mimo-v2.5-pro"
    "mimo-v2.5"
    "mimo-v2-pro"
    "mimo-v2-omni"
    "qwen3.6-plus"
    "qwen3.5-plus")
  "OpenCode Go models served by the OpenAI-compatible chat endpoint.
Adjust if OpenCode adds/renames models — there is no public model
discovery endpoint at the time of writing."
  :type '(repeat string)
  :group 'ollama-buddy-opencode)

(defcustom ollama-buddy-opencode-msg-models
  '("deepseek-v4-pro"
    "deepseek-v4-flash"
    "minimax-m2.7"
    "minimax-m2.5")
  "OpenCode Go models served by the Anthropic-compatible messages endpoint."
  :type '(repeat string)
  :group 'ollama-buddy-opencode)

(defcustom ollama-buddy-opencode-default-model "kimi-k2.6"
  "Default model when invoking OpenCode Go without an explicit selection."
  :type 'string
  :group 'ollama-buddy-opencode)

(defcustom ollama-buddy-opencode-temperature 0.7
  "Temperature setting for OpenCode Go requests (0.0-2.0)."
  :type 'float
  :group 'ollama-buddy-opencode)

(defcustom ollama-buddy-opencode-max-tokens nil
  "Maximum tokens to generate, or nil for the API default.
Note: the Anthropic-shape endpoint requires a max_tokens; the provider
layer falls back to 4096 when this is nil."
  :type '(choice integer (const nil))
  :group 'ollama-buddy-opencode)

(defun ollama-buddy-opencode--key ()
  "Return the OpenCode Go API key.
Used as the `:api-key' thunk so a single `auth-source' update is
reflected immediately."
  ollama-buddy-opencode-api-key)

(defun ollama-buddy-opencode--bare-name (model)
  "Strip the configured marker prefix from MODEL and return the bare name."
  (let ((p ollama-buddy-opencode-marker-prefix))
    (if (and model (stringp model) (string-prefix-p p model))
        (substring model (length p))
      (or model ""))))

(defun ollama-buddy-opencode--shape (model)
  "Return `chat' or `msg' for MODEL based on the configured model lists.
Falls back to `chat' for unknown models — the OpenAI-compatible
endpoint is the more common surface and a clearer error from the API
than a silent shape mismatch."
  (let ((bare (ollama-buddy-opencode--bare-name model)))
    (if (member bare ollama-buddy-opencode-msg-models) 'msg 'chat)))

(defun ollama-buddy-opencode--api-type (model)
  "Return the api-type symbol (`openai' or `claude') for MODEL."
  (pcase (ollama-buddy-opencode--shape model)
    ('msg 'claude)
    (_    'openai)))

(defun ollama-buddy-opencode--endpoint (model)
  "Return the endpoint URL for MODEL."
  (pcase (ollama-buddy-opencode--shape model)
    ('msg ollama-buddy-opencode-msg-endpoint)
    (_    ollama-buddy-opencode-chat-endpoint)))

(ollama-buddy-provider-create
 :name "OpenCode Go"
 :prefix ollama-buddy-opencode-marker-prefix
 :api-type #'ollama-buddy-opencode--api-type
 :api-key #'ollama-buddy-opencode--key
 :endpoint #'ollama-buddy-opencode--endpoint
 :default-model ollama-buddy-opencode-default-model
 :temperature ollama-buddy-opencode-temperature
 :max-tokens ollama-buddy-opencode-max-tokens
 :models (append ollama-buddy-opencode-chat-models
                 ollama-buddy-opencode-msg-models))

(provide 'ollama-buddy-opencode)
;;; ollama-buddy-opencode.el ends here
