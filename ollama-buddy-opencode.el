;;; ollama-buddy-opencode.el --- OpenCode Go subscription integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy

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
;; Models are exposed under a single prefix (`n:' by default) and one
;; provider entry labelled \"OpenCode Go\".  Per-model dispatch sends the
;; request to the correct endpoint and uses the matching API shape.
;; Usage budget ($12/5h, $30/wk, $60/mo as of April 2026) is shared
;; across the whole subscription.
;;
;; Model discovery: the Go surface exposes an OpenAI-style listing at
;; https://opencode.ai/zen/go/v1/models, so the model list is fetched
;; dynamically (see `ollama-buddy-opencode-models-endpoint'), falling back
;; to the static `ollama-buddy-opencode-chat-models' list when discovery
;; is unavailable.
;;
;; All OpenCode Go models are OpenAI-native, so by default every model is
;; routed to the chat/completions endpoint.  The Anthropic-compatible
;; /messages endpoint mis-proxies these models (it drops the request body
;; and the upstream reports \"Empty input messages\"), so it is opt-in only
;; via `ollama-buddy-opencode-msg-models' /
;; `ollama-buddy-opencode-msg-model-families'.
;;
;; Requests carry an `x-opencode-session' header with a stable per-session
;; identifier (plus an identifying User-Agent).  OpenCode Go uses this to
;; route requests and optimise prompt caching; without it the API rejects
;; the request with "Request is missing x-opencode-session".
;;
;; Usage:
;;   (require 'ollama-buddy-opencode)
;;   ;; with auth-source (~/.authinfo):
;;   ;; machine ollama-buddy-opencode login apikey password <YOUR_KEY>

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

(defcustom ollama-buddy-opencode-models-endpoint
  "https://opencode.ai/zen/go/v1/models"
  "OpenAI-style model-discovery endpoint for OpenCode Go.
When non-nil, the model list is fetched from here at registration time
instead of using the static `ollama-buddy-opencode-chat-models' /
`ollama-buddy-opencode-msg-models' lists (which become the fallback if
discovery fails).  Set to nil to disable discovery and use only the
static lists."
  :type '(choice (const :tag "Disable discovery" nil) string)
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
    "qwen3.5-plus"
    "deepseek-v4-pro"
    "deepseek-v4-flash"
    "minimax-m2.7"
    "minimax-m2.5")
  "OpenCode Go models served by the OpenAI-compatible chat endpoint.
Used as the offline fallback when `ollama-buddy-opencode-models-endpoint'
discovery is unavailable; otherwise the live model list takes precedence.
Every OpenCode Go model is OpenAI-native, so the chat endpoint is the
correct surface for all of them by default."
  :type '(repeat string)
  :group 'ollama-buddy-opencode)

(defcustom ollama-buddy-opencode-msg-models nil
  "OpenCode Go models forced onto the Anthropic-compatible messages endpoint.
Empty by default: every OpenCode Go model is OpenAI-native and works on
the chat endpoint, while the messages endpoint mis-proxies OpenAI-native
models (it drops the request body, yielding \"Empty input messages\").
This is an explicit per-model override consulted by
`ollama-buddy-opencode--shape'; only add a model here if OpenCode later
serves one that genuinely requires the Anthropic shape."
  :type '(repeat string)
  :group 'ollama-buddy-opencode)

(defcustom ollama-buddy-opencode-msg-model-families nil
  "Model-name prefixes routed to the Anthropic-compatible messages API.
Empty by default (see `ollama-buddy-opencode-msg-models' for why).  When
non-nil, a discovered model whose bare name begins with one of these
prefixes is routed to the messages endpoint instead of the chat endpoint."
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

(defcustom ollama-buddy-opencode-usage-url ""
  "URL to fetch OpenCode Go usage stats.
This is your unique workspace Go URL, e.g.:
https://opencode.ai/workspace/wrk_.../go"
  :type 'string
  :group 'ollama-buddy-opencode)

(defcustom ollama-buddy-opencode-session-token ""
  "Session token for fetching OpenCode Go usage stats.
This is the value of the `auth' cookie from opencode.ai.
To obtain it: sign in at https://opencode.ai, open browser DevTools (F12),
go to Application > Cookies > opencode.ai, and copy the `auth' value."
  :type 'string
  :group 'ollama-buddy-opencode)

(defcustom ollama-buddy-opencode-session-id nil
  "Stable session identifier sent in the `x-opencode-session' header.
OpenCode Go uses this ID to route requests and optimise prompt caching,
so it must stay stable for the lifetime of a conversation.  When nil, a
random ID is generated once per Emacs session.  Set this to a string to
use a fixed value, or call `ollama-buddy-opencode-new-session' to rotate
the generated ID."
  :type '(choice (const :tag "Generate automatically" nil) string)
  :group 'ollama-buddy-opencode)

(defcustom ollama-buddy-opencode-user-agent "ollama-buddy"
  "User-Agent header sent with OpenCode Go requests.
OpenCode asks clients to identify themselves with their own user agent
rather than a generic HTTP-library name.  A version suffix such as
\"ollama-buddy/7.6.2\" is also fine."
  :type 'string
  :group 'ollama-buddy-opencode)

(defvar ollama-buddy-opencode--generated-session-id nil
  "Automatically generated OpenCode Go session ID.")

(defun ollama-buddy-opencode--new-session-id ()
  "Return a random UUID-shaped session identifier."
  (let* ((hex (secure-hash 'md5
                           (format "%s-%s-%s-%s"
                                   (float-time)
                                   (emacs-pid)
                                   (random most-positive-fixnum)
                                   (random most-positive-fixnum))))
         (variant (logand (string-to-number (substring hex 16 17) 16) 3)))
    (format "%s-%s-4%s-%s%s-%s"
            (substring hex 0 8)
            (substring hex 8 12)
            (substring hex 13 16)
            (substring "89ab" variant (1+ variant))
            (substring hex 17 20)
            (substring hex 20 32))))

(defun ollama-buddy-opencode--session-id ()
  "Return the session ID used for the `x-opencode-session' header."
  (or (and (stringp ollama-buddy-opencode-session-id)
           (not (string-empty-p ollama-buddy-opencode-session-id))
           ollama-buddy-opencode-session-id)
      (or ollama-buddy-opencode--generated-session-id
          (setq ollama-buddy-opencode--generated-session-id
                (ollama-buddy-opencode--new-session-id)))))

(defun ollama-buddy-opencode-new-session ()
  "Generate a new OpenCode Go routing session ID.
OpenCode uses the ID sent in the `x-opencode-session' header to route
requests and optimise prompt caching.  Call this when you want the next
conversation to start with a fresh ID."
  (interactive)
  (setq ollama-buddy-opencode--generated-session-id
        (ollama-buddy-opencode--new-session-id))
  (message "OpenCode Go session ID: %s"
           ollama-buddy-opencode--generated-session-id))

(defun ollama-buddy-opencode--headers ()
  "Return extra HTTP headers for OpenCode Go requests.
OpenCode requires a stable `x-opencode-session' value and asks clients
to identify themselves with their own User-Agent."
  `(("x-opencode-session" . ,(ollama-buddy-opencode--session-id))
    ("User-Agent" . ,ollama-buddy-opencode-user-agent)))

(defvar ollama-buddy-opencode--usage-cache nil
  "Cached OpenCode usage data.")

(defvar ollama-buddy-opencode--usage-cache-time nil
  "Time when OpenCode usage was last fetched.")

(defun ollama-buddy-opencode--fetch-usage ()
  "Fetch OpenCode Go usage stats.
Returns an alist with session, weekly, and monthly percentages."
  (when (and (not (string-empty-p ollama-buddy-opencode-usage-url))
             (not (string-empty-p ollama-buddy-opencode-session-token)))
    ;; Return cached value if still fresh (5 mins)
    (if (and ollama-buddy-opencode--usage-cache
             ollama-buddy-opencode--usage-cache-time
             (< (float-time (time-subtract (current-time)
                                           ollama-buddy-opencode--usage-cache-time))
                300))
        ollama-buddy-opencode--usage-cache
      ;; Fetch fresh data
      (condition-case err
          (let ((buf (generate-new-buffer " *opencode-usage*")))
            (unwind-protect
                (let ((exit-code
                       (call-process
                        ollama-buddy-curl-executable nil buf nil
                        "-s"
                        "-b" (concat "auth=" ollama-buddy-opencode-session-token)
                        ollama-buddy-opencode-usage-url)))
                  (when (zerop exit-code)
                    (with-current-buffer buf
                      (goto-char (point-min))
                      ;; Find the script tag containing the JSON data
                      (when (re-search-forward "rollingUsage:[^}]*usagePercent:\\([0-9]+\\)" nil t)
                        (let ((rolling (match-string 1))
                              weekly monthly)
                          (when (re-search-forward "weeklyUsage:[^}]*usagePercent:\\([0-9]+\\)" nil t)
                            (setq weekly (match-string 1)))
                          (when (re-search-forward "monthlyUsage:[^}]*usagePercent:\\([0-9]+\\)" nil t)
                            (setq monthly (match-string 1)))
                          (let ((result `((session . ,(concat rolling "%"))
                                          (weekly . ,(concat weekly "%"))
                                          (monthly . ,(concat monthly "%")))))
                            (setq ollama-buddy-opencode--usage-cache result
                                  ollama-buddy-opencode--usage-cache-time (current-time))
                            result))))))
              (kill-buffer buf)))
        (error
         (message "Failed to fetch OpenCode usage: %s" (error-message-string err))
         nil)))))

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
  "Return `chat' or `msg' for MODEL.
A model takes the Anthropic-compatible `msg' shape when its bare name
matches one of `ollama-buddy-opencode-msg-model-families' or is listed
explicitly in `ollama-buddy-opencode-msg-models'.  Otherwise it falls
back to `chat' — the OpenAI-compatible endpoint is the more common
surface and yields a clearer error from the API than a silent shape
mismatch."
  (let ((bare (ollama-buddy-opencode--bare-name model)))
    (if (or (member bare ollama-buddy-opencode-msg-models)
            (seq-some (lambda (family) (string-prefix-p family bare))
                      ollama-buddy-opencode-msg-model-families))
        'msg
      'chat)))

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
 :models-endpoint ollama-buddy-opencode-models-endpoint
 :extra-headers #'ollama-buddy-opencode--headers
 ;; Static fallback when discovery is unavailable.
 :models (append ollama-buddy-opencode-chat-models
                 ollama-buddy-opencode-msg-models))

(provide 'ollama-buddy-opencode)
;;; ollama-buddy-opencode.el ends here
