;;; ollama-buddy-provider.el --- Generic provider registration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; This module provides a generic provider registration system for
;; ollama-buddy.  Instead of requiring a separate elisp file per LLM
;; provider, users can register any provider with a single
;; `ollama-buddy-provider-create' call — similar to gptel's
;; `gptel-make-openai' pattern.
;;
;; Three API types are supported:
;;
;;   `openai'  (default) — Any OpenAI-compatible chat/completions API
;;   `claude'  — Anthropic Claude Messages API
;;   `gemini'  — Google Gemini generateContent API
;;
;; Usage examples:
;;
;;   (require 'ollama-buddy-provider)
;;
;;   ;; OpenAI-compatible provider with static models
;;   (ollama-buddy-provider-create
;;    :name "DeepSeek" :prefix "d:"
;;    :api-key (getenv "DEEPSEEK_API_KEY")
;;    :endpoint "https://api.deepseek.com/chat/completions"
;;    :models '("deepseek-chat" "deepseek-reasoner"))
;;
;;   ;; OpenAI with dynamic model discovery + filter
;;   (ollama-buddy-provider-create
;;    :name "OpenAI" :prefix "a:"
;;    :api-key (lambda () (auth-source-pick-first-password
;;                         :host "openai" :user "apikey"))
;;    :endpoint "https://api.openai.com/v1/chat/completions"
;;    :models-endpoint "https://api.openai.com/v1/models"
;;    :models-filter (lambda (id) (string-match-p "gpt" id)))
;;
;;   ;; Anthropic Claude
;;   (ollama-buddy-provider-create
;;    :name "Claude" :prefix "c:" :api-type 'claude
;;    :api-key (lambda () (auth-source-pick-first-password
;;                         :host "anthropic" :user "apikey"))
;;    :endpoint "https://api.anthropic.com/v1/messages"
;;    :models-endpoint "https://api.anthropic.com/v1/models")
;;
;;   ;; Google Gemini
;;   (ollama-buddy-provider-create
;;    :name "Gemini" :prefix "g:" :api-type 'gemini
;;    :api-key (lambda () (auth-source-pick-first-password
;;                         :host "gemini" :user "apikey"))
;;    :endpoint "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent"
;;    :models-endpoint "https://generativelanguage.googleapis.com/v1/models")
;;
;;   ;; Local OpenAI-compatible server (no API key needed)
;;   (ollama-buddy-provider-create
;;    :name "LM Studio" :prefix "l:"
;;    :endpoint "http://localhost:1234/v1/chat/completions"
;;    :models-endpoint "http://localhost:1234/v1/models")
;;
;;   ;; Grok
;;   (ollama-buddy-provider-create
;;    :name "Grok" :prefix "k:"
;;    :api-key (getenv "GROK_API_KEY")
;;    :endpoint "https://api.x.ai/v1/chat/completions"
;;    :models-endpoint "https://api.x.ai/v1/models")
;;
;;   ;; OpenRouter
;;   (ollama-buddy-provider-create
;;    :name "OpenRouter" :prefix "r:"
;;    :api-key (getenv "OPENROUTER_API_KEY")
;;    :endpoint "https://openrouter.ai/api/v1/chat/completions"
;;    :models-endpoint "https://openrouter.ai/api/v1/models")
;;
;; The :api-key parameter accepts a string, a function returning a
;; string (useful for lazy auth-source evaluation), or nil for local
;; servers that do not require authentication.
;;
;; This module coexists with the individual provider files
;; (ollama-buddy-openai.el, ollama-buddy-claude.el, etc.).  Do not
;; load both for the same prefix — use one approach or the other.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'ollama-buddy-core)
(require 'ollama-buddy-remote)

;; Tool module forward declarations
(declare-function ollama-buddy-tools--generate-schema "ollama-buddy-tools")

;;; Provider data structure
;; ============================================================================

(cl-defstruct (ollama-buddy-provider
               (:constructor ollama-buddy-provider--make)
               (:copier nil))
  "A registered LLM provider."
  (name "" :type string
        :documentation "Display name shown in status line and chat headings.")
  (prefix "" :type string
          :documentation "Model prefix (e.g., \"d:\").")
  (api-key nil
           :documentation "API key: string, function returning string, or nil.")
  (api-type 'openai :type symbol
            :documentation "One of: openai, claude, gemini.")
  (endpoint "" :type string
            :documentation "API endpoint URL.  For Gemini, include %s for model name.")
  (models-endpoint nil
                   :documentation "URL for dynamic model discovery, or nil.")
  (models nil :type list
          :documentation "Static model name list (without prefix).")
  (models-filter nil
                 :documentation "Predicate to filter discovered model IDs.")
  (default-model nil
                 :documentation "Default model name (without prefix).")
  (temperature 0.7 :type float)
  (max-tokens nil
              :documentation "Max tokens integer or nil for API default.")
  (extra-headers nil :type list
                 :documentation "Additional HTTP headers alist.")
  (token-count-sym nil
                   :documentation "Interned symbol for token count storage."))

;;; Registry
;; ============================================================================

(defvar ollama-buddy-provider--registry (make-hash-table :test 'equal)
  "Hash table mapping prefix string to `ollama-buddy-provider' struct.")

;;; Key resolution
;; ============================================================================

(defun ollama-buddy-provider--resolve-key (provider)
  "Return the API key string for PROVIDER.
If the stored key is a function, call it.  Returns \"\" when nil."
  (let ((key (ollama-buddy-provider-api-key provider)))
    (cond
     ((functionp key) (or (funcall key) ""))
     ((stringp key) key)
     (t ""))))

;;; Per-model dispatch resolvers
;; ============================================================================
;; `:api-type' and `:endpoint' may be a static value (symbol/string) OR a
;; function of MODEL.  The function form lets a single provider entry
;; route different models through different API shapes / endpoints —
;; e.g. one OpenCode Go subscription whose chat models go to
;; /chat/completions while message models go to /messages.

(defun ollama-buddy-provider--effective-model (provider model)
  "Return a non-nil model name for dispatch under PROVIDER.
Falls back to the current model or the provider's default model when
the caller passes nil."
  (or model
      ollama-buddy--current-model
      (let ((default (ollama-buddy-provider-default-model provider)))
        (and default (concat (ollama-buddy-provider-prefix provider)
                             default)))))

(defun ollama-buddy-provider--resolve-api-type (provider model)
  "Return the API type symbol for PROVIDER and MODEL.
If api-type is a function, call it with the effective model name."
  (let ((at (ollama-buddy-provider-api-type provider)))
    (if (functionp at)
        (funcall at (ollama-buddy-provider--effective-model provider model))
      at)))

(defun ollama-buddy-provider--resolve-endpoint (provider model)
  "Return the endpoint URL for PROVIDER and MODEL.
If endpoint is a function, call it with the effective model name."
  (let ((ep (ollama-buddy-provider-endpoint provider)))
    (if (functionp ep)
        (funcall ep (ollama-buddy-provider--effective-model provider model))
      ep)))

;;; Send function factory
;; ============================================================================

(defun ollama-buddy-provider--make-send-fn (provider)
  "Return a send function (closure) for PROVIDER.
The closure looks up the provider from the registry each time, so
updates to the provider struct are reflected immediately."
  (let ((prefix (ollama-buddy-provider-prefix provider)))
    (lambda (prompt &optional model)
      (let ((prov (gethash prefix ollama-buddy-provider--registry)))
        (if (null prov)
            (user-error "No provider registered for prefix %s" prefix)
          (pcase (ollama-buddy-provider--resolve-api-type prov model)
            ('openai (ollama-buddy-provider--openai-send prov prompt model))
            ('claude (ollama-buddy-provider--claude-send prov prompt model))
            ('gemini (ollama-buddy-provider--gemini-send prov prompt model))
            (_ (error "Unknown api-type: %s"
                      (ollama-buddy-provider--resolve-api-type prov model)))))))))

;;; OpenAI-compatible send
;; ============================================================================

(defun ollama-buddy-provider--openai-send (provider prompt model)
  "Send PROMPT via PROVIDER using the OpenAI-compatible path.
MODEL is the prefixed model name, or nil for the default."
  (let* ((tools-schema
          (when (and (featurep 'ollama-buddy-tools)
                     (bound-and-true-p ollama-buddy-tools-enabled)
                     (not (and (boundp 'ollama-buddy--suppress-tools-once)
                               ollama-buddy--suppress-tools-once))
                     (fboundp 'ollama-buddy-tools--generate-schema))
            (when (boundp 'ollama-buddy--suppress-tools-once)
              (setq ollama-buddy--suppress-tools-once nil))
            (ollama-buddy-tools--generate-schema))))
    (ollama-buddy-remote--openai-send
     prompt model
     (append
      (list :prefix (ollama-buddy-provider-prefix provider)
            :api-key (ollama-buddy-provider--resolve-key provider)
            :endpoint (ollama-buddy-provider--resolve-endpoint provider model)
            :temperature (ollama-buddy-provider-temperature provider)
            :max-tokens (ollama-buddy-provider-max-tokens provider)
            :default-model (ollama-buddy-provider-default-model provider)
            :provider-name (ollama-buddy-provider-name provider)
            :extra-headers (ollama-buddy-provider-extra-headers provider)
            :token-count-var (ollama-buddy-provider-token-count-sym provider))
      (when tools-schema
        (list :tools-schema tools-schema))))))

;;; Claude send
;; ============================================================================

(defun ollama-buddy-provider--claude-send (provider prompt model)
  "Send PROMPT via PROVIDER using the Claude Messages API.
MODEL is the prefixed model name, or nil for the default."
  (let* ((openai-schema
          (when (and (featurep 'ollama-buddy-tools)
                     (bound-and-true-p ollama-buddy-tools-enabled)
                     (not (and (boundp 'ollama-buddy--suppress-tools-once)
                               ollama-buddy--suppress-tools-once))
                     (fboundp 'ollama-buddy-tools--generate-schema))
            (when (boundp 'ollama-buddy--suppress-tools-once)
              (setq ollama-buddy--suppress-tools-once nil))
            (ollama-buddy-tools--generate-schema)))
         (tools-schema
          (when openai-schema
            (ollama-buddy-remote--convert-schema-to-claude openai-schema))))
    (ollama-buddy-remote--process-inline-features-async
     prompt
     (lambda (processed-prompt)
       (ollama-buddy-provider--claude-send-payload
        provider processed-prompt model tools-schema)))))

(defun ollama-buddy-provider--claude-send-payload (provider prompt model
                                                             &optional tools-schema)
  "Build and send the Claude payload for PROMPT via PROVIDER with MODEL.
TOOLS-SCHEMA is an optional Claude-format tools vector."
  (let* ((prefix (ollama-buddy-provider-prefix provider))
         (api-key (ollama-buddy-provider--resolve-key provider))
         (token-sym (ollama-buddy-provider-token-count-sym provider))
         (provider-name (ollama-buddy-provider-name provider))
         (tool-continuation-p (bound-and-true-p
                                ollama-buddy-remote--tool-continuation-p)))
    ;; Set current model
    (setq ollama-buddy--current-model
          (or model
              ollama-buddy--current-model
              (ollama-buddy-remote--get-full-model-name
               prefix (ollama-buddy-provider-default-model provider))))
    (set token-sym 0)

    ;; Reset tool iteration counter for new requests
    (unless tool-continuation-p
      (setq ollama-buddy--tool-call-iteration 0))

    (let* ((history (when ollama-buddy-history-enabled
                      (gethash ollama-buddy--current-model
                               ollama-buddy--conversation-history-by-model
                               nil)))
           ;; Convert OpenAI-format history to Claude format
           (claude-history (ollama-buddy-remote--convert-history-to-claude history))
           (system-prompt (ollama-buddy--effective-system-prompt))
           (full-context (ollama-buddy-remote--build-context))
           ;; Claude: system prompt is a top-level key, NOT in messages
           ;; For tool continuations, use history only (no new user message)
           (messages (if tool-continuation-p
                        (vconcat [] claude-history)
                      (vconcat []
                               (append
                                claude-history
                                `(((role . "user")
                                   (content . ,(if full-context
                                                   (concat prompt "\n\n"
                                                           full-context)
                                                 prompt))))))))
           (max-tokens (or (ollama-buddy-provider-max-tokens provider) 4096))
           (json-payload
            `((model . ,(ollama-buddy-remote--get-real-model-name
                         prefix ollama-buddy--current-model))
              (messages . ,messages)
              (temperature . ,(ollama-buddy-provider-temperature provider))
              (max_tokens . ,max-tokens)))
           ;; Add system prompt
           (json-payload
            (if (and system-prompt (not (string-empty-p system-prompt)))
                (append json-payload `((system . ,system-prompt)))
              json-payload))
           ;; Add tools schema
           (json-payload
            (if tools-schema
                (append json-payload `((tools . ,tools-schema)))
              json-payload))
           (stream-json-payload
            (if ollama-buddy-streaming-enabled
                (append json-payload '((stream . t)))
              json-payload))
           (json-str (let ((json-encoding-pretty-print nil))
                       (ollama-buddy-escape-unicode
                        (json-encode stream-json-payload))))
           (start-point
            (ollama-buddy-remote--prepare-chat-buffer provider-name))
           (headers (append
                     `(("Content-Type" . "application/json")
                       ("Authorization" . ,(concat "Bearer " api-key))
                       ("X-API-Key" . ,api-key)
                       ("anthropic-version" . "2023-06-01"))
                     (ollama-buddy-provider-extra-headers provider)))
           ;; Use tools-aware extractor when tools present
           (extractor (if tools-schema
                         #'ollama-buddy-remote--claude-extract-content-with-tools
                       #'ollama-buddy-remote--claude-extract-content)))

      ;; Store continuation state for streaming finalize
      (setq ollama-buddy-remote--streaming-tool-continuation-p tool-continuation-p)

      (if ollama-buddy-streaming-enabled
          ;; Streaming: curl with SSE
          (ollama-buddy-remote--start-streaming-request
           (ollama-buddy-provider--resolve-endpoint
            provider ollama-buddy--current-model)
           headers json-str
           extractor
           provider-name prompt start-point)
        ;; Non-streaming: url-retrieve
        (let* ((url-request-method "POST")
               (url-request-extra-headers headers)
               (url-request-data json-str)
               (url-mime-charset-string "utf-8")
               (url-mime-language-string nil)
               (url-mime-encoding-string nil)
               (url-mime-accept-string "application/json"))
          (url-retrieve
           (ollama-buddy-provider--resolve-endpoint
            provider ollama-buddy--current-model)
           (lambda (status)
             (if (plist-get status :error)
                 (ollama-buddy-remote--handle-http-error
                  start-point (plist-get status :error))
               (goto-char (point-min))
               (when (re-search-forward "\n\n" nil t)
                 (let* ((raw (buffer-substring (point) (point-max)))
                        (decoded (decode-coding-string raw 'utf-8))
                        (json-object-type 'alist)
                        (json-array-type 'vector)
                        (json-key-type 'symbol))
                   (condition-case err
                       (let* ((response (json-read-from-string decoded))
                              (error-msg (alist-get 'error response))
                              (content ""))
                         (if error-msg
                             (setq content
                                   (format "Error: %s"
                                           (ollama-buddy-remote--format-api-error
                                            error-msg)))
                           (setq content
                                 (ollama-buddy-provider--claude-extract-response
                                  response)))
                         (ollama-buddy-remote--finalize-response
                          start-point content prompt token-sym))
                     (error
                      (ollama-buddy-remote--handle-error
                       start-point provider-name
                       (error-message-string err))))))))))))))

(defun ollama-buddy-provider--claude-extract-response (response)
  "Extract text content from a Claude API RESPONSE (non-streaming)."
  (let ((extracted "")
        (content-obj (alist-get 'content response)))
    (cond
     ;; content is an alist with a nested content array
     ((and (listp content-obj) (alist-get 'content content-obj))
      (let ((arr (alist-get 'content content-obj)))
        (when (vectorp arr)
          (dotimes (i (length arr))
            (let* ((item (aref arr i))
                   (item-type (alist-get 'type item))
                   (item-text (alist-get 'text item)))
              (when (and (string= item-type "text") item-text)
                (setq extracted (concat extracted item-text))))))))
     ;; content is directly a vector of content blocks
     ((vectorp content-obj)
      (dotimes (i (length content-obj))
        (let* ((item (aref content-obj i))
               (item-type (alist-get 'type item))
               (item-text (alist-get 'text item)))
          (when (and (string= item-type "text") item-text)
            (setq extracted (concat extracted item-text)))))))
    extracted))

;;; Gemini send
;; ============================================================================

(defun ollama-buddy-provider--gemini-send (provider prompt model)
  "Send PROMPT via PROVIDER using the Gemini API.
MODEL is the prefixed model name, or nil for the default."
  (let* ((openai-schema
          (when (and (featurep 'ollama-buddy-tools)
                     (bound-and-true-p ollama-buddy-tools-enabled)
                     (not (and (boundp 'ollama-buddy--suppress-tools-once)
                               ollama-buddy--suppress-tools-once))
                     (fboundp 'ollama-buddy-tools--generate-schema))
            (when (boundp 'ollama-buddy--suppress-tools-once)
              (setq ollama-buddy--suppress-tools-once nil))
            (ollama-buddy-tools--generate-schema)))
         (tools-schema
          (when openai-schema
            (ollama-buddy-remote--convert-schema-to-gemini openai-schema))))
    (ollama-buddy-remote--process-inline-features-async
     prompt
     (lambda (processed-prompt)
       (ollama-buddy-provider--gemini-send-payload
        provider processed-prompt model tools-schema)))))

(defun ollama-buddy-provider--gemini-send-payload (provider prompt model
                                                            &optional tools-schema)
  "Build and send the Gemini payload for PROMPT via PROVIDER with MODEL.
TOOLS-SCHEMA is an optional Gemini-format tools vector."
  (let* ((prefix (ollama-buddy-provider-prefix provider))
         (api-key (ollama-buddy-provider--resolve-key provider))
         (token-sym (ollama-buddy-provider-token-count-sym provider))
         (provider-name (ollama-buddy-provider-name provider))
         (tool-continuation-p (bound-and-true-p
                                ollama-buddy-remote--tool-continuation-p)))
    ;; Set current model
    (setq ollama-buddy--current-model
          (or model
              ollama-buddy--current-model
              (ollama-buddy-remote--get-full-model-name
               prefix (ollama-buddy-provider-default-model provider))))
    (set token-sym 0)

    ;; Reset tool iteration counter for new requests
    (unless tool-continuation-p
      (setq ollama-buddy--tool-call-iteration 0))

    (let* ((model-name (ollama-buddy-remote--get-real-model-name
                        prefix ollama-buddy--current-model))
           (history (when ollama-buddy-history-enabled
                      (gethash ollama-buddy--current-model
                               ollama-buddy--conversation-history-by-model
                               nil)))
           (system-prompt (ollama-buddy--effective-system-prompt))
           (full-context (ollama-buddy-remote--build-context))
           ;; Convert history to Gemini format (no system messages —
           ;; Gemini uses top-level system_instruction instead)
           (gemini-history (ollama-buddy-remote--convert-history-to-gemini
                            history))
           ;; For tool continuations, use history only
           (formatted-contents
            (if tool-continuation-p
                (vconcat [] gemini-history)
              (vconcat []
                       (append gemini-history
                               `(((role . "user")
                                  (parts . ,(vector
                                             `((text . ,(if full-context
                                                            (concat prompt "\n\n"
                                                                    full-context)
                                                          prompt)))))))))))
           ;; Build endpoints — model name in URL.  When :endpoint is a
           ;; function it may return either a %s template or a pre-
           ;; substituted URL; format handles both safely.
           (api-endpoint
            (format (ollama-buddy-provider--resolve-endpoint
                     provider ollama-buddy--current-model)
                    model-name))
           (api-endpoint-with-key
            (concat api-endpoint "?key=" api-key))
           (stream-api-endpoint
            (concat (replace-regexp-in-string
                     ":generateContent$" ":streamGenerateContent"
                     api-endpoint)
                    "?alt=sse&key=" api-key))
           (json-payload
            `((contents . ,formatted-contents)
              (generationConfig
               . ((temperature
                   . ,(ollama-buddy-provider-temperature provider))))))
           ;; Add system_instruction as top-level field (Gemini native)
           (json-payload
            (if (and system-prompt (not (string-empty-p system-prompt)))
                (append json-payload
                        `((systemInstruction
                           . ((parts . ,(vector
                                         `((text . ,system-prompt))))))))
              json-payload))
           ;; Add max tokens
           (json-payload
            (if (ollama-buddy-provider-max-tokens provider)
                (let ((gen-config
                       (alist-get 'generationConfig json-payload)))
                  (setf (alist-get 'generationConfig json-payload)
                        (append gen-config
                                `((maxOutputTokens
                                   . ,(ollama-buddy-provider-max-tokens
                                       provider)))))
                  json-payload)
              json-payload))
           ;; Add tools schema and toolConfig
           (json-payload
            (if tools-schema
                (append json-payload
                        `((tools . ,tools-schema)
                          (toolConfig
                           . ((functionCallingConfig
                               . ((mode . "AUTO")))))))
              json-payload))
           (json-str (let ((json-encoding-pretty-print nil))
                       (ollama-buddy-escape-unicode
                        (json-encode json-payload))))
           (start-point
            (ollama-buddy-remote--prepare-chat-buffer provider-name))
           (headers (append '(("Content-Type" . "application/json"))
                            (ollama-buddy-provider-extra-headers provider)))
           ;; Use tools-aware extractor when tools present
           (extractor (if tools-schema
                         #'ollama-buddy-remote--gemini-extract-content-with-tools
                       #'ollama-buddy-remote--gemini-extract-content)))

      ;; Store continuation state for streaming finalize
      (setq ollama-buddy-remote--streaming-tool-continuation-p tool-continuation-p)

      (if ollama-buddy-streaming-enabled
          ;; Streaming: curl with SSE via streamGenerateContent
          (ollama-buddy-remote--start-streaming-request
           stream-api-endpoint
           headers json-str
           extractor
           provider-name prompt start-point)
        ;; Non-streaming: url-retrieve
        (let* ((url-request-method "POST")
               (url-request-extra-headers headers)
               (url-request-data json-str)
               (url-mime-charset-string "utf-8")
               (url-mime-language-string nil)
               (url-mime-encoding-string nil)
               (url-mime-accept-string "application/json"))
          (url-retrieve
           api-endpoint-with-key
           (lambda (status)
             (if (plist-get status :error)
                 (ollama-buddy-remote--handle-http-error
                  start-point (plist-get status :error))
               (goto-char (point-min))
               (when (re-search-forward "\n\n" nil t)
                 (let* ((raw (buffer-substring (point) (point-max)))
                        (decoded (decode-coding-string raw 'utf-8))
                        (json-object-type 'alist)
                        (json-array-type 'vector)
                        (json-key-type 'symbol))
                   (condition-case err
                       (let* ((response (json-read-from-string decoded))
                              (error-msg (alist-get 'error response))
                              (content ""))
                         (if error-msg
                             (setq content
                                   (format "Error: %s"
                                           (ollama-buddy-remote--format-api-error
                                            error-msg)))
                           ;; Gemini response: candidates[0].content.parts[0].text
                           (let* ((candidates
                                   (alist-get 'candidates response))
                                  (first
                                   (when (and (vectorp candidates)
                                              (> (length candidates) 0))
                                     (aref candidates 0)))
                                  (content-obj
                                   (when first
                                     (alist-get 'content first)))
                                  (parts
                                   (when content-obj
                                     (alist-get 'parts content-obj))))
                             (when (and (vectorp parts) (> (length parts) 0))
                               (setq content
                                     (alist-get 'text (aref parts 0))))))
                         (ollama-buddy-remote--finalize-response
                          start-point content prompt token-sym))
                     (error
                      (ollama-buddy-remote--handle-error
                       start-point provider-name
                       (error-message-string err))))))))))))))

(defun ollama-buddy-provider--gemini-format-messages (messages)
  "Format chat MESSAGES for the Gemini API.
Transforms OpenAI-format messages into Gemini's contents/parts format."
  (let ((formatted '()))
    (dolist (msg messages)
      (let ((role (alist-get 'role msg))
            (content (alist-get 'content msg)))
        (cond
         ((string= role "system")
          (push `((role . "user")
                  (parts . [((text . ,(format "[System Instruction] %s"
                                              content)))]))
                formatted))
         ((string= role "user")
          (push `((role . "user")
                  (parts . [((text . ,content))]))
                formatted))
         ((string= role "assistant")
          (push `((role . "model")
                  (parts . [((text . ,content))]))
                formatted)))))
    (vconcat [] (reverse formatted))))

;;; Model discovery
;; ============================================================================

(defun ollama-buddy-provider--fetch-models (provider)
  "Fetch and register models for PROVIDER.
Uses :models-endpoint for dynamic discovery if set, with :models
as fallback.  If only :models is set, registers those directly."
  (let* ((prefix (ollama-buddy-provider-prefix provider))
         (static-models (ollama-buddy-provider-models provider))
         (models-ep (ollama-buddy-provider-models-endpoint provider))
         (send-fn (ollama-buddy-provider--make-send-fn provider))
         (provider-name (ollama-buddy-provider-name provider)))
    (cond
     ;; Dynamic discovery (with static as fallback)
     ;; Note: old models are cleaned inside the response handler before registering new ones
     (models-ep
      (ollama-buddy-provider--fetch-models-dynamic provider send-fn))
     ;; Static list only
     (static-models
      (ollama-buddy-remote--register-models prefix static-models send-fn))
     (t
      (message "ollama-buddy-provider: %s has no models configured"
               provider-name)))))

(defun ollama-buddy-provider--fetch-models-dynamic (provider send-fn)
  "Fetch models from PROVIDER's discovery endpoint and register them.
SEND-FN is the send function to register with the models.
Falls back to the static :models list on failure."
  (let* ((models-ep (ollama-buddy-provider-models-endpoint provider))
         (api-key (ollama-buddy-provider--resolve-key provider))
         ;; Resolve api-type with a nil model — falls back to default
         ;; model.  For static api-type this is identical to the raw
         ;; field; for function api-type it picks the default's shape.
         (api-type (ollama-buddy-provider--resolve-api-type provider nil))
         (prefix (ollama-buddy-provider-prefix provider))
         (provider-name (ollama-buddy-provider-name provider))
         (static-models (ollama-buddy-provider-models provider))
         (models-filter (ollama-buddy-provider-models-filter provider))
         (url-request-method "GET")
         (url-request-extra-headers
          (pcase api-type
            ('claude
             `(("x-api-key" . ,api-key)
               ("anthropic-version" . "2023-06-01")))
            ('gemini
             `(("x-goog-api-key" . ,api-key)))
            (_
             (when (and api-key (not (string-empty-p api-key)))
               `(("Authorization" . ,(concat "Bearer " api-key))))))))
    (ollama-buddy--update-status
     (format "Fetching %s models..." provider-name))
    (url-retrieve
     models-ep
     (lambda (status)
       (if (plist-get status :error)
           (progn
             (ollama-buddy-remote--friendly-fetch-error
              status provider-name)
             ;; Fall back to static models
             (when static-models
               (ollama-buddy-remote--register-models
                prefix static-models send-fn)))
         (condition-case err
             (progn
               (goto-char (point-min))
               (when (re-search-forward "\n\n" nil t)
                 (let* ((json-object-type 'alist)
                        (json-array-type 'vector)
                        (json-key-type 'symbol)
                        (raw (buffer-substring (point) (point-max)))
                        (response (json-read-from-string raw))
                        (models
                         (ollama-buddy-provider--parse-models-response
                          response api-type prefix)))
                   ;; Apply filter if provided
                   (when models-filter
                     (setq models
                           (cl-remove-if-not models-filter models)))
                   (if models
                       (ollama-buddy-remote--register-models
                        prefix models send-fn)
                     ;; Empty result — fall back to static
                     (when static-models
                       (ollama-buddy-remote--register-models
                        prefix static-models send-fn))))))
           (error
            (message "ollama-buddy-provider: Error fetching %s models: %s"
                     provider-name (error-message-string err))
            (when static-models
              (ollama-buddy-remote--register-models
               prefix static-models send-fn))))))
     nil t)))

(defun ollama-buddy-provider--parse-models-response
    (response api-type prefix)
  "Parse model list from RESPONSE according to API-TYPE.
PREFIX is used for metadata caching.
Returns a list of model name strings (without prefix)."
  (pcase api-type
    ('claude
     ;; Claude: {"data": [{"id": "claude-...", "display_name": "..."}]}
     (let* ((data (append (alist-get 'data response) nil))
            (pairs (delq nil
                         (mapcar
                          (lambda (info)
                            (let ((id (alist-get 'id info))
                                  (display (alist-get 'display_name info)))
                              (when id (cons id display))))
                          data)))
            (models (mapcar #'car pairs)))
       ;; Cache display names
       (dolist (pair pairs)
         (let* ((full-name (concat prefix (car pair)))
                (ctx (ollama-buddy--get-context-window full-name)))
           (puthash full-name
                    `((display-name . ,(cdr pair))
                      (context-window . ,ctx))
                    ollama-buddy--models-metadata-cache)))
       models))

    ('gemini
     ;; Gemini: {"models": [{"name": "models/gemini-...", ...}]}
     (let* ((data (append (alist-get 'models response) nil))
            (triples
             (delq nil
                   (mapcar
                    (lambda (info)
                      (let* ((raw (alist-get 'name info))
                             (short (if (string-match "models/\\(.*\\)" raw)
                                        (match-string 1 raw)
                                      raw))
                             (ctx (alist-get 'inputTokenLimit info))
                             (display (alist-get 'displayName info)))
                        (list short ctx display)))
                    data)))
            (models (mapcar #'car triples)))
       ;; Cache context windows and display names
       (dolist (entry triples)
         (puthash (concat prefix (car entry))
                  `((context-window . ,(cadr entry))
                    (display-name . ,(caddr entry)))
                  ollama-buddy--models-metadata-cache))
       models))

    (_
     ;; OpenAI-compatible: {"data": [{"id": "model-name"}]}
     (let* ((data (alist-get 'data response))
            (ids (mapcar (lambda (entry)
                           (alist-get 'id entry))
                         (append data nil))))
       ids))))

;;; Provider model queries
;; ============================================================================

(defun ollama-buddy-provider--model-is-provider-p (model)
  "Return non-nil if MODEL belongs to a registered generic provider."
  (catch 'found
    (maphash (lambda (prefix _provider)
               (when (string-prefix-p prefix model)
                 (throw 'found prefix)))
             ollama-buddy-provider--registry)
    nil))

(defun ollama-buddy-provider--get-for-model (model)
  "Return the provider struct for MODEL, or nil if not a provider model."
  (catch 'found
    (maphash (lambda (prefix provider)
               (when (string-prefix-p prefix model)
                 (throw 'found provider)))
             ollama-buddy-provider--registry)
    nil))

;;; Public API
;; ============================================================================

;;;###autoload
(defun ollama-buddy-provider-create (&rest args)
  "Create and register a new LLM provider.

ARGS is a plist with the following keys:

Required:
  :name           - Display name (e.g., \"DeepSeek\")
  :prefix         - Model prefix (e.g., \"d:\")
  :endpoint       - API endpoint URL string, OR a function (MODEL) -> URL
                    string when a single provider routes different models
                    to different endpoints

Optional:
  :api-key        - API key: string, function, or nil
  :api-type       - Symbol `openai' (default), `claude', or `gemini',
                    OR a function (MODEL) -> symbol for per-model
                    dispatch (one provider serving multiple shapes)
  :models         - Static list of model names (without prefix)
  :models-endpoint - URL for dynamic model discovery
  :models-filter  - Predicate to filter discovered model IDs
  :default-model  - Default model name (without prefix)
  :temperature    - Temperature float (default 0.7)
  :max-tokens     - Max tokens integer or nil
  :extra-headers  - Additional HTTP headers alist

Either :models or :models-endpoint (or both) should be provided.
When both are set, :models-endpoint is tried first with :models as
the fallback if discovery fails.

Returns the provider struct."
  (let* ((name (or (plist-get args :name)
                   (error ":name is required")))
         (prefix (or (plist-get args :prefix)
                     (error ":prefix is required")))
         (endpoint (or (plist-get args :endpoint)
                       (error ":endpoint is required")))
         (api-type (or (plist-get args :api-type) 'openai))
         ;; Create a unique interned symbol for token counting
         (token-sym (intern
                     (format "ollama-buddy-provider--%s-token-count"
                             (replace-regexp-in-string
                              "[^a-zA-Z0-9]" "-" (downcase name)))))
         (provider
          (ollama-buddy-provider--make
           :name name
           :prefix prefix
           :api-key (plist-get args :api-key)
           :api-type api-type
           :endpoint endpoint
           :models-endpoint (plist-get args :models-endpoint)
           :models (plist-get args :models)
           :models-filter (plist-get args :models-filter)
           :default-model (or (plist-get args :default-model)
                              (car (plist-get args :models)))
           :temperature (or (plist-get args :temperature) 0.7)
           :max-tokens (plist-get args :max-tokens)
           :extra-headers (plist-get args :extra-headers)
           :token-count-sym token-sym)))

    ;; Initialise the token counter symbol
    (set token-sym 0)

    ;; Store in registry
    (puthash prefix provider ollama-buddy-provider--registry)

    ;; Update provider labels (remove old entry for this prefix if any)
    (setq ollama-buddy--provider-labels
          (cl-remove-if (lambda (pair) (string= (car pair) prefix))
                        ollama-buddy--provider-labels))
    (push (cons prefix name) ollama-buddy--provider-labels)

    ;; Fetch/register models (deferred to avoid blocking init)
    (run-with-idle-timer
     0.5 nil #'ollama-buddy-provider--fetch-models provider)

    provider))

;;;###autoload
(defun ollama-buddy-provider-remove (prefix)
  "Remove the provider registered under PREFIX.
Unregisters its models and removes it from the registry."
  (interactive
   (list (completing-read "Remove provider: "
                          (hash-table-keys
                           ollama-buddy-provider--registry)
                          nil t)))
  (when (gethash prefix ollama-buddy-provider--registry)
    (remhash prefix ollama-buddy-provider--registry)
    ;; Remove from provider labels
    (setq ollama-buddy--provider-labels
          (cl-remove-if (lambda (pair) (string= (car pair) prefix))
                        ollama-buddy--provider-labels))
    ;; Remove registered models
    (setq ollama-buddy-remote-models
          (cl-remove-if (lambda (m) (string-prefix-p prefix m))
                        ollama-buddy-remote-models))
    (message "Removed provider %s" prefix)))

;;;###autoload
(defun ollama-buddy-provider-refresh (prefix)
  "Re-fetch models for the provider registered under PREFIX."
  (interactive
   (list (completing-read "Refresh provider: "
                          (hash-table-keys
                           ollama-buddy-provider--registry)
                          nil t)))
  (let ((provider (gethash prefix ollama-buddy-provider--registry)))
    (if provider
        (progn
          (ollama-buddy-provider--fetch-models provider)
          (message "Refreshing %s models..."
                   (ollama-buddy-provider-name provider)))
      (user-error "No provider registered with prefix %s" prefix))))

;;;###autoload
(defun ollama-buddy-provider-list ()
  "Display all registered generic providers."
  (interactive)
  (let ((providers '()))
    (maphash
     (lambda (_prefix provider)
       (let ((at (ollama-buddy-provider-api-type provider))
             (ep (ollama-buddy-provider-endpoint provider)))
         (push (format "  %-6s %-15s %-8s %s"
                       (ollama-buddy-provider-prefix provider)
                       (ollama-buddy-provider-name provider)
                       (if (functionp at) "<dispatch>" at)
                       (if (functionp ep) "<dispatch>" ep))
               providers)))
     ollama-buddy-provider--registry)
    (if providers
        (message "Registered providers:\n%s"
                 (string-join (sort providers #'string<) "\n"))
      (message "No generic providers registered"))))

(provide 'ollama-buddy-provider)
;;; ollama-buddy-provider.el ends here
