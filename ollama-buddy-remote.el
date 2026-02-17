;;; ollama-buddy-remote.el --- Shared infrastructure for remote providers -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This module provides shared infrastructure for all remote LLM providers
;; (OpenAI, Claude, Gemini, Grok, Copilot, Codestral).  It extracts common
;; helper functions and the OpenAI-compatible send function so each provider
;; module is a thin configuration layer.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'ollama-buddy-core)

;; Web search forward declarations
(declare-function ollama-buddy-web-search-process-inline "ollama-buddy-web-search")
(declare-function ollama-buddy-web-search-get-context "ollama-buddy-web-search")
;; RAG forward declarations
(declare-function ollama-buddy-rag-process-inline "ollama-buddy-rag")

;;; Prefix helper functions
;; ============================================================================

(defun ollama-buddy-remote--is-provider-model (prefix model)
  "Check if MODEL belongs to the provider identified by PREFIX."
  (and model (string-prefix-p prefix model)))

(defun ollama-buddy-remote--get-full-model-name (prefix model)
  "Get the full model name by prepending PREFIX to MODEL."
  (concat prefix model))

(defun ollama-buddy-remote--get-real-model-name (prefix model)
  "Extract the actual model name from MODEL by stripping PREFIX."
  (if (ollama-buddy-remote--is-provider-model prefix model)
      (string-trim (substring model (length prefix)))
    model))

;;; API key verification
;; ============================================================================

(defun ollama-buddy-remote--verify-api-key (key-value key-symbol provider-name)
  "Verify that KEY-VALUE is non-empty.
KEY-SYMBOL is the defcustom symbol to open for configuration.
PROVIDER-NAME is used in the error message."
  (if (string-empty-p key-value)
      (progn
        (customize-variable key-symbol)
        (error "Please set your %s API key" provider-name))
    t))

;;; Context building
;; ============================================================================

(defun ollama-buddy-remote--build-context ()
  "Build the full context string from attachments and web search results.
Returns nil if there is no context to include."
  (let* ((attachment-context
          (when ollama-buddy--current-attachments
            (concat "\n\n## Attached Files Context:\n\n"
                    (mapconcat
                     (lambda (attachment)
                       (let ((file (plist-get attachment :file))
                             (content (plist-get attachment :content)))
                         (format "### File: %s\n\n#+end_src%s\n%s\n#+begin_src \n\n"
                                 (file-name-nondirectory file)
                                 (or (plist-get attachment :type) "")
                                 content)))
                     ollama-buddy--current-attachments
                     ""))))
         (web-search-context
          (when (and (featurep 'ollama-buddy-web-search)
                     (fboundp 'ollama-buddy-web-search-get-context))
            (ollama-buddy-web-search-get-context)))
         (contexts (delq nil (list attachment-context web-search-context))))
    (when contexts (mapconcat #'identity contexts "\n\n"))))

;;; Message building
;; ============================================================================

(defun ollama-buddy-remote--build-openai-messages (system-prompt history prompt full-context)
  "Build the OpenAI-format messages array.
SYSTEM-PROMPT is the system instruction (or nil).
HISTORY is the conversation history list.
PROMPT is the current user prompt.
FULL-CONTEXT is the attachment/search context string (or nil)."
  (vconcat []
           (append
            (when (and system-prompt (not (string-empty-p system-prompt)))
              `(((role . "system") (content . ,system-prompt))))
            history
            `(((role . "user")
               (content . ,(if full-context
                               (concat prompt "\n\n" full-context)
                             prompt)))))))

;;; Inline feature processing
;; ============================================================================

(defun ollama-buddy-remote--process-inline-features (prompt)
  "Process inline web search and RAG features in PROMPT.
Returns the possibly-modified prompt string."
  ;; Process inline web search delimiters if web-search module is loaded
  (when (and (featurep 'ollama-buddy-web-search)
             (fboundp 'ollama-buddy-web-search-process-inline))
    (setq prompt (ollama-buddy-web-search-process-inline prompt)))
  ;; Process inline @rag() queries if RAG module is loaded
  (when (and (featurep 'ollama-buddy-rag)
             (fboundp 'ollama-buddy-rag-process-inline))
    (setq prompt (ollama-buddy-rag-process-inline prompt)))
  prompt)

;;; Chat buffer preparation
;; ============================================================================

(defun ollama-buddy-remote--prepare-chat-buffer (provider-name)
  "Prepare the chat buffer for a new response from PROVIDER-NAME.
Returns the start-point marker for where the response content begins.
This must be called within a `let' that binds `inhibit-read-only' to t."
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (pop-to-buffer (current-buffer))
    (goto-char (point-max))

    (unless (> (buffer-size) 0)
      (insert (ollama-buddy--create-intro-message)))

    ;; Show any attached files
    (when ollama-buddy--current-attachments
      (insert (format "\n\n[Including %d attached file(s) in context]"
                      (length ollama-buddy--current-attachments))))

    (let ((inhibit-read-only t)
          start-point)
      (insert (format "\n\n** [%s: RESPONSE]\n\n" ollama-buddy--current-model))
      (setq start-point (point))
      (insert "Loading response...")
      (ollama-buddy--update-status (format "Sending request to %s..." provider-name))
      (set-register ollama-buddy-default-register "")
      start-point)))

;;; Response finalization
;; ============================================================================

(defun ollama-buddy-remote--finalize-response (start-point content prompt token-count-symbol)
  "Finalize a successful response in the chat buffer.
START-POINT is where the response content starts.
CONTENT is the response text.
PROMPT is the original user prompt (for history).
TOKEN-COUNT-SYMBOL is the symbol to set with the token count."
  (with-current-buffer ollama-buddy--chat-buffer
    (let* ((inhibit-read-only t)
           (window (get-buffer-window ollama-buddy--chat-buffer t))
           (token-count 0))
      (save-excursion
        (goto-char start-point)
        (delete-region start-point (point-max))

        ;; Insert the content
        (insert content)

        ;; Convert markdown to org if enabled
        (when ollama-buddy-convert-markdown-to-org
          (ollama-buddy--md-to-org-convert-region start-point (point-max)))

        ;; Write to register
        (let* ((reg-char ollama-buddy-default-register)
               (current (get-register reg-char))
               (new-content (concat (if (stringp current) current "") content)))
          (set-register reg-char new-content))

        ;; Add to history
        (when ollama-buddy-history-enabled
          (ollama-buddy--add-to-history "user" prompt)
          (ollama-buddy--add-to-history "assistant" content))

        ;; Calculate token count
        (setq token-count (length (split-string content "\\b" t)))
        (set token-count-symbol token-count)

        ;; Show token stats if enabled
        (when ollama-buddy-display-token-stats
          (insert (format "\n\n*** Token Stats\n[%d tokens]" token-count)))

        (insert "\n\n*** FINISHED")
        (ollama-buddy--prepare-prompt-area))
      ;; Move to prompt only if response fits in window
      (ollama-buddy--maybe-goto-prompt window start-point)
      (ollama-buddy--update-status
       (format "Finished [%d tokens]" (symbol-value token-count-symbol))))))

;;; Error handling
;; ============================================================================

(defun ollama-buddy-remote--handle-error (start-point provider-name error-info)
  "Handle an error in the chat buffer.
START-POINT is where the response content starts.
PROVIDER-NAME is the display name of the provider.
ERROR-INFO is a string with error details."
  (with-current-buffer ollama-buddy--chat-buffer
    (let ((inhibit-read-only t))
      (goto-char start-point)
      (delete-region start-point (point-max))
      (insert (format "Error: Failed to parse %s response\n" provider-name))
      (insert "Details: " error-info "\n")
      (insert "\n\n*** FAILED")
      (ollama-buddy--prepare-prompt-area)
      (ollama-buddy--update-status "Failed - JSON parse error"))))

(defun ollama-buddy-remote--handle-http-error (start-point error-details)
  "Handle an HTTP-level error in the chat buffer.
START-POINT is where the response content starts.
ERROR-DETAILS is the error plist from url-retrieve."
  (with-current-buffer ollama-buddy--chat-buffer
    (let ((inhibit-read-only t))
      (goto-char start-point)
      (delete-region start-point (point-max))
      (insert "Error: URL retrieval failed\n")
      (insert "Details: " (prin1-to-string error-details) "\n")
      (insert "\n\n*** FAILED")
      (ollama-buddy--prepare-prompt-area)
      (ollama-buddy--update-status "Failed - URL retrieval error"))))

;;; Model registration
;; ============================================================================

(defun ollama-buddy-remote--register-models (prefix models send-fn)
  "Register MODELS with PREFIX and SEND-FN as the handler.
Appends prefixed model names to `ollama-buddy-remote-models'."
  (let ((prefixed-models (mapcar (lambda (model-name)
                                   (concat prefix model-name))
                                 models)))
    (when (fboundp 'ollama-buddy-register-model-handler)
      (ollama-buddy-register-model-handler prefix send-fn))
    (setq ollama-buddy-remote-models
          (append ollama-buddy-remote-models prefixed-models))))

;;; OpenAI-compatible send function
;; ============================================================================

(defun ollama-buddy-remote--openai-send (prompt model config)
  "Send PROMPT using MODEL via an OpenAI-compatible API.
CONFIG is a plist with the following keys:
  :prefix        - marker prefix string
  :api-key       - API key value
  :endpoint      - API endpoint URL
  :temperature   - temperature float
  :max-tokens    - max tokens integer or nil
  :default-model - fallback model name
  :provider-name - display name (\"OpenAI\", \"Grok\", etc.)
  :extra-headers - additional HTTP headers alist (optional)
  :token-count-var - symbol for the token count variable"
  (let* ((prefix (plist-get config :prefix))
         (api-key (plist-get config :api-key))
         (endpoint (plist-get config :endpoint))
         (temperature (plist-get config :temperature))
         (max-tokens (or (plist-get config :max-tokens) 4096))
         (default-model (plist-get config :default-model))
         (provider-name (plist-get config :provider-name))
         (extra-headers (plist-get config :extra-headers))
         (token-count-var (plist-get config :token-count-var)))

    ;; Process inline features
    (setq prompt (ollama-buddy-remote--process-inline-features prompt))

    ;; Set up the current model
    (setq ollama-buddy--current-model
          (or model
              ollama-buddy--current-model
              (ollama-buddy-remote--get-full-model-name prefix default-model)))

    ;; Initialize token counter
    (set token-count-var 0)

    ;; Get history and system prompt
    (let* ((history (when ollama-buddy-history-enabled
                      (gethash ollama-buddy--current-model
                               ollama-buddy--conversation-history-by-model
                               nil)))
           (system-prompt (ollama-buddy--effective-system-prompt))
           (full-context (ollama-buddy-remote--build-context))
           (messages (ollama-buddy-remote--build-openai-messages
                      system-prompt history prompt full-context))
           (json-payload
            `((model . ,(ollama-buddy-remote--get-real-model-name
                         prefix ollama-buddy--current-model))
              (messages . ,messages)
              (temperature . ,temperature)
              (max_tokens . ,max-tokens)))
           (json-str (let ((json-encoding-pretty-print nil))
                       (ollama-buddy-escape-unicode (json-encode json-payload))))
           (start-point (ollama-buddy-remote--prepare-chat-buffer provider-name)))

      ;; Make the HTTP request
      (let* ((url-request-method "POST")
             (url-request-extra-headers
              (append `(("Content-Type" . "application/json")
                        ("Authorization" . ,(concat "Bearer " api-key)))
                      extra-headers))
             (url-request-data json-str)
             (url-mime-charset-string "utf-8")
             (url-mime-language-string nil)
             (url-mime-encoding-string nil)
             (url-mime-accept-string "application/json"))

        (url-retrieve
         endpoint
         (lambda (status)
           (if (plist-get status :error)
               (ollama-buddy-remote--handle-http-error
                start-point (plist-get status :error))
             (progn
               ;; Success - process the response
               (goto-char (point-min))
               (when (re-search-forward "\n\n" nil t)
                 (let* ((json-response-raw (buffer-substring (point) (point-max)))
                        (json-response-decoded (decode-coding-string json-response-raw 'utf-8))
                        (json-object-type 'alist)
                        (json-array-type 'vector)
                        (json-key-type 'symbol))
                   (condition-case err
                       (let* ((json-response (json-read-from-string json-response-decoded))
                              (error-message (alist-get 'error json-response))
                              (content "")
                              (choices (alist-get 'choices json-response)))
                         ;; Extract the message content
                         (if error-message
                             (setq content (format "Error: %s" (alist-get 'message error-message)))
                           (when choices
                             (setq content (alist-get 'content
                                                      (alist-get 'message (aref choices 0))))))
                         ;; Finalize the response
                         (ollama-buddy-remote--finalize-response
                          start-point content prompt token-count-var))
                     (error
                      (ollama-buddy-remote--handle-error
                       start-point provider-name
                       (error-message-string err))))))))))))))

(provide 'ollama-buddy-remote)
;;; ollama-buddy-remote.el ends here
