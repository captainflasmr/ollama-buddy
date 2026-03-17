;;; ollama-buddy-remote.el --- Shared infrastructure for remote providers -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "29.1"))

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
(declare-function ollama-buddy-web-search-process-inline-async "ollama-buddy-web-search")
(declare-function ollama-buddy-web-search-get-context "ollama-buddy-web-search")
;; RAG forward declarations
(declare-function ollama-buddy-rag-process-inline "ollama-buddy-rag")
(declare-function ollama-buddy-rag-process-inline-async "ollama-buddy-rag")
;; Main module forward declarations
(declare-function ollama-buddy--trim-token-history "ollama-buddy")
(declare-function ollama-buddy--rebuild-tool-batch "ollama-buddy")
(declare-function ollama-buddy--model-supports-tools "ollama-buddy")
(declare-function ollama-buddy--send "ollama-buddy")
;; Tool module forward declarations
(declare-function ollama-buddy-tools--generate-schema "ollama-buddy-tools")
(declare-function ollama-buddy-tools--execute "ollama-buddy-tools")
(declare-function ollama-buddy-tools--format-args-for-display "ollama-buddy-tools")
;; Provider module forward declarations
(declare-function ollama-buddy-provider--model-is-provider-p "ollama-buddy-provider")

(defvar ollama-buddy-remote--request-start-time nil
  "Timestamp when the current remote request was sent.")

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

(defun ollama-buddy-remote--process-inline-features-async (prompt callback)
  "Process inline web search and RAG features in PROMPT asynchronously.
Calls CALLBACK with the modified prompt when done.
When PROMPT is nil (e.g. tool continuations), skips processing."
  (if (null prompt)
      (funcall callback prompt)
    ;; Web search (async) → RAG (async) → callback
    (let ((web-search-next
           (lambda (p)
             ;; Process inline @rag() queries asynchronously if RAG module is loaded
             (if (and (featurep 'ollama-buddy-rag)
                      (fboundp 'ollama-buddy-rag-process-inline-async))
                 (ollama-buddy-rag-process-inline-async p callback)
               (funcall callback p)))))
      ;; Process inline web search asynchronously if module is loaded
      (if (and (featurep 'ollama-buddy-web-search)
               (fboundp 'ollama-buddy-web-search-process-inline-async))
          (ollama-buddy-web-search-process-inline-async prompt web-search-next)
        (funcall web-search-next prompt)))))

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
      (let ((heading-start (point)))
        (insert (format "\n\n** [%s: RESPONSE]\n\n" ollama-buddy--current-model))
        (setq ollama-buddy--response-heading-marker
              (copy-marker (+ heading-start 2))))  ; skip the two newlines
      (setq start-point (point))
      (insert "Loading response...")
      (setq ollama-buddy-remote--request-start-time (float-time))
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
           (token-count 0)
           (elapsed-time (if ollama-buddy-remote--request-start-time
                             (- (float-time)
                                ollama-buddy-remote--request-start-time)
                           0))
           (token-rate 0))
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

        ;; Calculate token count and rate
        (setq token-count (length (split-string content "\\b" t)))
        (set token-count-symbol token-count)
        (setq token-rate (if (> elapsed-time 0)
                             (/ token-count elapsed-time)
                           0))

        ;; Record to token usage history
        (push (list :model ollama-buddy--current-model
                    :tokens token-count
                    :elapsed elapsed-time
                    :rate token-rate
                    :wait-time ollama-buddy--response-wait-duration
                    :timestamp (current-time))
              ollama-buddy--token-usage-history)
        (ollama-buddy--trim-token-history)

        ;; Insert property drawer on the response heading
        (ollama-buddy--insert-response-properties
         token-count elapsed-time token-rate
         ollama-buddy--response-wait-duration)

        (ollama-buddy--prepare-prompt-area))

      ;; Reset start time
      (setq ollama-buddy-remote--request-start-time nil)

      ;; Move to prompt only if response fits in window
      (ollama-buddy--maybe-goto-prompt window start-point)
      (ollama-buddy--update-status
       (format "Finished [%d tokens in %.1fs, %.1f t/s]"
               (symbol-value token-count-symbol)
               elapsed-time token-rate)))))

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

(defun ollama-buddy-remote--http-status-message (code)
  "Return a human-readable message for HTTP status CODE."
  (pcase code
    (400 "Bad Request -- The request was malformed or missing required fields.")
    (401 "Unauthorized -- Invalid or missing API key. Check your API key configuration.")
    (402 "Payment Required -- Your account has insufficient credits or requires a billing plan.")
    (403 "Forbidden -- Your API key does not have permission for this resource or model.")
    (404 "Not Found -- The model or endpoint does not exist. Check the model name.")
    (405 "Method Not Allowed -- The HTTP method is not supported for this endpoint.")
    (408 "Request Timeout -- The request took too long. Try a shorter prompt or simpler query.")
    (413 "Payload Too Large -- The request body is too large. Reduce prompt length or attachments.")
    (422 "Unprocessable Entity -- The request format is valid but the content cannot be processed.")
    (429 "Rate Limited -- Too many requests. You may need to wait, reduce request frequency, or upgrade to a paid tier.")
    (500 "Internal Server Error -- The provider is experiencing issues. Try again later.")
    (502 "Bad Gateway -- The provider's server is temporarily unavailable.")
    (503 "Service Unavailable -- The provider is overloaded or under maintenance.")
    (504 "Gateway Timeout -- The provider took too long to respond. Try again later.")
    (529 "Overloaded -- The provider is temporarily overloaded. Try again in a few moments.")
    (_ (format "HTTP error %s." code))))

(defun ollama-buddy-remote--extract-error-body ()
  "Try to extract an error message from the current HTTP response buffer.
Returns the provider's error message string, or nil if none found."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\n\n" nil t)
      (condition-case nil
          (let* ((json-object-type 'alist)
                 (json-array-type 'vector)
                 (json-key-type 'symbol)
                 (body (buffer-substring-no-properties (point) (point-max)))
                 (json-response (json-read-from-string body))
                 (error-obj (alist-get 'error json-response)))
            (cond
             ;; OpenAI/Grok/DeepSeek/OpenRouter: {"error": {"message": "...", "type": "..."}}
             ((and (listp error-obj) (alist-get 'message error-obj))
              (let ((msg (alist-get 'message error-obj))
                    (type (alist-get 'type error-obj)))
                (if type (format "%s (%s)" msg type) msg)))
             ;; Claude: {"error": {"message": "..."}} or {"type": "error", "error": {"type": "...", "message": "..."}}
             ((and (listp error-obj) (alist-get 'type error-obj))
              (format "%s (%s)"
                      (or (alist-get 'message error-obj) "Unknown error")
                      (alist-get 'type error-obj)))
             ;; Gemini: {"error": {"message": "...", "status": "..."}}
             ((and (listp error-obj) (alist-get 'status error-obj))
              (format "%s (%s)"
                      (or (alist-get 'message error-obj) "Unknown error")
                      (alist-get 'status error-obj)))
             ;; Simple string error
             ((stringp error-obj) error-obj)
             ;; Fallback: try top-level message
             ((alist-get 'message json-response)
              (alist-get 'message json-response))))
        (error nil)))))

(defun ollama-buddy-remote--handle-http-error (start-point error-details)
  "Handle an HTTP-level error in the chat buffer.
START-POINT is where the response content starts.
ERROR-DETAILS is the error plist from url-retrieve."
  (let* ((http-code (and (listp error-details)
                         (eq (car error-details) 'error)
                         (eq (cadr error-details) 'http)
                         (caddr error-details)))
         (status-msg (when http-code
                       (ollama-buddy-remote--http-status-message http-code)))
         (body-msg (ollama-buddy-remote--extract-error-body)))
    (with-current-buffer ollama-buddy--chat-buffer
      (let ((inhibit-read-only t))
        (goto-char start-point)
        (delete-region start-point (point-max))
        (if http-code
            (progn
              (insert (format "Error: HTTP %s\n\n" http-code))
              (insert status-msg "\n")
              (when body-msg
                (insert "\nProvider message: " body-msg "\n"))
              (insert "\nRaw: " (prin1-to-string error-details) "\n"))
          (insert "Error: URL retrieval failed\n\n")
          (when body-msg
            (insert "Provider message: " body-msg "\n"))
          (insert "Details: " (prin1-to-string error-details) "\n"))
        (insert "\n\n*** FAILED")
        (ollama-buddy--prepare-prompt-area)
        (ollama-buddy--update-status
         (if http-code
             (format "Failed - HTTP %s" http-code)
           "Failed - URL retrieval error"))))))

(defun ollama-buddy-remote--friendly-fetch-error (status provider-name)
  "Format a friendly error message for a failed model fetch.
STATUS is the url-retrieve status plist.  PROVIDER-NAME is the provider."
  (let* ((err (plist-get status :error))
         (http-code (and (listp err)
                         (eq (car err) 'error)
                         (eq (cadr err) 'http)
                         (caddr err)))
         (status-msg (when http-code
                       (ollama-buddy-remote--http-status-message http-code)))
         (body-msg (ollama-buddy-remote--extract-error-body))
         (friendly (or body-msg
                       status-msg
                       (prin1-to-string err))))
    (message "Error fetching %s models: %s" provider-name friendly)
    (ollama-buddy--update-status
     (format "Failed to fetch %s models%s"
             provider-name
             (if http-code (format " (HTTP %s)" http-code) "")))))

(defun ollama-buddy-remote--format-api-error (error-obj)
  "Format an API error object ERROR-OBJ into a readable string.
ERROR-OBJ is the value of the `error' key from a JSON API response.
Handles OpenAI, Claude, Gemini, and generic error formats."
  (cond
   ;; Object with message and optional type/code/status
   ((listp error-obj)
    (let ((msg (or (alist-get 'message error-obj) "Unknown error"))
          (type (alist-get 'type error-obj))
          (code (alist-get 'code error-obj))
          (status (alist-get 'status error-obj)))
      (concat msg
              (when (or type code status)
                (format " [%s]"
                        (string-join
                         (delq nil (list
                                    (when type (format "type: %s" type))
                                    (when code (format "code: %s" code))
                                    (when status (format "status: %s" status))))
                         ", "))))))
   ;; Simple string
   ((stringp error-obj) error-obj)
   ;; Fallback
   (t (prin1-to-string error-obj))))

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

;;; SSE Streaming infrastructure
;; ============================================================================

;; Forward declarations
(declare-function ollama-buddy--autosave-transcript "ollama-buddy")

;; Per-request streaming state (reset before each request)
(defvar ollama-buddy-remote--streaming-buffer ""
  "Accumulates partial SSE data from the curl process output.")

(defvar ollama-buddy-remote--streaming-start-point nil
  "Buffer position marker where streaming content is being inserted.")

(defvar ollama-buddy-remote--streaming-token-count 0
  "Number of content tokens received in the current streaming request.")

(defvar ollama-buddy-remote--streaming-response ""
  "Accumulated full response text for the current streaming request.")

(defvar ollama-buddy-remote--streaming-prompt nil
  "The original user prompt for the current streaming request.")

(defvar ollama-buddy-remote--streaming-headers-done nil
  "Non-nil once HTTP headers have been stripped from the curl output.")

(defvar ollama-buddy-remote--streaming-content-extractor nil
  "Function called with a `data:' SSE line to extract the token string.
Returns nil to skip the line (e.g. [DONE] or non-content events).")

(defvar ollama-buddy-remote--streaming-provider-name nil
  "Display name of the provider for the current streaming request.")

(defvar ollama-buddy-remote--streaming-temp-file nil
  "Path to the temp file for the current streaming request.")

(defvar ollama-buddy-remote--streaming-tool-call-acc nil
  "Hash table mapping tool call index to plist during streaming.
Each value is a plist (:id ID :type TYPE :name NAME :arguments ARGS-STRING).
Used to accumulate incremental tool_calls from OpenAI SSE chunks.")

(defvar ollama-buddy-remote--streaming-tool-continuation-p nil
  "Non-nil when the current streaming request is a tool continuation.")

(defvar ollama-buddy-remote--streaming-http-error nil
  "Non-nil when the streaming response had an HTTP error.
Contains the error body string for display.")

(defun ollama-buddy-remote--streaming-insert-content (content)
  "Insert CONTENT token into the chat buffer during streaming."
  (when (and content (not (string-empty-p content)))
    (setq ollama-buddy-remote--streaming-token-count
          (1+ ollama-buddy-remote--streaming-token-count))
    (setq ollama-buddy-remote--streaming-response
          (concat ollama-buddy-remote--streaming-response content))
    (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
      (with-current-buffer ollama-buddy--chat-buffer
        (let* ((inhibit-read-only t)
               (window (get-buffer-window ollama-buddy--chat-buffer t))
               (old-point (and window (window-point window)))
               (was-at-end (and window (>= old-point (point-max))))
               (old-window-start (and window (window-start window))))
          (save-excursion
            (goto-char (point-max))
            (insert content))
          (when window
            (cond
             ((and was-at-end ollama-buddy-auto-scroll)
              (set-window-point window (point-max)))
             (t
              (set-window-point window old-point)
              (set-window-start window old-window-start t)))))))))

(defun ollama-buddy-remote--streaming-finalize ()
  "Finalize a completed SSE streaming response in the chat buffer.
If tool calls were accumulated during streaming, executes them and
sends a continuation request through the same provider."
  (if ollama-buddy-remote--streaming-http-error
      ;; === HTTP ERROR PATH ===
      (let ((error-body ollama-buddy-remote--streaming-http-error))
        (setq ollama-buddy-remote--streaming-http-error nil
              ollama-buddy-remote--streaming-tool-call-acc nil
              ollama-buddy-remote--streaming-tool-continuation-p nil)
        ;; Try to extract a readable error message from the JSON body
        (let ((error-msg
               (condition-case nil
                   (let* ((json-object-type 'alist)
                          (json-key-type 'symbol)
                          (json-data (json-read-from-string
                                      (string-trim error-body)))
                          (err-obj (alist-get 'error json-data)))
                     (if (stringp err-obj)
                         err-obj
                       (or (alist-get 'message err-obj)
                           (format "%s" err-obj))))
                 (error (string-trim error-body)))))
          (message "ollama-buddy: API error: %s" error-msg)
          (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
            (with-current-buffer ollama-buddy--chat-buffer
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert (format "\n\nAPI Error: %s\n" error-msg))
                (ollama-buddy--prepare-prompt-area)
                (ollama-buddy--update-status "API error"))))))

    ;; === NORMAL/TOOL PATH ===
    (let* ((content ollama-buddy-remote--streaming-response)
           (start-point ollama-buddy-remote--streaming-start-point)
           (prompt ollama-buddy-remote--streaming-prompt)
           (token-count ollama-buddy-remote--streaming-token-count)
           (elapsed-time (if ollama-buddy-remote--request-start-time
                             (- (float-time) ollama-buddy-remote--request-start-time)
                           0))
           (token-rate (if (> elapsed-time 0) (/ token-count elapsed-time) 0))
           (tool-calls (ollama-buddy-remote--build-tool-calls-from-acc))
           (tool-continuation-p ollama-buddy-remote--streaming-tool-continuation-p))

      ;; Branch: tool calls detected vs normal completion
      (if (and tool-calls
               (featurep 'ollama-buddy-tools)
               (bound-and-true-p ollama-buddy-tools-enabled))
          ;; === TOOL CALL PATH ===
          ;; Clean up accumulator before the handler starts a new request.
          ;; Do NOT reset --streaming-tool-continuation-p here because
          ;; --handle-tool-calls starts a new async request that sets
          ;; fresh streaming state — resetting after would clobber it.
          (progn
            (setq ollama-buddy-remote--streaming-tool-call-acc nil)
            (ollama-buddy-remote--handle-tool-calls
             tool-calls content start-point prompt tool-continuation-p))

        ;; === NORMAL COMPLETION PATH ===
        (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
          (with-current-buffer ollama-buddy--chat-buffer
            (let* ((inhibit-read-only t)
                   (window (get-buffer-window ollama-buddy--chat-buffer t)))
              ;; Convert markdown to org in-place if enabled
              (when (and ollama-buddy-convert-markdown-to-org start-point)
                (ollama-buddy--md-to-org-convert-region start-point (point-max)))
              ;; Write to register
              (let* ((reg-char ollama-buddy-default-register)
                     (current (get-register reg-char))
                     (new-content (concat (if (stringp current) current "")
                                          content)))
                (set-register reg-char new-content))
              ;; Add to history (skip for tool continuations — already in history)
              (when (and ollama-buddy-history-enabled (not tool-continuation-p))
                (ollama-buddy--add-to-history "user" prompt)
                (ollama-buddy--add-to-history "assistant" content))
              (when (and ollama-buddy-history-enabled tool-continuation-p)
                (ollama-buddy--add-to-history "assistant" content))
              ;; Record token usage
              (push (list :model ollama-buddy--current-model
                          :tokens token-count
                          :elapsed elapsed-time
                          :rate token-rate
                          :wait-time ollama-buddy--response-wait-duration
                          :timestamp (current-time))
                    ollama-buddy--token-usage-history)
              (ollama-buddy--trim-token-history)
              ;; Insert property drawer on the response heading
              (ollama-buddy--insert-response-properties
               token-count elapsed-time token-rate
               ollama-buddy--response-wait-duration)
              (ollama-buddy--prepare-prompt-area)
              (setq ollama-buddy-remote--request-start-time nil)
              (ollama-buddy--maybe-goto-prompt window start-point)
              (ollama-buddy--update-status
               (format "Finished [%d tokens in %.1fs, %.1f t/s]"
                       token-count elapsed-time token-rate)))))
        ;; Auto-save transcript
        (when (fboundp 'ollama-buddy--autosave-transcript)
          (ollama-buddy--autosave-transcript))
        ;; Clean up — only in normal path, not after tool handler
        (setq ollama-buddy-remote--streaming-tool-call-acc nil
              ollama-buddy-remote--streaming-tool-continuation-p nil)))))

(defun ollama-buddy-remote--handle-tool-calls
    (tool-calls content start-point prompt tool-continuation-p)
  "Handle tool calls from a remote provider streaming response.
TOOL-CALLS is the list of accumulated tool call alists.
CONTENT is any text content from the response.
START-POINT is the buffer position where the response began.
PROMPT is the original user prompt.
TOOL-CONTINUATION-P is non-nil if this is already a continuation."
  (let* ((model ollama-buddy--current-model)
         (max-iterations (if (boundp 'ollama-buddy-tools-max-iterations)
                             ollama-buddy-tools-max-iterations 10))
         (iteration (or (bound-and-true-p ollama-buddy--tool-call-iteration) 0)))

    (if (>= iteration max-iterations)
        ;; Iteration limit reached — stop
        (progn
          (ollama-buddy--update-status "Tool call iteration limit reached")
          (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
            (with-current-buffer ollama-buddy--chat-buffer
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert "\n\n[Tool call iteration limit reached]\n")
                (ollama-buddy--prepare-prompt-area)))))

      ;; Increment iteration counter
      (setq ollama-buddy--tool-call-iteration (1+ iteration))

      ;; Add to history: user message (if not continuation) + assistant with tool_calls
      (when (and ollama-buddy-history-enabled (not tool-continuation-p))
        (ollama-buddy--add-to-history "user" prompt))
      (when ollama-buddy-history-enabled
        (ollama-buddy--add-to-history "assistant" (or content "") tool-calls))

      ;; Execute tool calls and build OpenAI-format result messages
      (let* ((tool-results
              (ollama-buddy-remote--execute-tool-calls tool-calls))
             ;; Convert tool-calls to the format expected by rebuild-tool-batch
             ;; rebuild-tool-batch expects: ((function . ((name . N) (arguments . A))))
             (batch-tool-calls
              (mapcar (lambda (tc)
                        `((function . ,(alist-get 'function tc))))
                      tool-calls))
             ;; Convert results to format for rebuild-tool-batch
             ;; rebuild-tool-batch expects: ((content . RESULT-STRING))
             (batch-tool-results
              (mapcar (lambda (r)
                        `((content . ,(alist-get 'content r))))
                      tool-results)))

        ;; Rebuild buffer with structured tool batch display
        (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
          (with-current-buffer ollama-buddy--chat-buffer
            (let ((inhibit-read-only t))
              (ollama-buddy--rebuild-tool-batch
               start-point model batch-tool-calls batch-tool-results
               nil  ; thinking-content
               (when (and content (not (string-empty-p content))) content)))))

        ;; Add tool results to history (OpenAI format with tool_call_id)
        (when ollama-buddy-history-enabled
          (dolist (result tool-results)
            (ollama-buddy--add-to-history-raw result)))

        ;; Reset request start time
        (setq ollama-buddy-remote--request-start-time nil)

        ;; Send continuation via dispatch (will route to correct provider)
        (if (and (boundp 'ollama-buddy-tools--stop-after-batch)
                 ollama-buddy-tools--stop-after-batch)
            (progn
              (setq ollama-buddy-tools--stop-after-batch nil)
              (setq ollama-buddy--suppress-tools-once t)
              (ollama-buddy--send
               "continue, I will manually apply the changes using ediff, please just supply a brief summary of what was changed"
               model nil))
          (ollama-buddy--send nil model t))))))

(defun ollama-buddy-remote--execute-tool-calls (tool-calls)
  "Execute TOOL-CALLS and return OpenAI-format tool result messages.
Each result has `role', `tool_call_id', and `content' keys."
  (let (results)
    (dolist (call tool-calls)
      (let* ((id (alist-get 'id call))
             (func (alist-get 'function call))
             (name (alist-get 'name func))
             (arguments (alist-get 'arguments func))
             (result (ollama-buddy-tools--execute name arguments)))
        (push `((role . "tool")
                (tool_call_id . ,id)
                (content . ,result))
              results)))
    (nreverse results)))

(defun ollama-buddy-remote--streaming-process-filter (proc output)
  "Process filter for SSE streaming curl processes.
PROC is the curl process, OUTPUT is the new chunk of text."
  (let ((proc-buffer (process-buffer proc)))
    (when (and proc-buffer (buffer-live-p proc-buffer))
      (condition-case err
          (with-current-buffer proc-buffer
            ;; Append new output
            (goto-char (point-max))
            (insert output)
            ;; Strip HTTP headers on first call
            (unless ollama-buddy-remote--streaming-headers-done
              (goto-char (point-min))
              (when (re-search-forward "\r?\n\r?\n" nil t)
                (let ((headers (buffer-substring (point-min) (point))))
                  ;; Check HTTP status code
                  (when (string-match "^HTTP/[0-9.]+ \\([0-9]+\\)" headers)
                    (let ((status-code (string-to-number (match-string 1 headers))))
                      (unless (and (>= status-code 200) (< status-code 300))
                        (message "ollama-buddy: HTTP %d error from remote API" status-code)
                        ;; Capture remaining body as error
                        (setq ollama-buddy-remote--streaming-http-error
                              (buffer-substring (point) (point-max))))))
                  (delete-region (point-min) (point))
                  (setq ollama-buddy-remote--streaming-headers-done t))))
            ;; Process complete SSE lines (skip if HTTP error detected)
            (when (and ollama-buddy-remote--streaming-headers-done
                       (not ollama-buddy-remote--streaming-http-error))
              (goto-char (point-min))
              (while (re-search-forward "^\\(.*\\)\n" nil t)
                (let ((line (match-string 1)))
                  (delete-region (match-beginning 0) (match-end 0))
                  (when (string-prefix-p "data: " line)
                    (let* ((data-str (string-trim (substring line 6)))
                           (content (condition-case err
                                        (funcall ollama-buddy-remote--streaming-content-extractor
                                                 data-str)
                                      (error
                                       (message "ollama-buddy: extractor error: %s" err)
                                       nil))))
                      (when content
                        (ollama-buddy-remote--streaming-insert-content content))))))))
        (error
         (message "Error in remote streaming filter: %s" (error-message-string err)))))))

(defun ollama-buddy-remote--streaming-sentinel (proc event)
  "Sentinel for SSE streaming curl processes.
PROC is the curl process, EVENT is the status event string."
  (condition-case err
      (let ((proc-buffer (process-buffer proc))
            (temp-file (process-get proc :temp-file)))
        ;; Capture full error body before killing buffer
        (when (and ollama-buddy-remote--streaming-http-error
                   proc-buffer (buffer-live-p proc-buffer))
          (with-current-buffer proc-buffer
            (setq ollama-buddy-remote--streaming-http-error
                  (buffer-string))))
        ;; Clean up process buffer
        (when (and proc-buffer (buffer-live-p proc-buffer))
          (kill-buffer proc-buffer))
        ;; Clean up temp file
        (when (and temp-file (file-exists-p temp-file))
          (ignore-errors (delete-file temp-file)))
        (cond
         ((string-match-p "finished" event)
          ;; Process complete - finalize the response
          (ollama-buddy-remote--streaming-finalize))
         ((string-match-p "\\(?:killed\\|terminated\\)" event)
          (ollama-buddy--update-status "Request cancelled")
          (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
            (with-current-buffer ollama-buddy--chat-buffer
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert "\n\n*** CANCELLED")
                (ollama-buddy--prepare-prompt-area)))))
         (t
          (ollama-buddy--update-status
           (format "Remote streaming failed: %s" event))
          (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
            (with-current-buffer ollama-buddy--chat-buffer
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert (format "\n\nError: streaming failed: %s" event))
                (insert "\n\n*** FAILED")
                (ollama-buddy--prepare-prompt-area)))))))
    (error
     (message "Error in remote streaming sentinel: %s" (error-message-string err)))))

(defun ollama-buddy-remote--start-streaming-request
    (endpoint headers json-str content-extractor provider-name prompt start-point)
  "Start an SSE streaming curl request.
ENDPOINT is the API URL string.
HEADERS is an alist of HTTP header pairs.
JSON-STR is the JSON request body.
CONTENT-EXTRACTOR is the function to call on each `data:' SSE line.
PROVIDER-NAME is the display name for status messages.
PROMPT is the user prompt string (for history).
START-POINT is the buffer position where content should be inserted."
  ;; Reset streaming state
  (setq ollama-buddy-remote--streaming-buffer ""
        ollama-buddy-remote--streaming-headers-done nil
        ollama-buddy-remote--streaming-token-count 0
        ollama-buddy-remote--streaming-response ""
        ollama-buddy-remote--streaming-prompt prompt
        ollama-buddy-remote--streaming-start-point start-point
        ollama-buddy-remote--streaming-content-extractor content-extractor
        ollama-buddy-remote--streaming-provider-name provider-name
        ollama-buddy-remote--streaming-tool-call-acc nil
        ollama-buddy-remote--streaming-http-error nil)
  ;; Kill any existing active process
  (when (and ollama-buddy--active-process
             (process-live-p ollama-buddy--active-process))
    (delete-process ollama-buddy--active-process)
    (setq ollama-buddy--active-process nil))
  ;; Write payload to temp file
  (let* ((temp-file (make-temp-file "ollama-buddy-remote-payload"))
         (process-name (format "ollama-remote-stream-%s" provider-name))
         (process-buffer (generate-new-buffer
                          (format " *%s*" process-name)))
         ;; Build header args list
         (header-args (apply #'append
                             (mapcar (lambda (h)
                                       (list "--header"
                                             (format "%s: %s" (car h) (cdr h))))
                                     headers)))
         (curl-args (append
                     (list "--silent"
                           "--no-buffer"
                           "--include"
                           "--max-time" (number-to-string ollama-buddy-curl-timeout)
                           "--request" "POST")
                     header-args
                     (list "--data-binary" (concat "@" temp-file)
                           endpoint))))
    (with-temp-file temp-file
      (insert json-str))
    ;; Remove "Loading response..." placeholder
    (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
      (with-current-buffer ollama-buddy--chat-buffer
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char start-point)
            (when (looking-at "Loading response\\.\\.\\.")
              (delete-region start-point (+ start-point (length "Loading response..."))))))))
    ;; Start the process
    (condition-case err
        (let ((proc (apply #'start-process process-name process-buffer
                           ollama-buddy-curl-executable curl-args)))
          (process-put proc :temp-file temp-file)
          (set-process-filter proc #'ollama-buddy-remote--streaming-process-filter)
          (set-process-sentinel proc #'ollama-buddy-remote--streaming-sentinel)
          (setq ollama-buddy--active-process proc)
          (ollama-buddy--update-status
           (format "Streaming from %s..." provider-name)))
      (error
       (when (file-exists-p temp-file)
         (delete-file temp-file))
       (when (buffer-live-p process-buffer)
         (kill-buffer process-buffer))
       (ollama-buddy--update-status
        (format "Failed to start %s stream" provider-name))
       (error "Failed to start streaming curl: %s" (error-message-string err))))))

;;; SSE content extractors
;; ============================================================================

(defun ollama-buddy-remote--openai-extract-content (data-str)
  "Extract content token from an OpenAI-compatible SSE DATA-STR.
Returns the token string, or nil if the line should be skipped."
  (when (and data-str
             (not (string= data-str "[DONE]"))
             (not (string-empty-p (string-trim data-str))))
    (condition-case nil
        (let* ((json-object-type 'alist)
               (json-array-type 'vector)
               (json-key-type 'symbol)
               (json-data (json-read-from-string data-str))
               (choices (alist-get 'choices json-data))
               (delta (when (and choices (> (length choices) 0))
                        (alist-get 'delta (aref choices 0))))
               (content (when delta (alist-get 'content delta))))
          (when (and content (not (eq content :null)))
            content))
      (error nil))))

(defun ollama-buddy-remote--openai-extract-content-with-tools (data-str)
  "Extract content and accumulate tool calls from OpenAI SSE DATA-STR.
Like `ollama-buddy-remote--openai-extract-content' but also detects
incremental `delta.tool_calls' and accumulates them in
`ollama-buddy-remote--streaming-tool-call-acc'."
  (when (and data-str
             (not (string= data-str "[DONE]"))
             (not (string-empty-p (string-trim data-str))))
    (condition-case err
        (let* ((json-object-type 'alist)
               (json-array-type 'vector)
               (json-key-type 'symbol)
               (json-data (json-read-from-string data-str))
               (choices (alist-get 'choices json-data))
               (choice (when (and choices (> (length choices) 0))
                         (aref choices 0)))
               (delta (when choice (alist-get 'delta choice)))
               (content (when delta (alist-get 'content delta)))
               (tool-calls-delta (when delta
                                   (alist-get 'tool_calls delta))))
          ;; Accumulate incremental tool calls
          (when tool-calls-delta
            (unless ollama-buddy-remote--streaming-tool-call-acc
              (setq ollama-buddy-remote--streaming-tool-call-acc
                    (make-hash-table :test 'eql)))
            (seq-doseq (tc (if (vectorp tool-calls-delta)
                               tool-calls-delta
                             (list tool-calls-delta)))
              (let* ((index (alist-get 'index tc))
                     (existing (gethash index
                                        ollama-buddy-remote--streaming-tool-call-acc))
                     (id (alist-get 'id tc))
                     (type (alist-get 'type tc))
                     (func-delta (alist-get 'function tc))
                     (name (when func-delta (alist-get 'name func-delta)))
                     (args (when func-delta
                             (alist-get 'arguments func-delta))))
                (if existing
                    ;; Append to existing arguments buffer
                    (when (and args (not (string-empty-p args)))
                      (plist-put existing :arguments
                                 (concat (or (plist-get existing :arguments)
                                             "")
                                         args)))
                  ;; New tool call entry
                  (puthash index
                           (list :id (or id "")
                                 :type (or type "function")
                                 :name (or name "")
                                 :arguments (or args ""))
                           ollama-buddy-remote--streaming-tool-call-acc)))))
          ;; Return content token (may be nil for tool-call-only chunks)
          (when (and content (not (eq content :null)))
            content))
      (error
       (message "ollama-buddy: tools extractor error: %s on data: %.100s"
                (error-message-string err) data-str)
       nil))))

(defun ollama-buddy-remote--build-tool-calls-from-acc ()
  "Build a list of tool call alists from the streaming accumulator.
Returns a list in OpenAI format suitable for history and processing:
  ((id . ID) (type . TYPE)
   (function . ((name . NAME) (arguments . ARGS-JSON-STRING))))"
  (when ollama-buddy-remote--streaming-tool-call-acc
    (let (result)
      (maphash
       (lambda (_index plist)
         (push `((id . ,(plist-get plist :id))
                 (type . ,(plist-get plist :type))
                 (function . ((name . ,(plist-get plist :name))
                              (arguments . ,(plist-get plist :arguments)))))
               result))
       ollama-buddy-remote--streaming-tool-call-acc)
      ;; Sort by collected order (hash iteration order is not guaranteed)
      (sort result (lambda (a b)
                     (string< (or (alist-get 'id a) "")
                              (or (alist-get 'id b) "")))))))

(defun ollama-buddy-remote--claude-extract-content (data-str)
  "Extract content token from a Claude SSE DATA-STR.
Returns the token string, or nil if the line should be skipped."
  (when (and data-str
             (not (string-empty-p (string-trim data-str))))
    (condition-case nil
        (let* ((json-object-type 'alist)
               (json-array-type 'vector)
               (json-key-type 'symbol)
               (json-data (json-read-from-string data-str))
               (type (alist-get 'type json-data))
               (delta (alist-get 'delta json-data))
               (text (when delta (alist-get 'text delta))))
          (when (and (string= type "content_block_delta") text
                     (not (eq text :null)))
            text))
      (error nil))))

(defun ollama-buddy-remote--claude-extract-content-with-tools (data-str)
  "Extract content and accumulate tool calls from Claude SSE DATA-STR.
Handles `content_block_start' (tool_use), `content_block_delta'
\(text_delta and input_json_delta), and other event types.
Accumulates tool calls in `ollama-buddy-remote--streaming-tool-call-acc'."
  (when (and data-str
             (not (string-empty-p (string-trim data-str))))
    (condition-case err
        (let* ((json-object-type 'alist)
               (json-array-type 'vector)
               (json-key-type 'symbol)
               (json-data (json-read-from-string data-str))
               (type (alist-get 'type json-data)))
          (cond
           ;; content_block_start with tool_use: register new tool call
           ((string= type "content_block_start")
            (let* ((index (alist-get 'index json-data))
                   (block (alist-get 'content_block json-data))
                   (block-type (when block (alist-get 'type block))))
              (when (and block-type (string= block-type "tool_use"))
                (unless ollama-buddy-remote--streaming-tool-call-acc
                  (setq ollama-buddy-remote--streaming-tool-call-acc
                        (make-hash-table :test 'eql)))
                (puthash index
                         (list :id (or (alist-get 'id block) "")
                               :type "function"
                               :name (or (alist-get 'name block) "")
                               :arguments "")
                         ollama-buddy-remote--streaming-tool-call-acc)))
            nil)

           ;; content_block_delta: text or input_json
           ((string= type "content_block_delta")
            (let* ((index (alist-get 'index json-data))
                   (delta (alist-get 'delta json-data))
                   (delta-type (when delta (alist-get 'type delta))))
              (cond
               ;; Text delta — return for display
               ((and delta-type (string= delta-type "text_delta"))
                (let ((text (alist-get 'text delta)))
                  (when (and text (not (eq text :null)))
                    text)))
               ;; Tool input JSON delta — accumulate
               ((and delta-type (string= delta-type "input_json_delta"))
                (let ((partial (alist-get 'partial_json delta)))
                  (when (and partial
                             ollama-buddy-remote--streaming-tool-call-acc)
                    (let ((existing (gethash index
                                            ollama-buddy-remote--streaming-tool-call-acc)))
                      (when existing
                        (plist-put existing :arguments
                                   (concat (plist-get existing :arguments)
                                           partial)))))
                  nil))
               (t nil))))

           ;; All other event types: skip
           (t nil)))
      (error
       (message "ollama-buddy: claude tools extractor error: %s on data: %.100s"
                (error-message-string err) data-str)
       nil))))

(defun ollama-buddy-remote--convert-schema-to-claude (openai-schema)
  "Convert OPENAI-SCHEMA vector to Claude tool format.
OpenAI: [{type: function, function: {name, description, parameters}}]
Claude: [{name, description, input_schema}]"
  (vconcat
   (mapcar
    (lambda (tool)
      (let* ((func (alist-get 'function tool))
             (name (alist-get 'name func))
             (desc (alist-get 'description func))
             (params (alist-get 'parameters func)))
        `((name . ,name)
          (description . ,desc)
          (input_schema . ,params))))
    (append openai-schema nil))))

(defun ollama-buddy-remote--convert-history-to-claude (history)
  "Convert OpenAI-format HISTORY entries to Claude Messages API format.
Handles assistant messages with tool_calls and tool result messages."
  (let (result)
    (dolist (msg history)
      (let ((role (alist-get 'role msg))
            (content (alist-get 'content msg))
            (tool-calls (alist-get 'tool_calls msg))
            (tool-call-id (alist-get 'tool_call_id msg)))
        (cond
         ;; Assistant message with tool_calls → Claude content blocks
         ((and (string= role "assistant") tool-calls)
          (let ((blocks nil))
            ;; Add text block if there's content
            (when (and content (stringp content) (not (string-empty-p content)))
              (push `((type . "text") (text . ,content)) blocks))
            ;; Add tool_use blocks
            (seq-doseq (tc (if (vectorp tool-calls) tool-calls
                             (vconcat tool-calls)))
              (let* ((id (alist-get 'id tc))
                     (func (alist-get 'function tc))
                     (name (alist-get 'name func))
                     (args-raw (alist-get 'arguments func))
                     (input (if (stringp args-raw)
                                (condition-case nil
                                    (json-read-from-string args-raw)
                                  (error args-raw))
                              (or args-raw (make-hash-table)))))
                (push `((type . "tool_use")
                        (id . ,id)
                        (name . ,name)
                        (input . ,input))
                      blocks)))
            (push `((role . "assistant")
                    (content . ,(vconcat (nreverse blocks))))
                  result)))

         ;; Tool result message → Claude tool_result
         ((and (string= role "tool") tool-call-id)
          (push `((role . "user")
                  (content . ,(vector
                               `((type . "tool_result")
                                 (tool_use_id . ,tool-call-id)
                                 (content . ,(or content ""))))))
                result))

         ;; Regular message — pass through
         (t (push msg result)))))
    (nreverse result)))

(defun ollama-buddy-remote--gemini-extract-content (data-str)
  "Extract content token from a Gemini SSE DATA-STR.
Returns the token string, or nil if the line should be skipped."
  (when (and data-str
             (not (string-empty-p (string-trim data-str))))
    (condition-case nil
        (let* ((json-object-type 'alist)
               (json-array-type 'vector)
               (json-key-type 'symbol)
               (json-data (json-read-from-string data-str))
               (candidates (alist-get 'candidates json-data))
               (first (when (and candidates (> (length candidates) 0))
                        (aref candidates 0)))
               (content (when first (alist-get 'content first)))
               (parts (when content (alist-get 'parts content)))
               (text (when (and parts (> (length parts) 0))
                       (alist-get 'text (aref parts 0)))))
          (when (and text (not (eq text :null)))
            text))
      (error nil))))

(defun ollama-buddy-remote--gemini-extract-content-with-tools (data-str)
  "Extract content and accumulate tool calls from Gemini SSE DATA-STR.
Gemini returns functionCall parts in `candidates[0].content.parts'.
Each functionCall has `name' and `args' (already a JSON object)."
  (when (and data-str
             (not (string-empty-p (string-trim data-str))))
    (condition-case err
        (let* ((json-object-type 'alist)
               (json-array-type 'vector)
               (json-key-type 'symbol)
               (json-data (json-read-from-string data-str))
               (candidates (alist-get 'candidates json-data))
               (first (when (and candidates (> (length candidates) 0))
                        (aref candidates 0)))
               (content (when first (alist-get 'content first)))
               (parts (when content (alist-get 'parts content)))
               (text-result nil))
          ;; Process all parts — may contain text and/or functionCall
          (when parts
            (dotimes (i (length parts))
              (let* ((part (aref parts i))
                     (text (alist-get 'text part))
                     (func-call (alist-get 'functionCall part)))
                ;; Text part
                (when (and text (not (eq text :null)))
                  (setq text-result (concat (or text-result "") text)))
                ;; Function call part — accumulate
                (when func-call
                  (unless ollama-buddy-remote--streaming-tool-call-acc
                    (setq ollama-buddy-remote--streaming-tool-call-acc
                          (make-hash-table :test 'eql)))
                  (let* ((name (alist-get 'name func-call))
                         (args (alist-get 'args func-call))
                         (args-str (if (and args (not (eq args :null)))
                                       (json-encode args)
                                     "{}"))
                         ;; Generate a synthetic ID (Gemini doesn't provide one)
                         (id (format "gemini_%s_%d" name
                                     (hash-table-count
                                      ollama-buddy-remote--streaming-tool-call-acc))))
                    (puthash (hash-table-count
                              ollama-buddy-remote--streaming-tool-call-acc)
                             (list :id id
                                   :type "function"
                                   :name name
                                   :arguments args-str)
                             ollama-buddy-remote--streaming-tool-call-acc))))))
          text-result)
      (error
       (message "ollama-buddy: gemini tools extractor error: %s on data: %.100s"
                (error-message-string err) data-str)
       nil))))

(defun ollama-buddy-remote--convert-schema-to-gemini (openai-schema)
  "Convert OPENAI-SCHEMA vector to Gemini tool format.
OpenAI: [{type: function, function: {name, description, parameters}}]
Gemini: [{functionDeclarations: [{name, description, parameters}]}]"
  (vector
   `((functionDeclarations
      . ,(vconcat
          (mapcar
           (lambda (tool)
             (let* ((func (alist-get 'function tool))
                    (name (alist-get 'name func))
                    (desc (alist-get 'description func))
                    (params (alist-get 'parameters func)))
               `((name . ,name)
                 (description . ,desc)
                 (parameters . ,params))))
           (append openai-schema nil)))))))

(defun ollama-buddy-remote--convert-history-to-gemini (history)
  "Convert OpenAI-format HISTORY entries to Gemini API format.
Handles assistant messages with tool_calls and tool result messages.
Gemini uses role `model' for assistant, `user' for function results."
  (let (result)
    (dolist (msg history)
      (let ((role (alist-get 'role msg))
            (content (alist-get 'content msg))
            (tool-calls (alist-get 'tool_calls msg))
            (tool-call-id (alist-get 'tool_call_id msg)))
        (cond
         ;; Assistant message with tool_calls → model with functionCall parts
         ((and (string= role "assistant") tool-calls)
          (let ((parts nil))
            ;; Add text part if there's content
            (when (and content (stringp content) (not (string-empty-p content)))
              (push `((text . ,content)) parts))
            ;; Add functionCall parts
            (seq-doseq (tc (if (vectorp tool-calls) tool-calls
                             (vconcat tool-calls)))
              (let* ((func (alist-get 'function tc))
                     (name (alist-get 'name func))
                     (args-raw (alist-get 'arguments func))
                     (args (if (stringp args-raw)
                               (condition-case nil
                                   (json-read-from-string args-raw)
                                 (error (make-hash-table)))
                             (or args-raw (make-hash-table)))))
                (push `((functionCall . ((name . ,name)
                                         (args . ,args))))
                      parts)))
            (push `((role . "model")
                    (parts . ,(vconcat (nreverse parts))))
                  result)))

         ;; Tool result message → functionResponse
         ((and (string= role "tool") tool-call-id)
          ;; Extract tool name from the ID (gemini_NAME_N format)
          ;; or fall back to "unknown"
          (let ((name (if (and tool-call-id
                              (string-match "^gemini_\\(.+\\)_[0-9]+$"
                                            tool-call-id))
                          (match-string 1 tool-call-id)
                        "unknown")))
            (push `((role . "user")
                    (parts . ,(vector
                               `((functionResponse
                                  . ((name . ,name)
                                     (response . ((content . ,(or content ""))))))))))
                  result)))

         ;; System message → user with instruction prefix
         ((string= role "system")
          (push `((role . "user")
                  (parts . ,(vector `((text . ,(format "[System Instruction] %s"
                                                       content))))))
                result))

         ;; Regular user message
         ((string= role "user")
          (push `((role . "user")
                  (parts . ,(vector `((text . ,content)))))
                result))

         ;; Regular assistant message
         ((string= role "assistant")
          (push `((role . "model")
                  (parts . ,(vector `((text . ,(or content ""))))))
                result))

         ;; Anything else — pass through
         (t (push msg result)))))
    (nreverse result)))

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
  :token-count-var - symbol for the token count variable
  :tools-schema  - tools schema vector (optional, for tool calling)"
  ;; Process inline features asynchronously, then send
  (ollama-buddy-remote--process-inline-features-async
   prompt
   (lambda (processed-prompt)
     (ollama-buddy-remote--openai-send-payload
      processed-prompt model config))))

(defun ollama-buddy-remote--openai-send-payload (prompt model config)
  "Build and send the OpenAI-compatible payload for PROMPT.
MODEL and CONFIG are passed through from `ollama-buddy-remote--openai-send'."
  (let* ((prefix (plist-get config :prefix))
         (api-key (plist-get config :api-key))
         (endpoint (plist-get config :endpoint))
         (temperature (plist-get config :temperature))
         (max-tokens (or (plist-get config :max-tokens) 4096))
         (default-model (plist-get config :default-model))
         (provider-name (plist-get config :provider-name))
         (extra-headers (plist-get config :extra-headers))
         (token-count-var (plist-get config :token-count-var))
         (tools-schema (plist-get config :tools-schema))
         (tool-continuation-p (bound-and-true-p
                                ollama-buddy-remote--tool-continuation-p)))



    ;; Set up the current model
    (setq ollama-buddy--current-model
          (or model
              ollama-buddy--current-model
              (ollama-buddy-remote--get-full-model-name prefix default-model)))

    ;; Initialize token counter
    (set token-count-var 0)

    ;; Reset tool iteration counter for new requests
    (unless tool-continuation-p
      (setq ollama-buddy--tool-call-iteration 0))

    ;; Get history and system prompt
    (let* ((history (when ollama-buddy-history-enabled
                      (gethash ollama-buddy--current-model
                               ollama-buddy--conversation-history-by-model
                               nil)))
           (system-prompt (ollama-buddy--effective-system-prompt))
           (full-context (ollama-buddy-remote--build-context))
           ;; For tool continuations, use history only (no new user message)
           (messages (if tool-continuation-p
                        (vconcat []
                                 (append
                                  (when (and system-prompt
                                             (not (string-empty-p system-prompt)))
                                    `(((role . "system")
                                       (content . ,system-prompt))))
                                  history))
                      (ollama-buddy-remote--build-openai-messages
                       system-prompt history prompt full-context)))
           (json-payload
            `((model . ,(ollama-buddy-remote--get-real-model-name
                         prefix ollama-buddy--current-model))
              (messages . ,messages)
              (temperature . ,temperature)
              (max_tokens . ,max-tokens)))
           ;; Add tools schema if provided
           (json-payload
            (if tools-schema
                (append json-payload `((tools . ,tools-schema)))
              json-payload))
           (stream-json-payload
            (if ollama-buddy-streaming-enabled
                (append json-payload '((stream . t)))
              json-payload))
           (json-str (let ((json-encoding-pretty-print nil))
                       (ollama-buddy-escape-unicode (json-encode stream-json-payload))))
           (start-point (ollama-buddy-remote--prepare-chat-buffer provider-name))
           (all-headers (append `(("Content-Type" . "application/json")
                                  ("Authorization" . ,(concat "Bearer " api-key)))
                                extra-headers))
           ;; Use tools-aware extractor when tools schema is present
           (extractor (if tools-schema
                         #'ollama-buddy-remote--openai-extract-content-with-tools
                       #'ollama-buddy-remote--openai-extract-content)))

      ;; Store continuation state for the streaming finalize
      (setq ollama-buddy-remote--streaming-tool-continuation-p tool-continuation-p)

      (if ollama-buddy-streaming-enabled
          ;; Streaming path: use curl with SSE
          (ollama-buddy-remote--start-streaming-request
           endpoint
           all-headers
           json-str
           extractor
           provider-name
           prompt
           start-point)
        ;; Non-streaming path: use url-retrieve
        (let* ((url-request-method "POST")
               (url-request-extra-headers all-headers)
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
                                (choices (alist-get 'choices json-response))
                                (message-obj
                                 (when (and choices (> (length choices) 0))
                                   (alist-get 'message (aref choices 0))))
                                (resp-tool-calls
                                 (when message-obj
                                   (let ((tc (alist-get 'tool_calls message-obj)))
                                     (when tc (append tc nil))))))
                           ;; Extract the message content
                           (if error-message
                               (setq content (format "Error: %s"
                                                     (ollama-buddy-remote--format-api-error
                                                      error-message)))
                             (when message-obj
                               (setq content (or (alist-get 'content message-obj) ""))))
                           ;; Branch: tool calls vs normal
                           (if (and resp-tool-calls
                                    tools-schema
                                    (featurep 'ollama-buddy-tools)
                                    (bound-and-true-p ollama-buddy-tools-enabled))
                               (ollama-buddy-remote--handle-tool-calls
                                resp-tool-calls content start-point
                                prompt tool-continuation-p)
                             (ollama-buddy-remote--finalize-response
                              start-point content prompt token-count-var)))
                       (error
                        (ollama-buddy-remote--handle-error
                         start-point provider-name
                         (error-message-string err)))))))))))))))

(provide 'ollama-buddy-remote)
;;; ollama-buddy-remote.el ends here
