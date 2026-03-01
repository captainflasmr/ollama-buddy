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

        ;; Show token stats if enabled
        (when ollama-buddy-display-token-stats
          (insert (format "\n\n*** Token Stats\n[%d tokens in %.1fs, %.1f tokens/sec]"
                          token-count elapsed-time token-rate)))

        (insert "\n\n*** FINISHED")
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
  "Finalize a completed SSE streaming response in the chat buffer."
  (let* ((content ollama-buddy-remote--streaming-response)
         (start-point ollama-buddy-remote--streaming-start-point)
         (prompt ollama-buddy-remote--streaming-prompt)
         (token-count ollama-buddy-remote--streaming-token-count)
         (elapsed-time (if ollama-buddy-remote--request-start-time
                           (- (float-time) ollama-buddy-remote--request-start-time)
                         0))
         (token-rate (if (> elapsed-time 0) (/ token-count elapsed-time) 0)))
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
                 (new-content (concat (if (stringp current) current "") content)))
            (set-register reg-char new-content))
          ;; Add to history
          (when ollama-buddy-history-enabled
            (ollama-buddy--add-to-history "user" prompt)
            (ollama-buddy--add-to-history "assistant" content))
          ;; Record token usage
          (push (list :model ollama-buddy--current-model
                      :tokens token-count
                      :elapsed elapsed-time
                      :rate token-rate
                      :wait-time ollama-buddy--response-wait-duration
                      :timestamp (current-time))
                ollama-buddy--token-usage-history)
          ;; Show token stats if enabled
          (when ollama-buddy-display-token-stats
            (goto-char (point-max))
            (insert (format "\n\n*** Token Stats\n[%d tokens in %.1fs, %.1f tokens/sec]"
                            token-count elapsed-time token-rate)))
          (goto-char (point-max))
          (insert "\n\n*** FINISHED")
          (ollama-buddy--prepare-prompt-area)
          (setq ollama-buddy-remote--request-start-time nil)
          (ollama-buddy--maybe-goto-prompt window start-point)
          (ollama-buddy--update-status
           (format "Finished [%d tokens in %.1fs, %.1f t/s]"
                   token-count elapsed-time token-rate)))))
    ;; Auto-save transcript
    (when (fboundp 'ollama-buddy--autosave-transcript)
      (ollama-buddy--autosave-transcript))))

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
                (delete-region (point-min) (point))
                (setq ollama-buddy-remote--streaming-headers-done t)))
            ;; Process complete SSE lines
            (when ollama-buddy-remote--streaming-headers-done
              (goto-char (point-min))
              (while (re-search-forward "^\\(.*\\)\n" nil t)
                (let ((line (match-string 1)))
                  (delete-region (match-beginning 0) (match-end 0))
                  (when (string-prefix-p "data: " line)
                    (let* ((data-str (substring line 6))
                           (content (condition-case nil
                                        (funcall ollama-buddy-remote--streaming-content-extractor
                                                 data-str)
                                      (error nil))))
                      (when content
                        (ollama-buddy-remote--streaming-insert-content content))))))))
        (error
         (message "Error in remote streaming filter: %s" (error-message-string err)))))))

(defun ollama-buddy-remote--streaming-sentinel (proc event)
  "Sentinel for SSE streaming curl processes.
PROC is the curl process, EVENT is the status event string."
  (condition-case err
      (let ((proc-buffer (process-buffer proc)))
        ;; Clean up process buffer
        (when (and proc-buffer (buffer-live-p proc-buffer))
          (kill-buffer proc-buffer))
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
        ollama-buddy-remote--streaming-provider-name provider-name)
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
    ;; Schedule temp file cleanup
    (run-with-timer 2.0 nil (lambda ()
                              (when (file-exists-p temp-file)
                                (delete-file temp-file))))
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
           (stream-json-payload
            (if ollama-buddy-streaming-enabled
                (append json-payload '((stream . t)))
              json-payload))
           (json-str (let ((json-encoding-pretty-print nil))
                       (ollama-buddy-escape-unicode (json-encode stream-json-payload))))
           (start-point (ollama-buddy-remote--prepare-chat-buffer provider-name))
           (all-headers (append `(("Content-Type" . "application/json")
                                  ("Authorization" . ,(concat "Bearer " api-key)))
                                extra-headers)))

      (if ollama-buddy-streaming-enabled
          ;; Streaming path: use curl with SSE
          (ollama-buddy-remote--start-streaming-request
           endpoint
           all-headers
           json-str
           #'ollama-buddy-remote--openai-extract-content
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
                                (choices (alist-get 'choices json-response)))
                           ;; Extract the message content
                           (if error-message
                               (setq content (format "Error: %s"
                                                     (ollama-buddy-remote--format-api-error
                                                      error-message)))
                             (when choices
                               (setq content (alist-get 'content
                                                        (alist-get 'message (aref choices 0))))))
                           ;; Finalize the response
                           (ollama-buddy-remote--finalize-response
                            start-point content prompt token-count-var))
                       (error
                        (ollama-buddy-remote--handle-error
                         start-point provider-name
                         (error-message-string err)))))))))))))))

(provide 'ollama-buddy-remote)
;;; ollama-buddy-remote.el ends here
