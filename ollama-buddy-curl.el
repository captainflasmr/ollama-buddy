;;; ollama-buddy-curl.el --- Curl backend for ollama-buddy -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/captainflasmr/ollama-buddy
;;
;;; Commentary:
;; This file contains the curl backend implementation for ollama-buddy.
;; It provides an alternative communication method to the built-in network
;; process, useful for systems where network processes might not work properly.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'ollama-buddy-core)

;; Forward declarations for functions defined in other ollama-buddy files
;; Shared request helpers
(declare-function ollama-buddy--validate-send-request "ollama-buddy")
(declare-function ollama-buddy--process-inline-prompt "ollama-buddy")
(declare-function ollama-buddy--process-inline-prompt-async "ollama-buddy")
(declare-function ollama-buddy--build-chat-payload "ollama-buddy")
(declare-function ollama-buddy--setup-chat-send "ollama-buddy")
;; Shared streaming processor (used by both backends)
(declare-function ollama-buddy--stream-process-json "ollama-buddy")
;; Helpers still referenced directly from curl sentinel / non-streaming
(declare-function ollama-buddy--cancel-response-wait-timer "ollama-buddy")
(declare-function ollama-buddy--autosave-transcript "ollama-buddy")

;; Buffer-local variables defined in ollama-buddy.el and used here as free vars.
;; Declared to suppress byte-compile warnings; their true definitions live in
;; ollama-buddy.el where they are defvar-local on the chat buffer.
(defvar ollama-buddy--thinking-api-active)
(defvar ollama-buddy--thinking-arrow-marker)
(defvar ollama-buddy--thinking-block-start)
(defvar ollama-buddy--thinking-content-accumulator)

;; Curl-specific per-process state helpers
;; State is stored on each process object via process-put/process-get
;; to avoid races when multiple curl requests overlap.

(defsubst ollama-buddy-curl--get (proc key)
  "Get KEY from PROC's property list."
  (process-get proc key))

(defsubst ollama-buddy-curl--put (proc key val)
  "Set KEY to VAL on PROC's property list."
  (process-put proc key val))

;; Backend validation
(defun ollama-buddy-curl--validate-executable ()
  "Check if curl executable is available and working."
  (condition-case nil
      (zerop (call-process ollama-buddy-curl-executable nil nil nil "--version"))
    (error nil)))

;; Connection test
;;;###autoload
(defun ollama-buddy-curl--test-connection ()
  "Check if Ollama server is reachable using curl."
  (condition-case nil
      (let ((exit-code (call-process 
                        ollama-buddy-curl-executable nil nil nil
                        "--silent"
                        "--output" "/dev/null"
                        "--max-time" "5"
                        "--fail"
                        (format "http://%s:%d/api/tags" 
                                ollama-buddy-host ollama-buddy-port))))
        (zerop exit-code))
    (error nil)))

;; Direct curl request (no status check)
(defun ollama-buddy-curl--make-request-direct (endpoint method &optional payload)
  "Make a request using curl for ENDPOINT with METHOD and optional PAYLOAD.
This is a low-level function that doesn't check if Ollama is running first."
  (let* ((url (format "http://%s:%d%s"
                      ollama-buddy-host ollama-buddy-port endpoint))
         (temp-file (when payload (make-temp-file "ollama-buddy-payload")))
         (args (list
                "--silent"
                "--show-error"
                "--max-time" (number-to-string ollama-buddy-curl-timeout)
                "--request" method
                "--header" "Content-Type: application/json"
                "--header" "Connection: close")))
    
    ;; Add payload if provided
    (when payload
      (with-temp-file temp-file
        (insert payload))
      (setq args (append args (list "--data-binary" (concat "@" temp-file)))))
    
    ;; Add URL at the end
    (setq args (append args (list url)))
    
    (unwind-protect
        (with-temp-buffer
          (let ((exit-code (apply #'call-process ollama-buddy-curl-executable nil t nil args)))
            (if (zerop exit-code)
                (when (not (string-empty-p (buffer-string)))
                  (condition-case err
                      (json-read-from-string (buffer-string))
                    (error
                     (message "Warning: Failed to parse JSON response: %s" 
                              (error-message-string err))
                     nil)))
              ;; Don't error on non-zero exit code, just return nil
              nil)))
      ;; Clean up temp file
      (when (and temp-file (file-exists-p temp-file))
        (delete-file temp-file)))))

;; Synchronous curl request
(defun ollama-buddy-curl--make-request (endpoint method &optional payload)
  "Make a request using curl for ENDPOINT with METHOD and optional PAYLOAD."
  ;; Only check if Ollama is running for non-status endpoints
  (if (string= endpoint "/api/tags")
      ;; For status checks, use direct version to avoid circular dependency
      (ollama-buddy-curl--make-request-direct endpoint method payload)
    ;; For other endpoints, check if running first
    (when (ollama-buddy--ollama-running)
      (ollama-buddy-curl--make-request-direct endpoint method payload))))

;; Asynchronous curl request
(defun ollama-buddy-curl--make-request-async (endpoint method payload callback)
  "Make an asynchronous curl request to ENDPOINT using METHOD with PAYLOAD.
When complete, CALLBACK is called with the status response and result."
  (when (ollama-buddy--ollama-running)
    (let* ((url (format "http://%s:%d%s"
                        ollama-buddy-host ollama-buddy-port endpoint))
           (temp-file (when payload (make-temp-file "ollama-buddy-payload")))
           (process-name (format "ollama-curl-%s" (gensym)))
           (process-buffer (generate-new-buffer (format " *%s*" process-name)))
           (args (list
                  "--silent"
                  "--show-error"
                  "--max-time" (number-to-string ollama-buddy-curl-timeout)
                  "--request" method
                  "--header" "Content-Type: application/json"
                  "--header" "Connection: close")))
      
      ;; Add payload if provided
      (when payload
        (with-temp-file temp-file
          (insert payload))
        (setq args (append args (list "--data-binary" (concat "@" temp-file)))))
      
      ;; Add URL at the end
      (setq args (append args (list url)))
      
      (let ((process (apply #'start-process process-name process-buffer 
                            ollama-buddy-curl-executable args)))
        (set-process-sentinel
         process
         (lambda (proc event)
           (let ((status (if (string-match-p "finished" event)
                             nil
                           (list :error (cons 'error event))))
                 (result nil)
                 (proc-buffer (process-buffer proc)))
             
             ;; Safe buffer access - CRITICAL FIX
             (when (and proc-buffer (buffer-live-p proc-buffer))
               (condition-case err
                   (when (zerop (process-exit-status proc))
                     (with-current-buffer proc-buffer
                       (when (not (string-empty-p (buffer-string)))
                         (condition-case parse-err
                             (setq result (json-read-from-string (buffer-string)))
                           (error
                            (message "Warning: Failed to parse JSON response: %s" 
                                     (error-message-string parse-err)))))))
                 (error
                  (message "Error reading process buffer: %s" (error-message-string err)))))
             
             ;; Clean up temp file
             (when (and temp-file (file-exists-p temp-file))
               (condition-case err
                   (delete-file temp-file)
                 (error
                  (message "Warning: Failed to delete temp file %s: %s" 
                           temp-file (error-message-string err)))))
             
             ;; Clean up process buffer
             (when (and proc-buffer (buffer-live-p proc-buffer))
               (condition-case err
                   (kill-buffer proc-buffer)
                 (error
                  (message "Warning: Failed to kill process buffer: %s" 
                           (error-message-string err)))))
             
             ;; Call the callback
             (condition-case err
                 (funcall callback status result)
               (error
                (message "Error in curl async callback: %s" (error-message-string err)))))))))))

;; Streaming support
(defun ollama-buddy-curl--process-filter (proc output)
  "Process filter for curl streaming responses."
  (let ((proc-buffer (process-buffer proc)))
    (when (and proc-buffer (buffer-live-p proc-buffer))
      (condition-case err
          (with-current-buffer proc-buffer
            ;; Append new output
            (goto-char (point-max))
            (insert output)

            ;; Process headers if not already done
            (unless (ollama-buddy-curl--get proc :headers-processed)
              (goto-char (point-min))
              (when (re-search-forward "\r?\n\r?\n" nil t)
                (let ((headers (buffer-substring-no-properties (point-min) (point))))
                  ;; Check HTTP status from headers
                  (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" headers)
                    (let ((status (string-to-number (match-string 1 headers))))
                      (unless (and (>= status 200) (< status 300))
                        (ollama-buddy-curl--put proc :http-error-status status)))))
                ;; Headers found, remove them
                (delete-region (point-min) (point))
                (ollama-buddy-curl--put proc :headers-processed t)))

            ;; Non-2xx: try to parse the error body as a single JSON object.
            ;; The body may arrive across multiple chunks so ignore-errors
            ;; lets us retry on the next chunk.  On success, display the
            ;; error and kill the process (matching network-process behaviour).
            (when (ollama-buddy-curl--get proc :http-error-status)
              (let* ((body (string-trim
                            (buffer-substring-no-properties (point-min) (point-max))))
                     (error-json (when (> (length body) 0)
                                   (ignore-errors
                                     (json-read-from-string body)))))
                (when error-json
                  (let ((status-str (ollama-buddy--handle-http-error
                                     (ollama-buddy-curl--get proc :http-error-status)
                                     error-json)))
                    (ollama-buddy--update-status status-str))
                  ;; Clear buffer so we don't re-process
                  (erase-buffer)
                  ;; Kill the process to trigger sentinel cleanup
                  (when (process-live-p proc)
                    (delete-process proc)))))

            ;; Process all complete newline-delimited JSON lines.
            ;; Skipped when in HTTP-error mode.
            (unless (ollama-buddy-curl--get proc :http-error-status)
              (when (ollama-buddy-curl--get proc :headers-processed)
                (goto-char (point-min))
                (while (re-search-forward "^\\(.+\\)\n" nil t)
                  (let ((json-line (match-string 1)))
                    ;; Remove the processed line
                    (delete-region (match-beginning 0) (match-end 0))
                    ;; Process the JSON line
                    (ollama-buddy-curl--process-json-line json-line))))))
        (error
         (message "Error in curl filter: %s" (error-message-string err)))))))

(defun ollama-buddy-curl--process-json-line (json-line)
  "Process a single JSON line from curl output.
Delegates to the shared `ollama-buddy--stream-process-json' for full
feature parity (tool calls, thinking blocks, md-to-org, etc.)."
  (when (and json-line
             (not (string-empty-p (string-trim json-line)))
             (string-prefix-p "{" (string-trim json-line)))
    (condition-case err
        (let ((json-data (json-read-from-string json-line)))
          (ollama-buddy--stream-process-json json-data))
      (error
       (message "Error parsing JSON: %s" (error-message-string err))))))

(defun ollama-buddy-curl--sentinel (proc event)
  "Sentinel for curl streaming processes.
For normal completion, `ollama-buddy--stream-process-json' already handled
the done=true response.  This sentinel cleans up the process buffer,
temp file, and handles cancellation/failure."
  (condition-case err
      (let ((proc-buffer (process-buffer proc))
            (temp-file (ollama-buddy-curl--get proc :temp-file)))
        ;; Process any remaining data in the buffer before cleanup.
        ;; This catches JSON lines that lack a trailing newline.
        (when (and proc-buffer (buffer-live-p proc-buffer)
                   (not (ollama-buddy-curl--get proc :http-error-status)))
          (with-current-buffer proc-buffer
            (let ((remaining (string-trim
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))
              (when (and (> (length remaining) 0)
                         (string-prefix-p "{" remaining))
                (ollama-buddy-curl--process-json-line remaining)))))

        ;; Clean up process buffer
        (when (and proc-buffer (buffer-live-p proc-buffer))
          (kill-buffer proc-buffer))

        ;; Clean up temp file
        (when (and temp-file (file-exists-p temp-file))
          (ignore-errors (delete-file temp-file)))

        (cond
         ;; Normal completion — stream-process-json handled done=true already.
         ;; If an HTTP error was handled in the filter, clean up timers here
         ;; since the filter already displayed the error and prepared prompt.
         ((string-match-p "finished" event)
          (when (ollama-buddy-curl--get proc :http-error-status)
            (when ollama-buddy--token-update-timer
              (cancel-timer ollama-buddy--token-update-timer)
              (setq ollama-buddy--token-update-timer nil))
            (ollama-buddy--cancel-response-wait-timer)))

         ;; Cancellation (user killed the process)
         ((string-match-p "\\(?:killed\\|terminated\\)" event)
          (ollama-buddy--update-status "Request cancelled")
          (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
            (with-current-buffer ollama-buddy--chat-buffer
              (let ((inhibit-read-only t))
                (ollama-buddy--finalize-pending-thinking)
                (goto-char (point-max))
                (insert "\n\n*** CANCELLED")
                (ollama-buddy--prepare-prompt-area))))
          ;; Clean up timers
          (when ollama-buddy--token-update-timer
            (cancel-timer ollama-buddy--token-update-timer)
            (setq ollama-buddy--token-update-timer nil))
          (ollama-buddy--cancel-response-wait-timer))

         ;; Unexpected failure — also clean up timers
         (t
          (ollama-buddy--update-status "Request failed")
          (when ollama-buddy--token-update-timer
            (cancel-timer ollama-buddy--token-update-timer)
            (setq ollama-buddy--token-update-timer nil))
          (ollama-buddy--cancel-response-wait-timer)
          (message "Curl request failed: %s" event)))

        ;; Auto-save transcript
        (ollama-buddy--autosave-transcript))
    (error
     (message "Error in curl sentinel: %s" (error-message-string err)))))

;; Main curl send function

(defun ollama-buddy-curl--send (prompt &optional specified-model tool-continuation-p)
  "Send PROMPT using curl backend with streaming support.
Cloud models are proxied through the local Ollama server which handles
authentication via `ollama signin'."
  ;; Clear paused session state
  (unless tool-continuation-p
    (when (and (boundp 'ollama-buddy-tools--session-paused)
               ollama-buddy-tools--session-paused)
      (setq ollama-buddy-tools--session-paused nil)))
  ;; Validate request
  (ollama-buddy--validate-send-request prompt tool-continuation-p)

  ;; Process inline delimiters asynchronously, then send
  (ollama-buddy--process-inline-prompt-async
   prompt
   (lambda (processed-prompt)
     (ollama-buddy-curl--send-payload processed-prompt specified-model tool-continuation-p))))

(defun ollama-buddy-curl--send-payload (prompt specified-model tool-continuation-p)
  "Build and send the curl payload for PROMPT.
SPECIFIED-MODEL and TOOL-CONTINUATION-P are passed through
from `ollama-buddy-curl--send'."
  ;; Build payload and setup shared state
  (let* ((request (ollama-buddy--build-chat-payload prompt specified-model tool-continuation-p))
         (payload (plist-get request :payload))
         (temp-file (make-temp-file "ollama-buddy-payload"))
         (url (format "http://%s:%d/api/chat" ollama-buddy-host ollama-buddy-port))
         (process-name "ollama-chat-curl")
         (process-buffer (generate-new-buffer " *ollama-curl*")))

    (ollama-buddy--setup-chat-send request tool-continuation-p)

    ;; Write payload to temp file
    (with-temp-file temp-file
      (insert payload))

    ;; Start curl process
    (condition-case err
        (let ((args (list
                     "--silent"
                     "--max-time" (number-to-string ollama-buddy-curl-timeout)
                     "--request" "POST"
                     "--header" "Content-Type: application/json"
                     "--data-binary" (concat "@" temp-file)
                     url)))

          ;; Add streaming-specific args
          (when ollama-buddy-streaming-enabled
            (setq args (append '("--no-buffer" "--include") args)))

          (setq ollama-buddy--active-process
                (apply #'start-process process-name process-buffer
                       ollama-buddy-curl-executable args))

          ;; Store temp file path on process for sentinel cleanup
          (ollama-buddy-curl--put ollama-buddy--active-process :temp-file temp-file)

          ;; Set different filter and sentinel based on streaming mode
          (if ollama-buddy-streaming-enabled
              (progn
                (set-process-filter ollama-buddy--active-process
                                    #'ollama-buddy-curl--process-filter)
                (set-process-sentinel ollama-buddy--active-process
                                      #'ollama-buddy-curl--sentinel))
            (set-process-sentinel ollama-buddy--active-process
                                  #'ollama-buddy-curl--non-streaming-sentinel)))

      (error
       (when (file-exists-p temp-file)
         (delete-file temp-file))
       (when (and process-buffer (buffer-live-p process-buffer))
         (kill-buffer process-buffer))
       (ollama-buddy--update-status "Curl failed to start")
       (error "Failed to start curl: %s" (error-message-string err))))))

;; Add new sentinel for non-streaming mode:
(defun ollama-buddy-curl--non-streaming-sentinel (proc event)
  "Sentinel for non-streaming curl processes."
  (condition-case err
      (let ((proc-buffer (process-buffer proc))
            (temp-file (ollama-buddy-curl--get proc :temp-file))
            (result-content ""))

        ;; Get the complete response
        (when (and proc-buffer (buffer-live-p proc-buffer))
          (with-current-buffer proc-buffer
            (setq result-content (buffer-string))))

        ;; Clean up process buffer
        (when (and proc-buffer (buffer-live-p proc-buffer))
          (kill-buffer proc-buffer))

        ;; Clean up temp file
        (when (and temp-file (file-exists-p temp-file))
          (ignore-errors (delete-file temp-file)))

        ;; Handle different exit conditions
        (cond
         ((string-match-p "finished" event)
          ;; Process the complete response
          (ollama-buddy-curl--process-non-streaming-response result-content)
          (message "Curl request completed"))
         ((string-match-p "\\(?:killed\\|terminated\\)" event)
          (ollama-buddy--update-status "Request cancelled")
          (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
            (with-current-buffer ollama-buddy--chat-buffer
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                ;; Remove "Loading..." message
                (when (re-search-backward "Loading response\\.\\.\\." nil t)
                  (delete-region (match-beginning 0) (point-max)))
                (insert "\n\n*** CANCELLED")
                (ollama-buddy--prepare-prompt-area)))))
         (t
          (ollama-buddy--update-status "Request failed")
          (message "Curl request failed: %s" event))))
    (error
     (message "Error in curl non-streaming sentinel: %s" (error-message-string err)))))

;; Non-streaming response processing
(defun ollama-buddy-curl--process-non-streaming-response (response-content)
  "Process complete non-streaming RESPONSE-CONTENT.
Pre-sets token tracking for accurate stats, removes the Loading message,
then delegates to the shared JSON processor."
  (condition-case err
      (when (not (string-empty-p response-content))
        (let* ((json-data (condition-case parse-err
                              (json-read-from-string response-content)
                            (error
                             (message "Failed to parse non-streaming response: %s"
                                      (error-message-string parse-err))
                             nil)))
               (message-data (when json-data (alist-get 'message json-data)))
               (content (when message-data (alist-get 'content message-data))))
          (when (and json-data content)
            ;; Pre-set token tracking so the shared handler gets accurate stats
            ;; (it only increments by 1 per call, but we have the full response)
            (setq ollama-buddy--current-token-start-time (float-time)
                  ollama-buddy--current-token-count
                  (1- (ollama-buddy--estimate-token-count content)))

            ;; Remove "Loading response..." before the shared handler inserts content
            (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
              (with-current-buffer ollama-buddy--chat-buffer
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (when (re-search-backward "Loading response\\.\\.\\." nil t)
                    (delete-region (match-beginning 0) (point-max))))))

            ;; Delegate to shared processor (handles content + done=true)
            (ollama-buddy--stream-process-json json-data))))
    (error
     (message "Error processing non-streaming response: %s" (error-message-string err)))))

;; Public interface functions
(defun ollama-buddy-curl-test ()
  "Test curl backend functionality."
  (interactive)
  (if (ollama-buddy-curl--validate-executable)
      (if (ollama-buddy-curl--test-connection)
          (message "Curl backend is working correctly!")
        (message "Curl is available but cannot connect to Ollama"))
    (message "Curl executable not found or not working")))

(unless (advice-member-p #'ollama-buddy--dispatch-to-handler 'ollama-buddy-curl--send)
  (advice-add 'ollama-buddy-curl--send :around #'ollama-buddy--dispatch-to-handler))

(defun ollama-buddy-curl-unload-function ()
  "Remove advice when `ollama-buddy-curl' is unloaded."
  (advice-remove 'ollama-buddy-curl--send #'ollama-buddy--dispatch-to-handler)
  nil)

(provide 'ollama-buddy-curl)
;;; ollama-buddy-curl.el ends here
