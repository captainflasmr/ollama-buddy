;;; ollama-buddy-curl.el --- Curl backend for ollama-buddy -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/captainflasmr/ollama-buddy
;;
;; This file contains the curl backend implementation for ollama-buddy.
;; It provides an alternative communication method to the built-in network
;; process, useful for systems where network processes might not work properly.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'ollama-buddy-core)

;; Forward declarations for functions defined in other ollama-buddy files
(declare-function ollama-buddy--create-vision-message "ollama-buddy")
(declare-function ollama-buddy--detect-image-files "ollama-buddy")
(declare-function ollama-buddy--model-supports-vision "ollama-buddy")
(declare-function ollama-buddy--check-context-before-send "ollama-buddy")
(declare-function ollama-buddy--find-reasoning-marker "ollama-buddy")
(declare-function ollama-buddy--insert-thinking-header "ollama-buddy")
(declare-function ollama-buddy--finalize-thinking-block "ollama-buddy")
(declare-function ollama-buddy--update-token-rate-display "ollama-buddy")
(declare-function ollama-buddy--send-next-in-sequence "ollama-buddy")
(declare-function ollama-buddy--multishot-cancel-timer "ollama-buddy")
(declare-function ollama-buddy--autosave-transcript "ollama-buddy")
(declare-function ollama-buddy--extract-thinking-from-response "ollama-buddy")

;; Buffer-local variables defined in ollama-buddy.el and used here as free vars.
;; Declared to suppress byte-compile warnings; their true definitions live in
;; ollama-buddy.el where they are defvar-local on the chat buffer.
(defvar ollama-buddy--thinking-api-active)
(defvar ollama-buddy--thinking-arrow-marker)
(defvar ollama-buddy--thinking-block-start)
(defvar ollama-buddy--thinking-content-accumulator)
(defvar ollama-buddy--header-inserted-p)
(defvar ollama-buddy--turn-start-position)
(defvar ollama-buddy--current-original-model)
(defvar ollama-buddy--current-has-images)

(declare-function ollama-buddy--start-response-wait-timer "ollama-buddy")
(declare-function ollama-buddy--model-average-wait-time "ollama-buddy")
(declare-function ollama-buddy--trim-token-history "ollama-buddy")
(declare-function ollama-buddy--cancel-response-wait-timer "ollama-buddy")
(declare-function ollama-buddy--insert-response-header "ollama-buddy")

;; Curl-specific variables
(defvar ollama-buddy-curl--headers-processed nil
  "Flag to track if HTTP headers have been processed for current curl request.")

(defvar ollama-buddy-curl--http-error-status nil
  "Non-nil HTTP error status code from the current curl request.")

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
            (unless ollama-buddy-curl--headers-processed
              (goto-char (point-min))
              (when (re-search-forward "\r?\n\r?\n" nil t)
                (let ((headers (buffer-substring-no-properties (point-min) (point))))
                  ;; Check HTTP status from headers
                  (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" headers)
                    (let ((status (string-to-number (match-string 1 headers))))
                      (unless (and (>= status 200) (< status 300))
                        (setq ollama-buddy-curl--http-error-status status)))))
                ;; Headers found, remove them
                (delete-region (point-min) (point))
                (setq ollama-buddy-curl--headers-processed t)))
            
            ;; Process JSON lines when headers are processed
            (when ollama-buddy-curl--headers-processed
              (goto-char (point-min))
              (while (re-search-forward "^\\(.+\\)\n" nil t)
                (let ((json-line (match-string 1)))
                  ;; Remove the processed line
                  (delete-region (match-beginning 0) (match-end 0))
                  ;; Process the JSON line
                  (ollama-buddy-curl--process-json-line json-line)))))
        (error
         (message "Error in curl filter: %s" (error-message-string err)))))))

(defun ollama-buddy-curl--process-json-line (json-line)
  "Process a single JSON line from curl output."
  (when (and json-line 
             (not (string-empty-p (string-trim json-line)))
             (string-prefix-p "{" (string-trim json-line)))
    (condition-case err
        (let* ((json-data (json-read-from-string json-line))
               (error-msg (alist-get 'error json-data))
               (signin-url (alist-get 'signin_url json-data))
               (message-data (alist-get 'message json-data))
               (content (when message-data (alist-get 'content message-data)))
               (thinking-text (when message-data (alist-get 'thinking message-data)))
               (done (alist-get 'done json-data)))

          ;; Handle error responses (e.g. 401 unauthorized for cloud models)
          (when error-msg
            (let ((is-auth-error (string-match-p "unauthorized\\|authentication\\|sign.?in" error-msg)))
              (when is-auth-error
                (ollama-buddy--set-cloud-auth-status nil))
              (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
                (let ((inhibit-read-only t))
                  (save-excursion
                    (goto-char (point-max))
                    (if is-auth-error
                        (progn
                          (insert (format "\n\n*Authentication Error:* %s" error-msg))
                          (insert "\n\nSign in with =C-c A= or =M-x ollama-buddy-cloud-signin=")
                          (when signin-url
                            (insert (format "\n\nOr visit: %s" signin-url))))
                      (insert (format "\n\n*Error:* %s" error-msg)))
                    (ollama-buddy--prepare-prompt-area))))
              (ollama-buddy--update-status (if is-auth-error "Auth Required" "Error"))))

          ;; Handle thinking-API tokens (e.g. deepseek-r1)
          (when (and thinking-text (not (string-empty-p thinking-text)))
            (ollama-buddy-curl--handle-thinking thinking-text))

          (when content
            (ollama-buddy-curl--handle-content content))

          (when (eq done t)
            (ollama-buddy-curl--handle-completion json-data)))
      (error
       (message "Error parsing JSON: %s" (error-message-string err))))))

(defun ollama-buddy-curl--handle-thinking (thinking-text)
  "Handle a thinking token from the message.thinking API field."
  (unless ollama-buddy--current-token-start-time
    (ollama-buddy--cancel-response-wait-timer)
    (setq ollama-buddy--current-token-start-time (float-time)
          ollama-buddy--current-token-count 0
          ollama-buddy--current-response "")
    (when ollama-buddy--token-update-timer
      (cancel-timer ollama-buddy--token-update-timer))
    (setq ollama-buddy--token-update-timer
          (run-with-timer 0 ollama-buddy--token-update-interval
                          #'ollama-buddy--update-token-rate-display)))
  (setq ollama-buddy--current-token-count (1+ ollama-buddy--current-token-count))
  (with-current-buffer ollama-buddy--chat-buffer
    (let* ((inhibit-read-only t)
           (window (get-buffer-window ollama-buddy--chat-buffer t))
           (old-point (and window (window-point window)))
           (was-at-end (and window (>= old-point (point-max))))
           (old-window-start (and window (window-start window))))
      (save-excursion
        (goto-char (point-max))
        (cond
         (ollama-buddy-collapse-thinking
          (unless ollama-buddy--thinking-api-active
            (setq ollama-buddy--thinking-api-active t
                  ollama-buddy--thinking-arrow-marker (ollama-buddy--insert-thinking-header)
                  ollama-buddy--thinking-block-start  (copy-marker (point) t)))
          ;; Accumulate thinking tokens
          (setq ollama-buddy--thinking-content-accumulator
                (concat ollama-buddy--thinking-content-accumulator thinking-text))
          ;; Always insert into buffer; extend fold so text stays hidden
          (insert thinking-text)
          (ollama-buddy--extend-thinking-fold
           ollama-buddy--thinking-arrow-marker))
         (ollama-buddy-hide-reasoning
          (setq ollama-buddy--thinking-api-active t))
         (t
          (insert thinking-text))))
      (when window
        (cond
         ((and was-at-end ollama-buddy-auto-scroll)
          (set-window-point window (point-max)))
         (t
          (set-window-point window old-point)
          (set-window-start window old-window-start t)))))))

(defun ollama-buddy-curl--handle-content (content)
  "Handle content token from curl response."
  ;; Initialize token tracking on first content
  (unless ollama-buddy--current-token-start-time
    (ollama-buddy--cancel-response-wait-timer)
    (setq ollama-buddy--current-token-start-time (float-time)
          ollama-buddy--current-token-count 0
          ollama-buddy--current-response "")

    ;; Start token rate timer
    (when ollama-buddy--token-update-timer
      (cancel-timer ollama-buddy--token-update-timer))
    (setq ollama-buddy--token-update-timer
          (run-with-timer 0 ollama-buddy--token-update-interval
                          #'ollama-buddy--update-token-rate-display)))
  
  ;; Increment token count
  (setq ollama-buddy--current-token-count (1+ ollama-buddy--current-token-count))
  
  ;; Accumulate response
  (setq ollama-buddy--current-response 
        (concat ollama-buddy--current-response content))
  
  ;; Display content in chat buffer
  (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
    (with-current-buffer ollama-buddy--chat-buffer
      (let* ((inhibit-read-only t)
             (window (get-buffer-window ollama-buddy--chat-buffer t))
             (old-point (and window (window-point window)))
             (was-at-end (and window (>= old-point (point-max))))
             (old-window-start (and window (window-start window))))

        ;; Insert content at end of buffer
        (save-excursion
          (goto-char (point-max))

          ;; Close thinking-API block on first content token (deepseek-r1 style)
          (when (and ollama-buddy--thinking-api-active
                     (not (string-empty-p content)))
            (setq ollama-buddy--thinking-api-active nil)
            (when (and ollama-buddy-collapse-thinking
                       ollama-buddy--thinking-block-start)
              (ollama-buddy--finalize-thinking-block
               ollama-buddy--thinking-arrow-marker)
              (set-marker ollama-buddy--thinking-block-start nil)
              (setq ollama-buddy--thinking-block-start  nil
                    ollama-buddy--thinking-arrow-marker nil)))

          ;; Handle thinking block / reasoning hiding
          (let ((should-show-content t))
            (let ((reasoning-marker (ollama-buddy--find-reasoning-marker content)))
              (cond
               ;; --- Collapse mode ---
               (ollama-buddy-collapse-thinking
                (cond
                 ((and reasoning-marker (eq (car reasoning-marker) 'start))
                  (setq ollama-buddy--in-reasoning-section t
                        should-show-content nil
                        ollama-buddy--thinking-arrow-marker (ollama-buddy--insert-thinking-header)
                        ollama-buddy--thinking-block-start  (copy-marker (point) t)))
                 ((and reasoning-marker (eq (car reasoning-marker) 'end)
                       ollama-buddy--in-reasoning-section)
                  (setq ollama-buddy--in-reasoning-section nil
                        should-show-content nil)
                  (when ollama-buddy--thinking-block-start
                    (ollama-buddy--finalize-thinking-block
                     ollama-buddy--thinking-arrow-marker)
                    (set-marker ollama-buddy--thinking-block-start nil)
                    (setq ollama-buddy--thinking-block-start  nil
                          ollama-buddy--thinking-arrow-marker nil)))
                 ;; Inside block: accumulate + insert folded (peekable via TAB)
                 (ollama-buddy--in-reasoning-section
                  (when ollama-buddy--thinking-content-accumulator
                    (setq ollama-buddy--thinking-content-accumulator
                          (concat ollama-buddy--thinking-content-accumulator content))
                    ;; Always insert into buffer; extend fold so text stays hidden
                    (insert content)
                    (ollama-buddy--extend-thinking-fold
                     ollama-buddy--thinking-arrow-marker))
                  (setq should-show-content nil))))
               ;; --- Hide mode ---
               (ollama-buddy-hide-reasoning
                (cond
                 ((and reasoning-marker (eq (car reasoning-marker) 'start))
                  (setq ollama-buddy--in-reasoning-section t
                        should-show-content nil)
                  (insert (format "[%s...]"
                                  (capitalize
                                   (replace-regexp-in-string
                                    "[<>]" ""
                                    (cadr reasoning-marker))))))
                 ((and reasoning-marker (eq (car reasoning-marker) 'end))
                  (setq ollama-buddy--in-reasoning-section nil
                        should-show-content nil)
                  (when (re-search-backward "\\[.*\\.\\.\\.]" (line-beginning-position) t)
                    (delete-region (match-beginning 0) (match-end 0))))
                 (ollama-buddy--in-reasoning-section
                  (setq should-show-content nil))))))

            ;; Insert content if not suppressed
            (when (and should-show-content (not (string-empty-p (string-trim content))))
              (unless ollama-buddy--header-inserted-p
                (let ((pos (ollama-buddy--insert-response-header
                            ollama-buddy--current-model
                            ollama-buddy--current-original-model
                            ollama-buddy--current-has-images)))
                  (when pos
                    (set-marker ollama-buddy--response-start-position pos)
                    (set-marker pos nil))))
              (insert content))))

        ;; Window state management
        (when window
          (cond
           ;; Auto-scroll enabled and was at end, follow output
           ((and was-at-end ollama-buddy-auto-scroll)
            (set-window-point window (point-max)))
           ;; Otherwise restore original position
           (t
            (set-window-point window old-point)
            (set-window-start window old-window-start t))))))))

(defun ollama-buddy-curl--handle-completion (_json-data)
  "Handle completion of curl response."
  (condition-case err
      (progn
        ;; Cancel token timer
        (when ollama-buddy--token-update-timer
          (cancel-timer ollama-buddy--token-update-timer)
          (setq ollama-buddy--token-update-timer nil))
        
        ;; Calculate stats
        (let* ((elapsed (- (float-time) ollama-buddy--current-token-start-time))
               (rate (if (> elapsed 0) (/ ollama-buddy--current-token-count elapsed) 0))
               (token-info (list :model ollama-buddy--current-model
                                 :tokens ollama-buddy--current-token-count
                                 :elapsed elapsed
                                 :rate rate
                                 :wait-time ollama-buddy--response-wait-duration
                                 :timestamp (current-time))))
          (push token-info ollama-buddy--token-usage-history)
          (ollama-buddy--trim-token-history))

        ;; Add to history
        (ollama-buddy--add-to-history "user" ollama-buddy--current-prompt)
        (ollama-buddy--add-to-history "assistant" ollama-buddy--current-response)
        
        ;; Update chat buffer
        (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
          (with-current-buffer ollama-buddy--chat-buffer
            (let ((inhibit-read-only t)
                  (response-start (if (markerp ollama-buddy--response-start-position)
                                      (marker-position ollama-buddy--response-start-position)
                                    ollama-buddy--response-start-position))
                  (window (get-buffer-window ollama-buddy--chat-buffer t)))
              (goto-char (point-max))

              ;; Update turn header if tools were called
              (when (and (bound-and-true-p ollama-buddy--current-tool-calls)
                         (not (string-empty-p (or ollama-buddy--current-model ""))))
                (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
                  (let ((inhibit-read-only t))
                    (if ollama-buddy--header-inserted-p
                        ;; Rename existing RESPONSE -> TOOLS
                        (save-excursion
                          (let ((marker (if (markerp ollama-buddy--response-start-position)
                                            (marker-position ollama-buddy--response-start-position)
                                          ollama-buddy--response-start-position)))
                            (when marker
                              (goto-char marker)
                              (when (re-search-forward ": RESPONSE\\]" (line-end-position 2) t)
                                (replace-match ": TOOLS]")))))
                      ;; No header yet (only tools/thinking), insert TOOLS header at start
                      (ollama-buddy--insert-response-header
                       ollama-buddy--current-model
                       ollama-buddy--current-original-model
                       ollama-buddy--current-has-images)
                      ;; Rename that new header to TOOLS immediately
                      (save-excursion
                        (goto-char (point-max))
                        (when (re-search-backward ": RESPONSE\\]" nil t)
                          (replace-match ": TOOLS]")))))))

              ;; Pulse the response region to indicate completion
              (when (and ollama-buddy-pulse-response
                         ollama-buddy--response-start-position)
                (ignore-errors
                  (pulse-momentary-highlight-region
                   ollama-buddy--response-start-position (point))))

              ;; Convert the response from markdown to org format if enabled
              (when ollama-buddy-convert-markdown-to-org
                (let* ((clean-response
                        (let ((extracted (ollama-buddy--extract-thinking-from-response
                                         ollama-buddy--current-response)))
                          (if (car extracted) (cdr extracted) ollama-buddy--current-response)))
                       (converted-content (with-temp-buffer
                                            (insert clean-response)
                                            (ollama-buddy--md-to-org-convert-region (point-min) (point-max))
                                            (buffer-string))))
                  (set-register ollama-buddy-default-register converted-content))

                (when ollama-buddy--response-start-position
                  (let ((offset (if (save-excursion
                                      (goto-char ollama-buddy--response-start-position)
                                      (forward-line -1)
                                      (looking-at-p "\\*\\*\\* Response"))
                                    3 2)))
                    (ollama-buddy--md-to-org-convert-region
                     ollama-buddy--response-start-position
                     (point-max)
                     offset))
                  ;; Reset the marker after conversion
                  (when (markerp ollama-buddy--response-start-position)
                    (set-marker ollama-buddy--response-start-position nil))
                  (setq ollama-buddy--response-start-position nil)))

              (unless ollama-buddy-convert-markdown-to-org
                (set-register ollama-buddy-default-register ollama-buddy--current-response))

              ;; Show token stats if enabled
              (when ollama-buddy-display-token-stats
                (let ((last-info (car ollama-buddy--token-usage-history)))
                  (insert (format "\n\n*** Token Stats\n[%d tokens in %.1fs, %.1f tokens/sec]"
                                  (plist-get last-info :tokens)
                                  (plist-get last-info :elapsed)
                                  (plist-get last-info :rate)))))

              (ollama-buddy--prepare-prompt-area)

              ;; Move to prompt if response is wholly visible
              (ollama-buddy--maybe-goto-prompt window response-start))))
        
        ;; Clean up response wait timer
        (ollama-buddy--cancel-response-wait-timer)

        ;; Reset tracking variables
        (setq ollama-buddy--current-token-count 0
              ollama-buddy--current-token-start-time nil
              ollama-buddy--current-response ""
              ollama-buddy--in-reasoning-section nil
              ollama-buddy-curl--headers-processed nil
              ollama-buddy-curl--http-error-status nil)
        (when ollama-buddy--thinking-block-start
          (set-marker ollama-buddy--thinking-block-start nil)
          (setq ollama-buddy--thinking-block-start nil))
        (setq ollama-buddy--thinking-content-accumulator nil)
        (setq ollama-buddy--thinking-api-active nil
              ollama-buddy--thinking-arrow-marker nil)
        
        ;; Reset temporary model
        (when ollama-buddy--current-request-temporary-model
          (setq ollama-buddy--current-model ollama-buddy--current-request-temporary-model
                ollama-buddy--current-request-temporary-model nil))
        
        ;; Handle multishot or update status
        (if ollama-buddy--multishot-sequence
            (progn
              (ollama-buddy--multishot-cancel-timer)
              (if (< ollama-buddy--multishot-progress (length ollama-buddy--multishot-sequence))
                  (run-with-timer 0.5 nil #'ollama-buddy--send-next-in-sequence)
                (ollama-buddy--update-status "Multi Finished")))
          (let ((last-info (car ollama-buddy--token-usage-history)))
            (if last-info
                (ollama-buddy--update-status
                 (format "Curl Finished [%d %.1f t/s]"
                         (plist-get last-info :tokens)
                         (plist-get last-info :rate)))
              (ollama-buddy--update-status "Curl Finished"))))

        ;; Auto-save transcript after every response
        (ollama-buddy--autosave-transcript)

        ;; Check for pending project summary save
        (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
          (let ((buf (get-buffer ollama-buddy--chat-buffer)))
            (when (buffer-local-value 'ollama-buddy-project--pending-save-path buf)
              (run-with-timer
               0.5 nil
               (lambda ()
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (ollama-buddy-project--maybe-save-summary)))))))))
    (error
     (message "Error in curl completion: %s" (error-message-string err)))))

(defun ollama-buddy-curl--sentinel (proc event)
  "Sentinel for curl processes."
  (condition-case err
      (let ((proc-buffer (process-buffer proc)))
        ;; Clean up process buffer
        (when (and proc-buffer (buffer-live-p proc-buffer))
          (kill-buffer proc-buffer))
        
        ;; Handle different exit conditions
        (cond
         ((string-match-p "finished" event)
          (message "Curl request completed"))
         ((string-match-p "\\(?:killed\\|terminated\\)" event)
          (ollama-buddy--update-status "Request cancelled")
          (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
            (with-current-buffer ollama-buddy--chat-buffer
              (let ((inhibit-read-only t))
                ;; Preserve accumulated thinking content
                (when (and ollama-buddy--thinking-arrow-marker
                           (marker-buffer ollama-buddy--thinking-arrow-marker)
                           ollama-buddy--thinking-content-accumulator
                           (not (string-empty-p ollama-buddy--thinking-content-accumulator)))
                  (ollama-buddy--finalize-thinking-block
                   ollama-buddy--thinking-arrow-marker)
                  (when ollama-buddy--thinking-block-start
                    (set-marker ollama-buddy--thinking-block-start nil)
                    (setq ollama-buddy--thinking-block-start nil))
                  (setq ollama-buddy--thinking-arrow-marker nil
                        ollama-buddy--thinking-api-active nil
                        ollama-buddy--in-reasoning-section nil))
                (goto-char (point-max))
                (insert "\n\n*** CANCELLED")
                (ollama-buddy--prepare-prompt-area)))))
         (t
          (ollama-buddy--update-status "Request failed")
          (message "Curl request failed: %s" event))))
    (error
     (message "Error in curl sentinel: %s" (error-message-string err)))))

;; Main curl send function

(defun ollama-buddy-curl--send (prompt &optional specified-model)
  "Send PROMPT using curl backend with streaming support.
Cloud models are proxied through the local Ollama server which handles
authentication via `ollama signin'."
  (unless (ollama-buddy--check-status)
    (ollama-buddy--update-status "OFFLINE")
    (user-error "Ensure Ollama is running"))

  (unless (> (length prompt) 0)
    (user-error "Ensure prompt is defined"))

  (when ollama-buddy-show-context-percentage
    (unless (ollama-buddy--check-context-before-send)
      (user-error "Context too far over limit to send")))

  ;; Reset curl state
  (setq ollama-buddy-curl--headers-processed nil
        ollama-buddy-curl--http-error-status nil)

  (let* ((model-info (ollama-buddy--get-valid-model specified-model))
         (model (car model-info))
         (original-model (cdr model-info))
         (_ (ollama-buddy--ensure-cloud-model-available model))
         (supports-vision (and ollama-buddy-vision-enabled
                               (ollama-buddy--model-supports-vision model)))
         (image-files (when supports-vision
                        (ollama-buddy--detect-image-files prompt)))
         (has-images (and supports-vision image-files (not (null image-files))))
         (history (ollama-buddy--get-history-for-request))
         (messages-with-system
          (if ollama-buddy--current-system-prompt
              (append `(((role . "system")
                         (content . ,ollama-buddy--current-system-prompt)))
                      history)
            history))
         (attachment-context
          (when ollama-buddy--current-attachments
            (concat "\n\n## Attached Files Context:\n\n"
                    (mapconcat
                     (lambda (attachment)
                       (let ((file (plist-get attachment :file))
                             (content (plist-get attachment :content)))
                         (format "### File: %s\n\n%s\n\n"
                                 (file-name-nondirectory file)
                                 content)))
                     ollama-buddy--current-attachments
                     ""))))
         (current-message (if has-images
                              (ollama-buddy--create-vision-message prompt image-files)
                            `((role . "user")
                              (content . ,(if attachment-context
                                              (concat prompt attachment-context)
                                            prompt)))))
         (messages-all (append messages-with-system (list current-message)))
         (modified-options (ollama-buddy-params-get-for-request))
         (base-payload (append
                        `((model . ,(ollama-buddy--get-real-model-name model))
                          (messages . ,(vconcat [] messages-all))
                          (stream . ,(if ollama-buddy-streaming-enabled t :json-false)))
                        (when ollama-buddy-keepalive
                          `((keep_alive . ,ollama-buddy-keepalive)))))
         (final-payload (if modified-options
                            (append base-payload `((options . ,modified-options)))
                          base-payload))
         (payload (json-encode final-payload))
         (temp-file (make-temp-file "ollama-buddy-payload"))
         ;; Always use local Ollama server - it proxies cloud models automatically
         (url (format "http://%s:%d/api/chat" ollama-buddy-host ollama-buddy-port))
         (process-name "ollama-chat-curl")
         (process-buffer (generate-new-buffer " *ollama-curl*")))

    (unless ollama-buddy--multishot-sequence
      (set-register ollama-buddy-default-register ""))
    
    (setq ollama-buddy--current-model model
          ollama-buddy--current-prompt prompt)
    
    ;; Prepare chat buffer
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (pop-to-buffer (current-buffer))
      (goto-char (point-max))
      
      (unless (> (buffer-size) 0)
        (insert (ollama-buddy--create-intro-message)))

      (setq ollama-buddy--header-inserted-p nil)
      (setq ollama-buddy--current-original-model original-model)
      (setq ollama-buddy--current-has-images has-images)
      (setq ollama-buddy--response-start-position (copy-marker (point)))
      
      (visual-line-mode 1))

    ;; Show "Loading..." message when streaming is disabled
    (when (not ollama-buddy-streaming-enabled)
      (with-current-buffer ollama-buddy--chat-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "Loading response..."))))

    (ollama-buddy--update-status (if has-images
                                     "Working... [curl vision]"
                                   "Working... [curl]")
                                 original-model model)

    (ollama-buddy--start-response-wait-timer model)

    ;; Kill existing process
    (when (and ollama-buddy--active-process (process-live-p ollama-buddy--active-process))
      (delete-process ollama-buddy--active-process)
      (setq ollama-buddy--active-process nil))
    
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
          
          ;; Set different filter and sentinel based on streaming mode
          (if ollama-buddy-streaming-enabled
              (progn
                ;; Streaming mode - use line-by-line processing
                (set-process-filter ollama-buddy--active-process 
                                    #'ollama-buddy-curl--process-filter)
                (set-process-sentinel ollama-buddy--active-process 
                                      #'ollama-buddy-curl--sentinel))
            ;; Non-streaming mode - collect all output then process
            (set-process-sentinel ollama-buddy--active-process 
                                  #'ollama-buddy-curl--non-streaming-sentinel))
          
          ;; Clean up temp file after a delay
          (run-with-timer 1.0 nil (lambda () 
                                    (when (file-exists-p temp-file)
                                      (delete-file temp-file)))))
      
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
            (result-content ""))
        
        ;; Get the complete response
        (when (and proc-buffer (buffer-live-p proc-buffer))
          (with-current-buffer proc-buffer
            (setq result-content (buffer-string))))
        
        ;; Clean up process buffer
        (when (and proc-buffer (buffer-live-p proc-buffer))
          (kill-buffer proc-buffer))
        
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

;; Add function to process non-streaming response:
(defun ollama-buddy-curl--process-non-streaming-response (response-content)
  "Process complete non-streaming response RESPONSE-CONTENT."
  (condition-case err
      (when (not (string-empty-p response-content))
        ;; Parse the JSON response
        (let* ((json-data (condition-case parse-err
                              (json-read-from-string response-content)
                            (error
                             (message "Failed to parse non-streaming response: %s" 
                                      (error-message-string parse-err))
                             nil)))
               (message-data (when json-data (alist-get 'message json-data)))
               (content (when message-data (alist-get 'content message-data))))
          
          (when content
            ;; Set up token tracking
            (setq ollama-buddy--current-token-start-time (float-time)
                  ollama-buddy--current-token-count (ollama-buddy--estimate-token-count content)
                  ollama-buddy--current-response content)
            
            ;; Display the complete response at once
            (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
              (with-current-buffer ollama-buddy--chat-buffer
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  
                  ;; Remove "Loading..." message
                  (when (re-search-backward "Loading response\\.\\.\\." nil t)
                    (delete-region (match-beginning 0) (point-max)))
                  
                  ;; Insert the complete response
                  (insert content)
                  
                  ;; Update register
                  (set-register ollama-buddy-default-register content))))
            
            ;; Handle completion
            (ollama-buddy-curl--handle-completion json-data))))
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
