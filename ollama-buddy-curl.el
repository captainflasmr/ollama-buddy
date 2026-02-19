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
(declare-function ollama-buddy--update-token-rate-display "ollama-buddy")
(declare-function ollama-buddy--send-next-in-sequence "ollama-buddy")
(declare-function ollama-buddy--autosave-transcript "ollama-buddy")

;; Curl-specific variables
(defvar ollama-buddy-curl--headers-processed nil
  "Flag to track if HTTP headers have been processed for current curl request.")

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
               (message-data (alist-get 'message json-data))
               (content (when message-data (alist-get 'content message-data)))
               (done (alist-get 'done json-data)))
          
          (when content
            (ollama-buddy-curl--handle-content content))
          
          (when (eq done t)
            (ollama-buddy-curl--handle-completion json-data)))
      (error
       (message "Error parsing JSON: %s" (error-message-string err))))))

(defun ollama-buddy-curl--handle-content (content)
  "Handle content token from curl response."
  ;; Initialize token tracking on first content
  (unless ollama-buddy--current-token-start-time
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

          ;; Handle reasoning hiding
          (let ((should-show-content t))
            (when ollama-buddy-hide-reasoning
              (let ((reasoning-marker (ollama-buddy--find-reasoning-marker content)))
                (cond
                 ;; Start of reasoning section
                 ((and reasoning-marker (eq (car reasoning-marker) 'start))
                  (setq ollama-buddy--in-reasoning-section t
                        should-show-content nil)
                  (insert (format "[%s...]"
                                  (capitalize
                                   (replace-regexp-in-string
                                    "[<>]" ""
                                    (cadr reasoning-marker))))))
                 ;; End of reasoning section
                 ((and reasoning-marker (eq (car reasoning-marker) 'end))
                  (setq ollama-buddy--in-reasoning-section nil
                        should-show-content nil)
                  ;; Remove the reasoning indicator if present
                  (when (re-search-backward "\\[.*\\.\\.\\.]" (line-beginning-position) t)
                    (delete-region (match-beginning 0) (match-end 0))))
                 ;; Inside reasoning section
                 (ollama-buddy--in-reasoning-section
                  (setq should-show-content nil)))))

            ;; Insert content if not hidden
            (when should-show-content
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
                                 :timestamp (current-time))))
          (push token-info ollama-buddy--token-usage-history))
        
        ;; Add to history
        (ollama-buddy--add-to-history "user" ollama-buddy--current-prompt)
        (ollama-buddy--add-to-history "assistant" ollama-buddy--current-response)
        
        ;; Update chat buffer
        (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
          (with-current-buffer ollama-buddy--chat-buffer
            (let ((inhibit-read-only t)
                  (response-start ollama-buddy--response-start-position)
                  (window (get-buffer-window ollama-buddy--chat-buffer t)))
              (goto-char (point-max))

              ;; Pulse the response region to indicate completion
              (when (and ollama-buddy-pulse-response
                         ollama-buddy--response-start-position)
                (ignore-errors
                  (pulse-momentary-highlight-region
                   ollama-buddy--response-start-position (point))))

              ;; Convert the response from markdown to org format if enabled
              (when ollama-buddy-convert-markdown-to-org
                (let* ((converted-content (with-temp-buffer
                                            (insert ollama-buddy--current-response)
                                            (ollama-buddy--md-to-org-convert-region (point-min) (point-max))
                                            (buffer-string))))
                  (set-register ollama-buddy-default-register converted-content))

                (when ollama-buddy--response-start-position
                  (ollama-buddy--md-to-org-convert-region
                   ollama-buddy--response-start-position
                   (point-max))
                  ;; Reset the marker after conversion
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

              (insert "\n\n*** FINISHED")
              (ollama-buddy--prepare-prompt-area)

              ;; Move to prompt if response is wholly visible
              (ollama-buddy--maybe-goto-prompt window response-start))))
        
        ;; Reset tracking variables
        (setq ollama-buddy--current-token-count 0
              ollama-buddy--current-token-start-time nil
              ollama-buddy--current-response ""
              ollama-buddy--in-reasoning-section nil
              ollama-buddy-curl--headers-processed nil)
        
        ;; Reset temporary model
        (when ollama-buddy--current-request-temporary-model
          (setq ollama-buddy--current-model ollama-buddy--current-request-temporary-model
                ollama-buddy--current-request-temporary-model nil))
        
        ;; Handle multishot or update status
        (if ollama-buddy--multishot-sequence
            (if (< ollama-buddy--multishot-progress (length ollama-buddy--multishot-sequence))
                (run-with-timer 0.5 nil #'ollama-buddy--send-next-in-sequence)
              (ollama-buddy--update-status "Multi Finished"))
          (let ((last-info (car ollama-buddy--token-usage-history)))
            (if last-info
                (ollama-buddy--update-status
                 (format "Curl Finished [%d %.1f t/s]"
                         (plist-get last-info :tokens)
                         (plist-get last-info :rate)))
              (ollama-buddy--update-status "Curl Finished"))))

        ;; Auto-save transcript after every response
        (ollama-buddy--autosave-transcript))
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
  (setq ollama-buddy-curl--headers-processed nil)

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

      (when ollama-buddy--current-attachments
        (insert (format "\n\n[Including %d attached file(s) in context]"
                        (length ollama-buddy--current-attachments))))
      
      (if has-images
          (insert (format "\n\n** [%s: RESPONSE with %d image(s)]"
                          model (length image-files)))
        (insert (format "\n\n** [%s: RESPONSE]" model)))

      (setq ollama-buddy--response-start-position (point))
      (insert "\n\n")

      (when (and original-model model (not (string= original-model model)))
        (insert (format "*[Using %s instead of %s]*\n\n" model original-model)))

      (when has-images
        (insert "Detected images:\n")
        (dolist (img image-files)
          (insert (format "- %s\n" img)))
        (insert "\n"))

      (visual-line-mode 1))

    ;; Show "Loading..." message when streaming is disabled
    (when (not ollama-buddy-streaming-enabled)
      (with-current-buffer ollama-buddy--chat-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "Loading response..."))))
    
    (ollama-buddy--update-status (if has-images
                                     "Curl Vision Processing..."
                                   "Curl Processing...")
                                 original-model model)

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

(advice-add 'ollama-buddy-curl--send :around #'ollama-buddy--dispatch-to-handler)

(provide 'ollama-buddy-curl)
;;; ollama-buddy-curl.el ends here
