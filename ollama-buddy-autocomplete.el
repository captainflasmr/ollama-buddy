;;; ollama-buddy-autocomplete.el --- Autocomplete functionality for ollama-buddy -*- lexical-binding: t; -*-

;; Author: Your Name
;; Keywords: autocomplete, ai, ollama
;; Package-Requires: ((emacs "28.1") (ollama-buddy "0.12.1"))

;;; Commentary:

;; This file provides autocomplete functionality for ollama-buddy, similar to
;; GitHub Copilot but using local Ollama models. It uses C-e (end-of-line) to
;; accept suggestions, following the simple-autosuggest pattern for reliability.

;;; Code:

(require 'ollama-buddy-core)

;; Customization variables
(defgroup ollama-buddy-autocomplete nil
  "Autocomplete functionality for ollama-buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-autocomplete-")

(defcustom ollama-buddy-autocomplete-enabled nil
  "Whether to enable ollama-buddy autocomplete functionality."
  :type 'boolean
  :group 'ollama-buddy-autocomplete)

(defcustom ollama-buddy-autocomplete-model nil
  "Specific model to use for autocomplete. If nil, uses the default model."
  :type '(choice (const :tag "Use default model" nil)
                 (string :tag "Model name"))
  :group 'ollama-buddy-autocomplete)

(defcustom ollama-buddy-autocomplete-trigger-delay 1.5
  "Delay in seconds before triggering autocomplete after stopping typing."
  :type 'float
  :group 'ollama-buddy-autocomplete)

(defcustom ollama-buddy-autocomplete-max-lines 3
  "Maximum number of lines to suggest for autocomplete."
  :type 'integer
  :group 'ollama-buddy-autocomplete)

(defcustom ollama-buddy-autocomplete-context-lines 10
  "Number of lines before the cursor to include as context."
  :type 'integer
  :group 'ollama-buddy-autocomplete)

(defcustom ollama-buddy-autocomplete-min-chars 3
  "Minimum number of characters on current line before triggering autocomplete."
  :type 'integer
  :group 'ollama-buddy-autocomplete)

(defcustom ollama-buddy-autocomplete-programming-modes
  '(prog-mode text-mode conf-mode)
  "Major modes where autocomplete should be active."
  :type '(repeat symbol)
  :group 'ollama-buddy-autocomplete)

(defcustom ollama-buddy-autocomplete-excluded-modes
  '(minibuffer-mode compilation-mode)
  "Major modes where autocomplete should be disabled."
  :type '(repeat symbol)
  :group 'ollama-buddy-autocomplete)

(defcustom ollama-buddy-autocomplete-system-prompt
  "You are an intelligent code completion assistant. Complete the given code context with the most likely continuation. Provide only the completion text without explanations, comments, or additional formatting. The completion should be syntactically correct and contextually appropriate."
  "System prompt for autocomplete requests."
  :type 'string
  :group 'ollama-buddy-autocomplete)

(defcustom ollama-buddy-autocomplete-accept-key 'end-of-line
  "Command to accept autocomplete suggestions. Can be 'end-of-line, 'tab, or 'custom."
  :type '(choice (const :tag "End of line (C-e)" end-of-line)
                 (const :tag "Tab" tab)
                 (const :tag "Custom binding" custom))
  :group 'ollama-buddy-autocomplete)

;; Internal variables
(defvar-local ollama-buddy-autocomplete--overlay nil
  "Overlay showing the current autocomplete suggestion.")

(defvar-local ollama-buddy-autocomplete--timer nil
  "Timer for delayed autocomplete triggering.")

(defvar-local ollama-buddy-autocomplete--request-in-progress nil
  "Whether an autocomplete request is currently in progress.")

(defvar-local ollama-buddy-autocomplete--current-suggestion nil
  "Current full suggestion text.")

(defvar ollama-buddy-autocomplete--process nil
  "Active autocomplete process.")

(defvar ollama-buddy-autocomplete--response-buffer ""
  "Buffer for accumulating autocomplete response.")

;; Core autocomplete functionality

(defun ollama-buddy-autocomplete--should-trigger-p ()
  "Check if autocomplete should be triggered in the current context."
  (and ollama-buddy-autocomplete-enabled
       (not ollama-buddy-autocomplete--request-in-progress)
       (ollama-buddy--check-status) ; Ollama is running
       (ollama-buddy-autocomplete--is-suitable-mode-p)
       (not (minibufferp))
       (ollama-buddy-autocomplete--has-sufficient-context-p)
       ;; Only trigger after typing commands
       (memq last-command '(org-self-insert-command self-insert-command))))

(defun ollama-buddy-autocomplete--is-suitable-mode-p ()
  "Check if the current major mode is suitable for autocomplete."
  (and (cl-some (lambda (mode) (derived-mode-p mode))
                ollama-buddy-autocomplete-programming-modes)
       (not (cl-some (lambda (mode) (derived-mode-p mode))
                     ollama-buddy-autocomplete-excluded-modes))))

(defun ollama-buddy-autocomplete--has-sufficient-context-p ()
  "Check if there's sufficient context to trigger autocomplete."
  (let* ((line-start (save-excursion (beginning-of-line) (point)))
         (current-line-text (buffer-substring-no-properties line-start (point))))
    (and (>= (length (string-trim current-line-text))
             ollama-buddy-autocomplete-min-chars))))
         ;; (not (string-match-p "^\\s-*$" current-line-text)))))

(defun ollama-buddy-autocomplete--get-context ()
  "Get the context around the cursor for autocomplete."
  (save-excursion
    (let* ((current-pos (point))
           (start-line (max 1 (- (line-number-at-pos) 
                                 ollama-buddy-autocomplete-context-lines)))
           (start-pos (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- start-line))
                        (point)))
           (context (buffer-substring-no-properties start-pos current-pos)))
      context)))

(defun ollama-buddy-autocomplete--create-prompt (context)
  "Create the autocomplete prompt from CONTEXT."
  (format "Complete the following %s code (provide only the completion, maximum %d lines):\n\n%s"
          (or (when (boundp 'major-mode)
                (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
              "")
          ollama-buddy-autocomplete-max-lines
          context))

(defun ollama-buddy-autocomplete--get-completion ()
  "Get autocomplete suggestion and display it as overlay."
  (when (and (not ollama-buddy-autocomplete--request-in-progress)
             (ollama-buddy-autocomplete--should-trigger-p))
    
    (setq ollama-buddy-autocomplete--request-in-progress t
          ollama-buddy-autocomplete--response-buffer "")
    
    (let* ((context (ollama-buddy-autocomplete--get-context))
           (prompt (ollama-buddy-autocomplete--create-prompt context))
           (model (or ollama-buddy-autocomplete-model
                      ollama-buddy--current-model
                      ollama-buddy-default-model))
           (messages `[((role . "system")
                        (content . ,ollama-buddy-autocomplete-system-prompt))
                       ((role . "user")
                        (content . ,prompt))])
           (payload (json-encode `((model . ,(ollama-buddy--get-real-model-name model))
                                   (messages . ,messages)
                                   (stream . t)
                                   (options . ((temperature . 0.3)
                                               (top_p . 0.8)
                                               (num_predict . ,(* ollama-buddy-autocomplete-max-lines 50)))))))
           (buffer (current-buffer)))
      
      ;; Clean up any existing process
      (when (and ollama-buddy-autocomplete--process
                 (process-live-p ollama-buddy-autocomplete--process))
        (delete-process ollama-buddy-autocomplete--process))
      
      ;; Create new process
      (condition-case err
          (setq ollama-buddy-autocomplete--process
                (make-network-process
                 :name "ollama-autocomplete"
                 :buffer nil
                 :host ollama-buddy-host
                 :service ollama-buddy-port
                 :coding 'utf-8
                 :filter (lambda (proc output)
                           (ollama-buddy-autocomplete--stream-filter proc output buffer))
                 :sentinel (lambda (proc event)
                             (ollama-buddy-autocomplete--stream-sentinel proc event buffer))))
        (error
         (setq ollama-buddy-autocomplete--request-in-progress nil)
         (message "Failed to connect to Ollama for autocomplete: %s" 
                  (error-message-string err))
         (return)))
      
      ;; Send the request
      (condition-case err
          (process-send-string
           ollama-buddy-autocomplete--process
           (concat "POST /api/chat HTTP/1.1\r\n"
                   (format "Host: %s:%d\r\n" ollama-buddy-host ollama-buddy-port)
                   "Content-Type: application/json\r\n"
                   (format "Content-Length: %d\r\n\r\n" (string-bytes payload))
                   payload))
        (error
         (setq ollama-buddy-autocomplete--request-in-progress nil)
         (when (and ollama-buddy-autocomplete--process
                    (process-live-p ollama-buddy-autocomplete--process))
           (delete-process ollama-buddy-autocomplete--process))
         (message "Failed to send autocomplete request: %s" 
                  (error-message-string err)))))))

(defun ollama-buddy-autocomplete--stream-filter (proc output buffer)
  "Process autocomplete stream OUTPUT from PROC for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Parse JSON responses from the stream
      (let ((lines (split-string output "\n" t)))
        (dolist (line lines)
          (when (string-match-p "^{" line)
            (condition-case nil
                (let* ((json-data (json-read-from-string line))
                       (message-data (alist-get 'message json-data))
                       (content (when message-data
                                  (alist-get 'content message-data)))
                       (done (alist-get 'done json-data)))
                  
                  (when content
                    (setq ollama-buddy-autocomplete--response-buffer
                          (concat ollama-buddy-autocomplete--response-buffer content)))
                  
                  (when (eq done t)
                    (ollama-buddy-autocomplete--process-completion
                     ollama-buddy-autocomplete--response-buffer)))
              (error nil))))))))

(defun ollama-buddy-autocomplete--stream-sentinel (proc event buffer)
  "Handle autocomplete stream completion EVENT for PROC and BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ollama-buddy-autocomplete--request-in-progress nil)
      (unless (string-match-p "finished" event)
        ;; Connection was interrupted or failed
        (ollama-buddy-autocomplete--clear-suggestion)))))

(defun ollama-buddy-autocomplete--process-completion (response)
  "Process the autocomplete RESPONSE and show suggestion."
  (setq ollama-buddy-autocomplete--request-in-progress nil)
  
  (when (and response (not (string-empty-p response)))
    (let* ((cleaned-response (ollama-buddy-autocomplete--clean-response response))
           (limited-response (ollama-buddy-autocomplete--limit-lines 
                              cleaned-response ollama-buddy-autocomplete-max-lines)))
      
      (when (and limited-response (not (string-empty-p limited-response)))
        (setq ollama-buddy-autocomplete--current-suggestion limited-response)
        (ollama-buddy-autocomplete--show-suggestion limited-response)))))

(defun ollama-buddy-autocomplete--clean-response (response)
  "Clean the autocomplete RESPONSE by removing unwanted formatting."
  (let ((cleaned response))
    ;; Remove leading/trailing whitespace
    (setq cleaned (string-trim cleaned))
    
    ;; Remove code block markers if present
    (setq cleaned (replace-regexp-in-string "^```[a-zA-Z]*\n?" "" cleaned))
    (setq cleaned (replace-regexp-in-string "\n?```$" "" cleaned))
    
    ;; Remove explanatory text patterns
    (setq cleaned (replace-regexp-in-string "^Here.*completion.*:\n?" "" cleaned))
    (setq cleaned (replace-regexp-in-string "^The.*continues.*:\n?" "" cleaned))
    
    cleaned))

(defun ollama-buddy-autocomplete--limit-lines (text max-lines)
  "Limit TEXT to MAX-LINES number of lines."
  (when text
    (let ((lines (split-string text "\n")))
      (mapconcat 'identity 
                 (seq-take lines (min (length lines) max-lines))
                 "\n"))))

;; Overlay management (inspired by simple-autosuggest)

(defun ollama-buddy-autocomplete--show-suggestion (suggestion)
  "Show the autocomplete SUGGESTION as an overlay."
  (when (and ollama-buddy-autocomplete--overlay suggestion)
    ;; Add cursor property like simple-autosuggest
    (put-text-property 0 1 'cursor 0 suggestion)
    (overlay-put ollama-buddy-autocomplete--overlay 'after-string
                 (propertize suggestion 'face '(:inherit shadow)))
    (move-overlay ollama-buddy-autocomplete--overlay (point) (point))))

(defun ollama-buddy-autocomplete--clear-suggestion ()
  "Clear the current autocomplete suggestion overlay."
  (when ollama-buddy-autocomplete--overlay
    (overlay-put ollama-buddy-autocomplete--overlay 'after-string nil)
    (setq ollama-buddy-autocomplete--current-suggestion nil)))

;; Acceptance functions (following simple-autosuggest pattern)

(defun ollama-buddy-autocomplete-end-of-line (arg)
  "Move to end of line, accepting autocomplete suggestion first if available.
Works with both standard `move-end-of-line` and `org-end-of-line`."
  (interactive "^p")
  (if-let ((overlay ollama-buddy-autocomplete--overlay)
           (suggestion (overlay-get overlay 'after-string)))
      (progn
        (insert (substring-no-properties suggestion))
        (ollama-buddy-autocomplete--clear-suggestion))
    ;; Detect whether we're in org-mode and use the appropriate function
    (if (and (eq major-mode 'org-mode)
             (fboundp 'org-end-of-line))
        (org-end-of-line arg)
      (move-end-of-line arg))))

(defun ollama-buddy-autocomplete-tab-accept ()
  "Accept suggestion with TAB key (alternative to end-of-line)."
  (interactive)
  (if-let ((overlay ollama-buddy-autocomplete--overlay)
           (suggestion (overlay-get overlay 'after-string)))
      (progn
        (insert (substring-no-properties suggestion))
        (ollama-buddy-autocomplete--clear-suggestion))
    ;; If no suggestion, just insert a tab
    (insert "\t")))

;; Timer management

(defun ollama-buddy-autocomplete--schedule-request ()
  "Schedule an autocomplete request after a delay."
  (ollama-buddy-autocomplete--cancel-timer)
  
  (when (ollama-buddy-autocomplete--should-trigger-p)
    (setq ollama-buddy-autocomplete--timer
          (run-with-timer ollama-buddy-autocomplete-trigger-delay nil
                          #'ollama-buddy-autocomplete--get-completion))))

(defun ollama-buddy-autocomplete--cancel-timer ()
  "Cancel any pending autocomplete timer."
  (when ollama-buddy-autocomplete--timer
    (cancel-timer ollama-buddy-autocomplete--timer)
    (setq ollama-buddy-autocomplete--timer nil)))

;; Update function (following simple-autosuggest pattern)

(defun ollama-buddy-autocomplete-update ()
  "Update the auto-suggestion overlay."
  (when ollama-buddy-autocomplete--overlay
    ;; Clear suggestion if we're not at the right position or context changed inappropriately
    (let ((should-clear nil))
      ;; Clear if cursor moved away from overlay position
      (when (not (eq (overlay-start ollama-buddy-autocomplete--overlay) (point)))
        (setq should-clear t))
      
      ;; Clear if we're in an inappropriate context (but not just because we stopped typing)
      (when (and (not (ollama-buddy-autocomplete--is-suitable-mode-p))
                 (minibufferp))
        (setq should-clear t))
      
      ;; Clear on certain commands that should dismiss suggestions
      (when (memq this-command '(newline electric-newline-and-maybe-indent
                                 delete-backward-char backward-delete-char-untabify
                                 kill-line kill-whole-line))
        (setq should-clear t))
      
      (when should-clear
        (ollama-buddy-autocomplete--clear-suggestion))))
  
  ;; Schedule new request if conditions are met and we don't have a suggestion
  (when (and (not ollama-buddy-autocomplete--current-suggestion)
             (not ollama-buddy-autocomplete--request-in-progress)
             ;; Don't immediately reschedule after dismissing
             (not (eq this-command 'ollama-buddy-autocomplete-keyboard-quit)))
    (ollama-buddy-autocomplete--schedule-request)))

;; C-g handling for dismissing suggestions

(defun ollama-buddy-autocomplete-keyboard-quit ()
  "Handle C-g - dismiss suggestion if present, otherwise run normal keyboard-quit."
  (interactive)
  (if (and ollama-buddy-autocomplete--overlay
           (overlay-get ollama-buddy-autocomplete--overlay 'after-string))
      (progn
        (ollama-buddy-autocomplete--clear-suggestion)
        (message "Autocomplete suggestion dismissed"))
    ;; No suggestion present, run normal keyboard-quit
    (keyboard-quit)))

;; Minor mode definition (following simple-autosuggest pattern)

;;;###autoload
(define-minor-mode ollama-buddy-autocomplete-mode
  "Minor mode for ollama-buddy autocomplete functionality.
Shows AI-powered code completions as you type. Accept with C-e (end-of-line), dismiss with C-g."
  :lighter " OB-AC"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Always bind C-g for dismissing suggestions
            (define-key map (kbd "C-g") #'ollama-buddy-autocomplete-keyboard-quit)
            
            (cond
             ;; End-of-line acceptance (default and recommended)
             ((eq ollama-buddy-autocomplete-accept-key 'end-of-line)
              (define-key map [remap move-end-of-line] #'ollama-buddy-autocomplete-end-of-line)
              (when (fboundp 'org-end-of-line)
                (define-key map [remap org-end-of-line] #'ollama-buddy-autocomplete-end-of-line))
              (define-key map (kbd "C-e") #'ollama-buddy-autocomplete-end-of-line))
             
             ;; TAB acceptance (alternative)
             ((eq ollama-buddy-autocomplete-accept-key 'tab)
              (define-key map (kbd "TAB") #'ollama-buddy-autocomplete-tab-accept)
              (define-key map (kbd "<tab>") #'ollama-buddy-autocomplete-tab-accept)))
            map)
  :group 'ollama-buddy-autocomplete
  (if ollama-buddy-autocomplete-mode
      (progn
        (setq-local ollama-buddy-autocomplete--overlay 
                    (make-overlay (point) (point) nil t t))
        (setq ollama-buddy-autocomplete-enabled t)
        (add-hook 'post-command-hook #'ollama-buddy-autocomplete-update nil t))
    (progn
      (setq ollama-buddy-autocomplete-enabled nil)
      (remove-hook 'post-command-hook #'ollama-buddy-autocomplete-update t)
      (ollama-buddy-autocomplete--cancel-timer)
      (when ollama-buddy-autocomplete--overlay
        (delete-overlay ollama-buddy-autocomplete--overlay)
        (setq ollama-buddy-autocomplete--overlay nil
              ollama-buddy-autocomplete--current-suggestion nil)))))

;; Global mode

;;;###autoload
(define-globalized-minor-mode global-ollama-buddy-autocomplete-mode
  ollama-buddy-autocomplete-mode
  (lambda ()
    (unless (minibufferp)
      (ollama-buddy-autocomplete-mode 1))))

;; User commands

;;;###autoload
(defun ollama-buddy-autocomplete-toggle ()
  "Toggle ollama-buddy autocomplete functionality."
  (interactive)
  (if ollama-buddy-autocomplete-mode
      (ollama-buddy-autocomplete-mode -1)
    (ollama-buddy-autocomplete-mode 1))
  (message "Ollama Buddy autocomplete %s (accept with %s)" 
           (if ollama-buddy-autocomplete-mode "enabled" "disabled")
           (cond ((eq ollama-buddy-autocomplete-accept-key 'end-of-line) "C-e")
                 ((eq ollama-buddy-autocomplete-accept-key 'tab) "TAB")
                 (t "custom key"))))

;;;###autoload
(defun ollama-buddy-autocomplete-trigger-manual ()
  "Manually trigger autocomplete suggestion."
  (interactive)
  (ollama-buddy-autocomplete--cancel-timer)
  (ollama-buddy-autocomplete--get-completion))

;;;###autoload
(defun ollama-buddy-autocomplete-accept-suggestion ()
  "Accept the current autocomplete suggestion (manual command)."
  (interactive)
  (if-let ((overlay ollama-buddy-autocomplete--overlay)
           (suggestion (overlay-get overlay 'after-string)))
      (progn
        (insert (substring-no-properties suggestion))
        (ollama-buddy-autocomplete--clear-suggestion)
        (message "Autocomplete suggestion accepted"))
    (message "No autocomplete suggestion available")))

;;;###autoload
(defun ollama-buddy-autocomplete-dismiss-suggestion ()
  "Dismiss the current autocomplete suggestion."
  (interactive)
  (if (and ollama-buddy-autocomplete--overlay
           (overlay-get ollama-buddy-autocomplete--overlay 'after-string))
      (progn
        (ollama-buddy-autocomplete--clear-suggestion)
        (message "Autocomplete suggestion dismissed"))
    (message "No autocomplete suggestion to dismiss")))

;; Debug functions

;;;###autoload
(defun ollama-buddy-autocomplete-debug-info ()
  "Show debug information about autocomplete state."
  (interactive)
  (let ((overlay-suggestion (when ollama-buddy-autocomplete--overlay
                              (overlay-get ollama-buddy-autocomplete--overlay 'after-string))))
    (message "Debug: overlay=%s overlay-text=%s stored-suggestion=%s enabled=%s in-progress=%s mode=%s"
             (if ollama-buddy-autocomplete--overlay "YES" "NO")
             (if overlay-suggestion "YES" "NO")
             (if ollama-buddy-autocomplete--current-suggestion "YES" "NO") 
             (if ollama-buddy-autocomplete-enabled "YES" "NO")
             (if ollama-buddy-autocomplete--request-in-progress "YES" "NO")
             (if ollama-buddy-autocomplete-mode "ON" "OFF"))))

;; Integration with main ollama-buddy package

(with-eval-after-load 'ollama-buddy
  ;; Add autocomplete commands to the main command definitions
  (when (boundp 'ollama-buddy-command-definitions)
    (setq ollama-buddy-command-definitions
          (append ollama-buddy-command-definitions
                  '((autocomplete-toggle
                     :key ?A
                     :description "Toggle autocomplete"
                     :action ollama-buddy-autocomplete-toggle)
                    
                    (autocomplete-trigger
                     :key ?t
                     :description "Trigger autocomplete manually"
                     :action ollama-buddy-autocomplete-trigger-manual)))))
  
  ;; Add keybindings to ollama-buddy-mode-map if it exists
  (when (boundp 'ollama-buddy-mode-map)
    (define-key ollama-buddy-mode-map (kbd "C-c a") #'ollama-buddy-autocomplete-toggle)
    (define-key ollama-buddy-mode-map (kbd "C-c C-t") #'ollama-buddy-autocomplete-trigger-manual)
    (define-key ollama-buddy-mode-map (kbd "C-c C-a") #'ollama-buddy-autocomplete-accept-suggestion)
    (define-key ollama-buddy-mode-map (kbd "C-c C-d") #'ollama-buddy-autocomplete-dismiss-suggestion)))

(provide 'ollama-buddy-autocomplete)
;;; ollama-buddy-autocomplete.el ends here
