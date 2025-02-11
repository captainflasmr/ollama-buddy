(require 'json)
(require 'subr-x)
(require 'url)
(require 'cl-lib)

(defgroup ollama-buddy nil
  "Customization group for Ollama Buddy."
  :group 'applications
  :prefix "ollama-buddy-")

(defcustom ollama-buddy-host "localhost"
  "Host where Ollama server is running."
  :type 'string
  :group 'ollama-buddy)

(defcustom ollama-buddy-port 11434
  "Port where Ollama server is running."
  :type 'integer
  :group 'ollama-buddy)

(defcustom ollama-buddy-menu-columns 4
  "Number of columns to display in the Ollama Buddy menu."
  :type 'integer
  :group 'ollama-buddy)

(defcustom ollama-buddy-current-model nil
  "Default Ollama model to use."
  :type 'string
  :group 'ollama-buddy)

(defconst ollama-buddy--separators
  '((header . "\n=========================  n_____n  =========================
========================= | o Y o | =========================")
    (response . "------------------------- | @ Y @ | -------------------------"))
  "Separators for chat display.")

(defcustom ollama-buddy-connection-check-interval 5
  "Interval in seconds to check Ollama connection status."
  :type 'integer
  :group 'ollama-buddy)

(defvar ollama-buddy--connection-timer nil
  "Timer for checking Ollama connection status.")

(defvar ollama-buddy--chat-buffer "*Ollama Buddy Chat*"
  "Chat interaction buffer.")

(defvar ollama-buddy--active-process nil
  "Active Ollama process.")

(defvar ollama-buddy--status "Idle"
  "Current status of the Ollama request.")

(defun ollama-buddy--monitor-connection ()
  "Monitor Ollama connection status and update UI accordingly."
  (let ((connected (ollama-buddy--ollama-running)))
    (unless connected
      ;; Kill active process if it exists but connection is dead
      (when (and ollama-buddy--active-process
                 (process-live-p ollama-buddy--active-process))
        (delete-process ollama-buddy--active-process)
        (setq ollama-buddy--active-process nil)
        (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "\n\n[Connection Lost - Request Interrupted]\n\n")))))
    ;; Update status
    (ollama-buddy--update-status-overlay ollama-buddy--status)))

(defun ollama-buddy--start-connection-monitor ()
  "Start the Ollama connection monitoring."
  (ollama-buddy--stop-connection-monitor)
  (setq ollama-buddy--connection-timer
        (run-with-timer 0 ollama-buddy-connection-check-interval
                       #'ollama-buddy--monitor-connection)))

(defun ollama-buddy--stop-connection-monitor ()
  "Stop the Ollama connection monitoring."
  (when ollama-buddy--connection-timer
    (cancel-timer ollama-buddy--connection-timer)
    (setq ollama-buddy--connection-timer nil)))

(defun ollama-buddy--update-status-overlay (status)
  "Update the Ollama status and refresh the display."
  (setq ollama-buddy--status status)
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (setq header-line-format
          (propertize (format " [%s %s: %s] "
                              (if (ollama-buddy--ollama-running) "RUNNING" "OFFLINE")
                              (or ollama-buddy-current-model "No Model")
                              ollama-buddy--status)
                      'face '(:inherit bold))))
  (redisplay))

(defun ollama-buddy--stream-filter (proc output)
  "Process Ollama stream output."
  (condition-case nil
      (when-let* ((json-str (replace-regexp-in-string "^[^\{]*" "" output))
                  (len (length json-str))
                  (json-data (and (> len 0) (json-read-from-string json-str)))
                  (msg (alist-get 'message json-data))
                  (text (alist-get 'content msg)))
        (with-current-buffer ollama-buddy--chat-buffer
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert text)
            (ollama-buddy--update-status-overlay "Processing...") ;; Update status
            (when (eq (alist-get 'done json-data) t)
              (insert (format "\n\n[%s: FINISHED]\n\n" ollama-buddy-current-model))
              (ollama-buddy--update-status-overlay "Finished"))
            (redisplay))))
    (error nil)))

(defun ollama-buddy--stream-sentinel (proc event)
  "Handle stream completion."
  (when (string-match-p "finished\\|deleted\\|connection broken" event)
    (with-current-buffer ollama-buddy--chat-buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n\n[Stream %s]\n\n" 
                        (if (string-match-p "finished" event) "Completed" "Interrupted")))
        (ollama-buddy--update-status-overlay
         (format "Stream %s" 
                 (if (string-match-p "finished" event) "Completed " "Interrupted")))))))

(defun ollama-buddy--send (&optional system-prompt)
  "Send region to Ollama."
  (interactive)
  (unless (and (ollama-buddy--ollama-running) (use-region-p))
    (user-error "Ensure Ollama is running and text is selected"))
  
  (unless ollama-buddy-current-model
    (setq ollama-buddy-current-model
          (completing-read "Select model: " (ollama-buddy--get-models) nil t)))

  (let* ((prompt (buffer-substring-no-properties (region-beginning) (region-end)))
         (content (if system-prompt (concat system-prompt "\n\n" prompt) prompt))
         (payload (json-encode `((model . ,ollama-buddy-current-model)
                                 (messages . [((role . "user") (content . ,content))])
                                 (stream . t))))
         (buf (get-buffer-create ollama-buddy--chat-buffer)))
    
    (pop-to-buffer buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (unless (> (buffer-size) 0)
        (insert (ollama-buddy--create-intro-message)))
      (insert "\n\n" (alist-get 'header ollama-buddy--separators) "\n"
              (format "\n[User: PROMPT] %s\n" content)
              "\n" (alist-get 'response ollama-buddy--separators) "\n"
              (format "\n[%s: RESPONSE] ... \n\n" ollama-buddy-current-model))
      (visual-line-mode 1))

    (ollama-buddy--update-status-overlay "Sending request...")

    (setq ollama-buddy--active-process
          (make-network-process
           :name "ollama-chat-stream"
           :buffer nil
           :host ollama-buddy-host
           :service ollama-buddy-port
           :coding 'utf-8
           :filter #'ollama-buddy--stream-filter
           :sentinel #'ollama-buddy--stream-sentinel))
    
    (process-send-string 
     ollama-buddy--active-process
     (concat "POST /api/chat HTTP/1.1\r\n"
             (format "Host: %s:%d\r\n" ollama-buddy-host ollama-buddy-port)
             "Content-Type: application/json\r\n"
             (format "Content-Length: %d\r\n\r\n" (string-bytes payload))
             payload))))

(defun ollama-buddy--get-models ()
  "Retrieve available Ollama models using network process."
  (interactive)
  (when (ollama-buddy--ollama-running)
    (let ((url-request-method "GET")
          (url-request-extra-headers '(("Connection" . "close")))
          (endpoint (format "http://%s:%d/api/tags" 
                            ollama-buddy-host 
                            ollama-buddy-port)))
      (with-temp-buffer
        (url-insert-file-contents endpoint)
        (let ((json-data (json-read-from-string 
                          (buffer-substring-no-properties 
                           (point-min) 
                           (point-max)))))
          (mapcar (lambda (m) 
                    (alist-get 'name m)) 
                  (alist-get 'models json-data)))))))

(defun ollama-buddy--ollama-running ()
  "Check if Ollama server is running."
  (condition-case nil
      (let ((proc (make-network-process
                   :name "ollama-test"
                   :buffer nil
                   :host ollama-buddy-host
                   :service ollama-buddy-port
                   :nowait nil)))
        (delete-process proc)
        t)
    (error nil)))

(defun ollama-buddy--create-intro-message ()
  "Create welcome message."
  (let ((status (when (ollama-buddy--ollama-running)
                    (format "    Models available:\n\n%s\n\n"
                            (mapconcat (lambda (m) (format "      %s" m))
                                       (ollama-buddy--get-models) "\n")))))
    (concat
     "\n" (alist-get 'header ollama-buddy--separators) "\n"
     "         ╭──────────────────────────────────────╮\n"
     "         │              Welcome to               │\n"
     "         │             OLLAMA BUDDY              │\n"
     "         │       Your Friendly AI Assistant      │\n"
     "         ╰──────────────────────────────────────╯\n\n"
     "    Hi there!\n\n" status
     "    Quick Tips:\n"
     "    - Select text and use M-x ollama-buddy-menu\n"
     "    - Switch models [m], cancel [x]\n"
     "    - Send from any buffer\n\n"
     (alist-get 'response ollama-buddy--separators) "\n\n")))

(defcustom ollama-buddy-menu-items
  '((?o . ("Open chat buffer" 
           (lambda ()
             (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
             (when (= (buffer-size) 0)
               (insert (ollama-buddy--create-intro-message)))
             (goto-char (point-max)))))
    (?m . ("Swap model" 
           (lambda ()
             (if (not (ollama-buddy--ollama-running))
                 (error "!!WARNING!! ollama server not running.")
               (progn
                 (setq ollama-buddy-current-model 
                       (completing-read "Model: " (ollama-buddy--get-models) nil t))
                 (ollama-buddy--update-status-overlay "Idle"))))))
    (?h . ("Help assistant" 
           (lambda ()
             (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
             (goto-char (point-max))
             (insert (ollama-buddy--create-intro-message)))))
    (?l . ("Send region" (lambda () (ollama-buddy--send))))
    (?r . ("Refactor code" (lambda () (ollama-buddy--send "refactor the following code:"))))
    (?g . ("Git commit message" (lambda () (ollama-buddy--send "write a concise git commit message for the following:"))))
    (?c . ("Describe code" (lambda () (ollama-buddy--send "describe the following code:"))))
    (?d . ("Dictionary Lookup" 
           (lambda () 
             (ollama-buddy--send
              (concat "For the word {"
                      (buffer-substring-no-properties (region-beginning) (region-end))
                      "} provide a typical dictionary definition:")))))
    (?n . ("Word synonym" (lambda () (ollama-buddy--send "list synonyms for word:"))))
    (?p . ("Proofread text" (lambda () (ollama-buddy--send "proofread the following:"))))
    (?z . ("Make concise" (lambda () (ollama-buddy--send "reduce wordiness while preserving meaning:"))))
    (?e . ("Custom prompt" 
           (lambda ()
             (when-let ((prefix (read-string "Enter prompt prefix: " nil nil nil t)))
               (unless (string-empty-p prefix)
                 (ollama-buddy--send prefix))))))
    (?s . ("Save chat" 
           (lambda ()
             (with-current-buffer ollama-buddy--chat-buffer
               (write-region (point-min) (point-max) 
                             (read-file-name "Save conversation to: ")
                             'append-to-file
                             nil)))))
    (?x . ("Kill request" 
           (lambda ()
             (delete-process ollama-buddy--active-process))))
    (?q . ("Quit" (lambda () (message "Quit Ollama Shell menu.")))))
  "Menu items definition for Ollama Buddy."
  :type '(alist :key-type character
                :value-type (group (string :tag "Description")
                                   (function :tag "Action")))
  :group 'ollama-buddy)

(defun ollama-buddy-menu ()
  "Display Ollama Buddy menu."
  (interactive)
  (when-let* ((items ollama-buddy-menu-items)
              (formatted-items 
               (mapcar (lambda (item) 
                         (format "[%c] %s" (car item) (cadr item)))
                       items))
              (total (length formatted-items))
              (rows (ceiling (/ total (float ollama-buddy-menu-columns))))
              (padded-items (append formatted-items
                                    (make-list (- (* rows ollama-buddy-menu-columns) total)
                                               "")))
              (format-string
               (mapconcat
                (lambda (width) (format "%%-%ds" (+ width 2)))
                (butlast
                 (cl-loop for col below ollama-buddy-menu-columns collect
                          (cl-loop for row below rows
                                   for idx = (+ (* col rows) row)
                                   when (< idx total)
                                   maximize (length (nth idx padded-items)))))
                ""))
              (prompt
               (format "%s %s%s\nAvailable: %s\n%s"
                       (if (ollama-buddy--ollama-running) "RUNNING" "NOT RUNNING")
                       (or ollama-buddy-current-model "NONE")
                       (if (use-region-p) "" " (NO SELECTION)")
                       (mapconcat #'identity (ollama-buddy--get-models) " ")
                       (mapconcat
                        (lambda (row)
                          (if format-string (apply 'format (concat format-string "%s") row) (car row)))
                        (cl-loop for row below rows collect
                                 (cl-loop for col below ollama-buddy-menu-columns
                                          for idx = (+ (* col rows) row)
                                          when (< idx (length padded-items))
                                          collect (nth idx padded-items)))
                        "\n")))
              (key (read-key (propertize prompt 'face 'minibuffer-prompt)))
              (cmd (assoc key items)))
    (funcall (caddr cmd))))

(ollama-buddy--start-connection-monitor)

(provide 'ollama-buddy)
;;; ollama-buddy.el ends here
