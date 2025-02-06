(require 'json)
(require 'subr-x)

(defgroup ollama-buddy nil
  "Customization group for Ollama Buddy."
  :group 'applications
  :prefix "ollama-buddy-")

(defcustom ollama-buddy-menu-columns 3
  "Number of columns to display in the Ollama Buddy menu."
  :type 'integer
  :group 'ollama-buddy)

(defcustom ollama-buddy-separator-1
  "=========================  n_____n  =========================
========================= | o Y o | ========================="
  "Separator used for Ollama LLM output, variant 1."
  :type 'string
  :group 'ollama-buddy)

(defcustom ollama-buddy-separator-2
  "------------------------- | @ Y @ | -------------------------"
  "Separator used for Ollama LLM output, variant 2."
  :type 'string
  :group 'ollama-buddy)

(defvar ollama-buddy--chat-buffer "*Ollama Chat*"
  "Buffer for Ollama chat interactions.")

(defvar ollama-buddy--active-process nil
  "Current active Ollama process.")

(defcustom ollama-buddy-current-model nil
  "Default Ollama model to use."
  :type 'string
  :group 'ollama-buddy)

(defun ollama-buddy--format-menu (items columns)
  "Format menu ITEMS into COLUMNS columns."
  (let* ((items-list (mapcar (lambda (item) (format "[%c] %s" (car item) (cadr item))) items))
         (total (length items-list))
         (rows (ceiling (/ total (float columns))))
         (padded-list (append items-list (make-list (- (* rows columns) total) "")))
         (format-string
          (mapconcat
           (lambda (width) (format "%%-%ds" (+ width 2)))
           (butlast
            (cl-loop for col below columns collect
                     (cl-loop for row below rows
                              for idx = (+ (* col rows) row)
                              when (< idx total)
                              maximize (length (nth idx padded-list)))))
           "")))
    (format "%s %s%s\nAvailable: %s\n%s"
            (if (ollama-buddy--ollama-running) "RUNNING" "!!WARNING!! ollama NOT RUNNING.")
            (or ollama-buddy-current-model "NONE")
            (if (use-region-p) "" " (NO REGION SELECTED)")
            (mapconcat #'identity (ollama-buddy--get-models) " ")
            (mapconcat
             (lambda (row)
               (if format-string (apply 'format (concat format-string "%s") row) (car row)))
             (cl-loop for row below rows collect
                      (cl-loop for col below columns
                               for idx = (+ (* col rows) row)
                               when (< idx (length padded-list))
                               collect (nth idx padded-list)))
             "\n"))))

(defun ollama-buddy--create-intro-message ()
  "Create introduction message for Ollama Buddy."
  (concat
   "\n" ollama-buddy-separator-1 "\n"
   "         ╭──────────────────────────────────────╮\n"
   "         │              Welcome to               │\n"
   "         │             OLLAMA BUDDY              │\n"
   "         │       Your Friendly AI Assistant      │\n"
   "         ╰──────────────────────────────────────╯\n\n"
   "    Hi there!\n\n"
   (if (not (ollama-buddy--ollama-running))
       "    !!WARNING!! ollama server not running.\n\n"
     (format "    ollama RUNNING %s\n\n    Models available:\n\n%s\n\n"
             (if ollama-buddy-current-model
                 ollama-buddy-current-model
               "No model selected")
             (mapconcat (lambda (m) (format "      %s" m))
                        (ollama-buddy--get-models) "\n")))
   "    I'm here to help you with:\n\n"
   "    - Code refactoring and analysis\n"
   "    - Writing clear commit messages\n"
   "    - Proofreading and text improvements\n"
   "    - And much more!\n\n"
   "    Quick Start/Tips:\n\n"
   "    - Type a prompt in this buffer and select/mark it\n"
   "    - Run M-x ollama-buddy-menu and pick an option\n"
   "    - See ollama response in this buffer\n"
   "    - Switch models with [m], cancel with [x]\n"
   "    - Send selected/marked text from any buffer\n\n"
   ollama-buddy-separator-2 "\n\n"))

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
               (setq ollama-buddy-current-model 
                     (completing-read "Model: " (ollama-buddy--get-models) nil t))))))
    (?h . ("Help assistant" 
           (lambda ()
             (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
             (goto-char (point-max))
             (insert (ollama-buddy--create-intro-message)))))
    (?l . ("Send region" (lambda () (ollama-buddy--send))))
    (?r . ("Refactor code" (lambda () (ollama-buddy--send "refactor the following code:"))))
    (?g . ("Git commit message" (lambda () (ollama-buddy--send "write a concise git commit message for the following:"))))
    (?d . ("Describe code" (lambda () (ollama-buddy--send "describe the following code:"))))
    (?p . ("Proofread text" (lambda () (ollama-buddy--send "proofread the following:"))))
    (?z . ("Make concise" (lambda () (ollama-buddy--send "reduce wordiness while preserving meaning:"))))
    (?c . ("Custom prompt" 
           (lambda ()
             (when-let ((prefix (read-string "Enter prompt prefix: " nil nil nil t)))
               (unless (string-empty-p prefix)
                 (ollama-buddy--send prefix))))))
    (?x . ("Kill request" 
           (lambda ()
             (if (process-live-p ollama-buddy--active-process)
                 (progn
                   (kill-process ollama-buddy--active-process)
                   (setq ollama-buddy--active-process nil)
                   (error "Killed"))
               (error "No process running")))))
    (?q . ("Quit" (lambda () (message "Quit Ollama Shell menu.")))))
  "Menu items definition for Ollama Buddy."
  :type '(alist :key-type character
                :value-type (group (string :tag "Description")
                                   (function :tag "Action")))
  :group 'ollama-buddy)

(defun ollama-buddy--process-filter (proc output)
  "Process Ollama output stream."
  (when-let* ((json-data (ignore-errors (json-read-from-string output)))
              (text (alist-get 'content (alist-get 'message json-data))))
    (with-current-buffer ollama-buddy--chat-buffer
      (goto-char (point-max))
      (let ((inhibit-read-only t))(insert text))
      (redisplay))))

(defun ollama-buddy--ollama-running ()
  "t if ollama is running. nil if ollama not running"
  (if (eq (call-process "curl" nil nil nil "--silent" "--fail" "http://localhost:11434/api/tags") 0) t nil))

(defun ollama-buddy--send (&optional system-prompt)
  "Send region to Ollama and stream response."
  (interactive)
  (unless (ollama-buddy--ollama-running)
    (error "Ollama server is not running"))
  (unless (use-region-p)
    (error "Please select region containing query"))
  (unless ollama-buddy-current-model
    (setq ollama-buddy-current-model
          (completing-read "!! Select Model First !!: " (ollama-buddy--get-models) nil t)))
  (when (process-live-p ollama-buddy--active-process)
    (kill-process ollama-buddy--active-process))
  (let* ((prompt (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt-with-system (if system-prompt (concat system-prompt "\n\n" prompt) prompt))
         (json-payload (replace-regexp-in-string 
                       "\'" "\\\\\"" 
                       (json-encode
                        `(("model" . ,ollama-buddy-current-model)
                          ("messages" . [(("role" . "user")
                                        ("content" . ,prompt-with-system))])))))
         (buf (get-buffer-create ollama-buddy--chat-buffer)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (when (= (buffer-size) 0)
        (insert (ollama-buddy--create-intro-message)))
      (insert "\n\n" ollama-buddy-separator-1 "\n")
      (insert (format "\n[User: PROMPT] %s\n" prompt-with-system))
      (insert "\n" ollama-buddy-separator-2 "\n")
      (insert (format "\n[%s: RESPONSE] ... \n\n" ollama-buddy-current-model))
      (visual-line-mode 1))
    (setq ollama-buddy--active-process
          (start-process-shell-command
           "ollama-buddy--chat" buf
           (format "curl -s -X POST http://localhost:11434/api/chat -H 'Content-Type: application/json' -d '%s'"
                   json-payload)))
    (set-process-filter ollama-buddy--active-process #'ollama-buddy--process-filter)))

(defun ollama-buddy--get-models ()
  "Retrieve available Ollama models. If Ollama is not running, report an error."
  (interactive)
  (when (ollama-buddy--ollama-running)
    (thread-last
      (with-temp-buffer
        (call-process "curl" nil t nil "-s" "http://localhost:11434/api/tags")
        (goto-char (point-min))
        (json-read))
      (alist-get 'models)
      (mapcar (lambda (m) (alist-get 'name m))))))

(defun ollama-buddy-menu ()
  "Display the Ollama Buddy menu."
  (interactive)
  (let* ((prompt (propertize 
                  (ollama-buddy--format-menu 
                   ollama-buddy-menu-items 
                   ollama-buddy-menu-columns)
                  'face 'minibuffer-prompt))
         (key (read-key prompt))
         (cmd (assoc key ollama-buddy-menu-items)))
    (when cmd
      (funcall (caddr cmd)))))

(global-set-key (kbd "C-c l") #'ollama-buddy-menu)
