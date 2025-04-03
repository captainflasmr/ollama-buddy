;;; ollama-buddy-awesome.el --- Awesome ChatGPT Prompts integration for ollama-buddy -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides integration between ollama-buddy and the awesome-chatgpt-prompts
;; repository (https://github.com/f/awesome-chatgpt-prompts).
;; It enables on-demand syncing of prompts and using them as system prompts.

;;; Code:

(require 'ollama-buddy-core)
(require 'json)
(require 'csv)

(declare-function ollama-buddy--send "ollama-buddy")
(declare-function ollama-buddy--prepare-prompt-area "ollama-buddy")

(defgroup ollama-buddy-awesome nil
  "Customization group for ollama-buddy-awesome."
  :group 'ollama-buddy
  :prefix "ollama-buddy-awesome-")

(defcustom ollama-buddy-awesome-repo-url "https://github.com/f/awesome-chatgpt-prompts.git"
  "URL of the Awesome ChatGPT Prompts GitHub repository."
  :type 'string
  :group 'ollama-buddy-awesome)

(defcustom ollama-buddy-awesome-local-dir (expand-file-name "awesome-chatgpt-prompts" user-emacs-directory)
  "Local directory where Awesome ChatGPT Prompts will be stored."
  :type 'directory
  :group 'ollama-buddy-awesome)

(defcustom ollama-buddy-awesome-prompts-file "prompts.csv"
  "Filename containing the prompts within the repository."
  :type 'string
  :group 'ollama-buddy-awesome)

(defcustom ollama-buddy-awesome-update-on-startup nil
  "Whether to automatically update prompts when Emacs starts."
  :type 'boolean
  :group 'ollama-buddy-awesome)

(defcustom ollama-buddy-awesome-categorize-prompts t
  "Whether to categorize prompts based on common keywords."
  :type 'boolean
  :group 'ollama-buddy-awesome)

(defvar ollama-buddy-awesome--prompts nil
  "Cache of available awesome-chatgpt prompts.")

(defvar ollama-buddy-awesome--last-sync-time nil
  "Timestamp of the last synchronization with the remote repository.")

(defvar ollama-buddy-awesome--sync-in-progress nil
  "Flag indicating whether a sync operation is currently in progress.")

(defvar ollama-buddy-awesome--categories
  '(("writing" . ("writer" "writing" "copywriter" "storyteller" "poet" "screenwriter" "novelist"))
    ("code" . ("programming" "developer" "code" "programmer" "software" "engineer"))
    ("business" . ("business" "entrepreneur" "marketing" "consultant" "advisor" "strategist"))
    ("academic" . ("academic" "professor" "educational" "scientist" "researcher" "teacher"))
    ("creative" . ("creative" "artist" "designer" "art" "visual" "photographer"))
    ("philosophy" . ("philosopher" "philosophical" "ethics" "logic" "reasoning"))
    ("health" . ("doctor" "dietitian" "nutritionist" "therapist" "psychologist" "counselor" "fitness"))
    ("legal" . ("legal" "lawyer" "attorney" "law" "judicial"))
    ("finance" . ("financial" "accountant" "investment" "finance" "economist")))
  "Categories for classifying prompts based on keywords.")

(defun ollama-buddy-awesome--prompts-path ()
  "Return the full path to the prompts CSV file."
  (expand-file-name ollama-buddy-awesome-prompts-file
                    ollama-buddy-awesome-local-dir))

(defun ollama-buddy-awesome--ensure-repo-exists ()
  "Ensure the Awesome ChatGPT Prompts repository exists locally, cloning it if needed."
  (let ((default-directory (file-name-directory ollama-buddy-awesome-local-dir)))
    (unless (file-exists-p ollama-buddy-awesome-local-dir)
      (make-directory ollama-buddy-awesome-local-dir t))
    
    (if (file-exists-p (expand-file-name ".git" ollama-buddy-awesome-local-dir))
        ;; Repo exists, initialize sparse checkout
        (ollama-buddy-awesome--setup-sparse-checkout)
      ;; Need to clone the repo with sparse checkout
      (ollama-buddy-awesome--clone-repo))))

(defun ollama-buddy-awesome--clone-repo ()
  "Clone the Awesome ChatGPT Prompts repository with sparse checkout."
  (let ((default-directory (file-name-directory ollama-buddy-awesome-local-dir)))
    (message "Cloning Awesome ChatGPT Prompts repository (sparse checkout)...")
    
    ;; Initialize empty repo
    (call-process "git" nil "*Awesome Prompts Clone Output*" nil
                  "init" ollama-buddy-awesome-local-dir)
    
    (let ((default-directory ollama-buddy-awesome-local-dir))
      ;; Add remote
      (call-process "git" nil "*Awesome Prompts Clone Output*" nil
                    "remote" "add" "origin" ollama-buddy-awesome-repo-url)
      
      ;; Set up sparse checkout
      (ollama-buddy-awesome--setup-sparse-checkout)
      
      ;; Pull the content
      (call-process "git" nil "*Awesome Prompts Clone Output*" nil
                    "pull" "origin" "main")
      
      (message "Awesome ChatGPT Prompts repository cloned successfully!")
      (setq ollama-buddy-awesome--last-sync-time (current-time)))))

(defun ollama-buddy-awesome--setup-sparse-checkout ()
  "Configure sparse checkout for the Awesome ChatGPT Prompts repository."
  (let ((default-directory ollama-buddy-awesome-local-dir))
    ;; Enable sparse checkout
    (call-process "git" nil "*Awesome Prompts Sparse Output*" nil
                  "config" "core.sparseCheckout" "true")
    
    ;; Create sparse-checkout file with just the prompts.csv and README
    (with-temp-file (expand-file-name ".git/info/sparse-checkout" ollama-buddy-awesome-local-dir)
      (insert (format "/%s\n" ollama-buddy-awesome-prompts-file))
      (insert "/README.md\n"))))

(defun ollama-buddy-awesome-sync-prompts ()
  "Sync the latest prompts from the Awesome ChatGPT Prompts GitHub repository."
  (interactive)
  (when ollama-buddy-awesome--sync-in-progress
    (user-error "Sync already in progress, please wait"))
  
  (setq ollama-buddy-awesome--sync-in-progress t)
  (message "Syncing Awesome ChatGPT Prompts...")
  
  (let ((sync-buffer (get-buffer-create "*Awesome ChatGPT Prompts Sync*")))
    (with-current-buffer sync-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (view-mode -1)
        (insert "=== Syncing Awesome ChatGPT Prompts ===\n\n")))
    
    ;; Run the sync in a separate process to avoid blocking Emacs
    (make-process
     :name "awesome-prompts-sync"
     :buffer sync-buffer
     :command (list "bash" "-c" 
                    (format "mkdir -p %s && cd %s && \
                           (git -c advice.detachedHead=false fetch origin main && \
                            git -c advice.detachedHead=false checkout FETCH_HEAD -- %s README.md \
                            || echo 'Failed to sync prompts') 2>&1"
                            ollama-buddy-awesome-local-dir
                            ollama-buddy-awesome-local-dir
                            ollama-buddy-awesome-prompts-file))
     :sentinel (lambda (process event)
                 (when (string-match-p "finished" event)
                   (with-current-buffer (process-buffer process)
                     (let ((inhibit-read-only t))
                       (goto-char (point-max))
                       (insert "\n\n=== Sync completed ===\n")
                       (ollama-buddy-awesome-populate-prompts)
                       (insert (format "\nFound %d prompts\n" 
                                       (length ollama-buddy-awesome--prompts)))
                       (view-mode 1)))
                   (setq ollama-buddy-awesome--last-sync-time (current-time))
                   (setq ollama-buddy-awesome--sync-in-progress nil)
                   (message "Awesome ChatGPT Prompts synced successfully!")))
     :noquery t)))

(defun ollama-buddy-awesome--parse-csv-line (line)
  "Parse a CSV LINE into fields, correctly handling quotes.
This is a simplified but more robust version that handles the actual format
of the awesome-chatgpt-prompts CSV file."
  (with-temp-buffer
    (insert line)
    (goto-char (point-min))
    
    (let ((fields nil)
          (field-start (point))
          (in-quotes nil))
      
      (while (< (point) (point-max))
        (cond
         ;; Handle quoted fields
         ((and (eq (char-after) ?\") (not in-quotes))
          (setq in-quotes t)
          (forward-char 1)
          (setq field-start (point)))
         
         ((and (eq (char-after) ?\") in-quotes)
          (if (and (< (1+ (point)) (point-max)) 
                   (eq (char-after (1+ (point))) ?\"))
              ;; Double quote within quoted field - skip it
              (forward-char 2)
            ;; End of quoted field
            (let ((field-value (buffer-substring-no-properties field-start (point))))
              (push field-value fields)
              (setq in-quotes nil)
              (forward-char 1)
              ;; Skip to next field or end
              (if (search-forward "," nil t)
                  (setq field-start (point))
                (goto-char (point-max))))))
         
         ;; Handle commas outside quotes
         ((and (eq (char-after) ?,) (not in-quotes))
          (let ((field-value (buffer-substring-no-properties field-start (point))))
            (push field-value fields)
            (forward-char 1)
            (setq field-start (point))))
         
         ;; Just move forward for other characters
         (t (forward-char 1))))
      
      ;; Handle last field if we're at the end
      (when (< field-start (point-max))
        (push (buffer-substring-no-properties field-start (point-max)) fields))
      
      (nreverse fields))))

(defun ollama-buddy-awesome--categorize-prompt (title content)
  "Assign a category to a prompt based on TITLE and CONTENT."
  (catch 'category
    (dolist (category-pair ollama-buddy-awesome--categories)
      (let ((category (car category-pair))
            (keywords (cdr category-pair)))
        (dolist (keyword keywords)
          (when (or (string-match-p keyword (downcase title))
                    (string-match-p keyword (downcase content)))
            (throw 'category category)))))
    "other"))  ;; Default category if no matches

(defun ollama-buddy-awesome-populate-prompts ()
  "Populate the list of available prompts from the local repository."
  (interactive)
  ;; Ensure the repository exists
  (unless (file-exists-p (ollama-buddy-awesome--prompts-path))
    (ollama-buddy-awesome--ensure-repo-exists))
  
  (setq ollama-buddy-awesome--prompts nil)
  
  (when (file-exists-p (ollama-buddy-awesome--prompts-path))
    (message "Loading prompts from %s..." (ollama-buddy-awesome--prompts-path))
    
    ;; We'll read the file manually with basic CSV parsing
    (with-temp-buffer
      (insert-file-contents (ollama-buddy-awesome--prompts-path))
      (goto-char (point-min))
      
      ;; Skip header line if it exists
      (when (looking-at-p "act,prompt")
        (forward-line 1))
      
      ;; Process each line
      (let ((line-count 0)
            (success-count 0))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties 
                        (line-beginning-position) 
                        (line-end-position))))
            
            (setq line-count (1+ line-count))
            
            ;; Skip empty lines
            (unless (string-empty-p (string-trim line))
              ;; Simple parsing for lines with the format: "title","content"
              (if (string-match "^\"\\([^\"]*\\)\",\"\\([^\"]*\\)\"" line)
                  (let ((act (match-string 1 line))
                        (prompt (match-string 2 line)))
                    (when (and act prompt 
                               (not (string-empty-p act))
                               (not (string-empty-p prompt)))
                      (let* ((category (if ollama-buddy-awesome-categorize-prompts
                                           (ollama-buddy-awesome--categorize-prompt act prompt)
                                         "general"))
                             (entry (list :title act
                                          :content prompt
                                          :category category)))
                        (push entry ollama-buddy-awesome--prompts)
                        (setq success-count (1+ success-count)))))
                ;; Try fallback parsing for more complex cases
                (condition-case err
                    (let ((parts nil))
                      ;; Split by first comma outside quotes
                      (with-temp-buffer
                        (insert line)
                        (goto-char (point-min))
                        
                        ;; Find the title part
                        (let* ((start (if (eq (char-after) ?\") 
                                          (progn (forward-char) (point))
                                        (point)))
                               (title-end nil))
                          ;; Find end of title
                          (if (eq (char-after) ?\")
                              ;; Title is quoted
                              (progn
                                (while (and (search-forward "\"" nil t)
                                            (or (= (point) (1+ start))
                                                (eq (char-before (1- (point))) ?\\))))
                                (setq title-end (1- (point))))
                            ;; Title is not quoted
                            (if (search-forward "," nil t)
                                (setq title-end (1- (point)))
                              (setq title-end (point-max))))
                          
                          ;; Extract title
                          (when title-end
                            (let ((title (buffer-substring-no-properties 
                                          start title-end)))
                              (push title parts)
                              
                              ;; Move past comma if needed
                              (when (and (< (point) (point-max))
                                         (eq (char-after) ?,))
                                (forward-char)))))
                        
                        ;; The rest is the prompt content
                        (when (< (point) (point-max))
                          (let* ((start (if (eq (char-after) ?\") 
                                           (progn (forward-char) (point))
                                         (point)))
                                 (content (buffer-substring-no-properties 
                                           start (point-max))))
                            ;; Remove trailing quote if present
                            (when (and (> (length content) 0)
                                       (eq (aref content (1- (length content))) ?\"))
                              (setq content (substring content 0 (1- (length content)))))
                            (push content parts))))
                      
                      ;; Process parts
                      (setq parts (nreverse parts))
                      (when (>= (length parts) 2)
                        (let* ((act (car parts))
                               (prompt (cadr parts))
                               (category (if ollama-buddy-awesome-categorize-prompts
                                            (ollama-buddy-awesome--categorize-prompt act prompt)
                                          "general"))
                               (entry (list :title act
                                            :content prompt
                                            :category category)))
                          (push entry ollama-buddy-awesome--prompts)
                          (setq success-count (1+ success-count)))))
                  (error
                   (message "Error parsing line %d: %s" line-count (error-message-string err)))))))
          
          (forward-line 1))
        
        (message "Processed %d lines, loaded %d prompts" line-count success-count))))
  
  ;; Sort prompts by category and title
  (setq ollama-buddy-awesome--prompts
        (sort ollama-buddy-awesome--prompts
              (lambda (a b)
                (let ((cat-a (plist-get a :category))
                      (cat-b (plist-get b :category))
                      (title-a (plist-get a :title))
                      (title-b (plist-get b :title)))
                  (if (string= cat-a cat-b)
                      (string< title-a title-b)
                    (string< cat-a cat-b)))))))

(defun ollama-buddy-awesome--format-prompt-name (prompt)
  "Format PROMPT name for display in the completion UI."
  (let ((category (plist-get prompt :category))
        (title (plist-get prompt :title)))
    (format "%s: %s" 
            (propertize category 'face 'font-lock-type-face)
            (propertize title 'face 'font-lock-function-name-face))))

(defun ollama-buddy-awesome-yield-prompt ()
  "Select an Awesome ChatGPT Prompt and return its content."
  (unless ollama-buddy-awesome--prompts
    (ollama-buddy-awesome-populate-prompts))
  
  (let* ((formatted-prompts (mapcar #'ollama-buddy-awesome--format-prompt-name 
                                    ollama-buddy-awesome--prompts))
         (prompt-alist (cl-mapcar #'cons formatted-prompts 
                                  ollama-buddy-awesome--prompts))
         (selected-formatted (completing-read "Awesome ChatGPT Prompt: " formatted-prompts nil t))
         (selected-prompt (cdr (assoc selected-formatted prompt-alist))))
    
    (plist-get selected-prompt :content)))

(defun ollama-buddy-awesome-send ()
  "Apply an Awesome ChatGPT Prompt to the selected text and send to Ollama."
  (interactive)
  (let ((system-prompt (ollama-buddy-awesome-yield-prompt))
        (selected-text (when (use-region-p)
                         (buffer-substring-no-properties
                          (region-beginning) (region-end)))))
    
    (unless selected-text
      (setq selected-text (read-string "Enter text to process: ")))
    
    ;; Set the system prompt
    (setq ollama-buddy--current-system-prompt system-prompt)
    
    ;; Prepare the chat buffer
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (pop-to-buffer (current-buffer))
      (ollama-buddy--prepare-prompt-area t nil)  ;; New prompt, no content
      (goto-char (point-max))
      (insert (string-trim selected-text)))
    
    ;; Send the request
    (ollama-buddy--send selected-text)))

(defun ollama-buddy-awesome-list-prompts ()
  "Display a list of available Awesome ChatGPT Prompts."
  (interactive)
  (unless ollama-buddy-awesome--prompts
    (ollama-buddy-awesome-populate-prompts))
  
  (let ((buf (get-buffer-create "*Awesome ChatGPT Prompts List*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "* Awesome ChatGPT Prompts\n\n")
        
        (if ollama-buddy-awesome--last-sync-time
            (insert (format "Last synced: %s\n\n" 
                            (format-time-string "%Y-%m-%d %H:%M:%S" 
                                                ollama-buddy-awesome--last-sync-time)))
          (insert "Never synced with GitHub repository\n\n"))
        
        (insert "** Available Prompts\n")
        
        (let ((current-category ""))
          (dolist (prompt ollama-buddy-awesome--prompts)
            (let ((category (plist-get prompt :category))
                  (title (plist-get prompt :title))
                  (content (plist-get prompt :content)))
              
              ;; Add category header if changed
              (unless (string= category current-category)
                (insert (format "*** %s\n" (capitalize category)))
                (setq current-category category))
              
              ;; Prompt title and content preview
              (insert (format "**** %s\n" title))
              (if (> (length content) 250)
                  (insert (concat (substring content 0 247) "...\n"))
                (insert (concat content "\n")))
              (goto-char (point-max))))))
      (view-mode 1)
      (goto-char (point-min)))
    (display-buffer buf)))

(defun ollama-buddy-awesome-show-prompt (formatted-name)
  "Display the full content of a prompt with FORMATTED-NAME."
  (interactive
   (list (completing-read "Show prompt: " 
                          (mapcar #'ollama-buddy-awesome--format-prompt-name 
                                  ollama-buddy-awesome--prompts))))
  
  (let* ((prompt-alist (cl-mapcar #'cons 
                                 (mapcar #'ollama-buddy-awesome--format-prompt-name 
                                         ollama-buddy-awesome--prompts)
                                 ollama-buddy-awesome--prompts))
         (selected-prompt (cdr (assoc formatted-name prompt-alist))))
    
    (with-current-buffer (get-buffer-create "*Awesome ChatGPT Prompt*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "* %s\n\n" (plist-get selected-prompt :title)))
        (insert (plist-get selected-prompt :content))
        (goto-char (point-min))
        (view-mode 1))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun ollama-buddy-awesome-setup ()
  "Set up the ollama-buddy-awesome package."
  (interactive)
  (ollama-buddy-awesome--ensure-repo-exists)
  (ollama-buddy-awesome-populate-prompts)
  (message "ollama-buddy-awesome setup complete. Found %d prompts."
           (length ollama-buddy-awesome--prompts)))

;; Add a command to set the system prompt without sending
(defun ollama-buddy-awesome-set-system-prompt ()
  "Set the system prompt to an Awesome ChatGPT Prompt without sending a request."
  (interactive)
  (let ((system-prompt (ollama-buddy-awesome-yield-prompt)))
    (setq ollama-buddy--current-system-prompt system-prompt)
    (message "System prompt set to Awesome ChatGPT Prompt")
    (ollama-buddy--update-status "Awesome prompt set")))

;; Initialize on load if configured
(when ollama-buddy-awesome-update-on-startup
  (with-eval-after-load 'ollama-buddy
    (run-with-idle-timer 3 nil #'ollama-buddy-awesome-sync-prompts)))

(provide 'ollama-buddy-awesome)
;;; ollama-buddy-awesome.el ends here
