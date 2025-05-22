;;; ollama-buddy-fabric.el --- Fabric integration for ollama-buddy -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/captainflasmr/ollama-buddy
;;
;;; Commentary:
;; This package provides integration between ollama-buddy and danielmiessler's
;; Fabric patterns (https://github.com/danielmiessler/fabric).
;; It enables on-demand syncing of patterns and using them as system prompts.

;;; Code:

(require 'ollama-buddy-core)

(declare-function ollama-buddy--send "ollama-buddy")
(declare-function ollama-buddy--prepare-prompt-area "ollama-buddy")
(declare-function ollama-buddy--open-chat "ollama-buddy")
(declare-function ollama-buddy--update-status "ollama-buddy")
(declare-function ollama-buddy--md-to-org-convert-region "ollama-buddy")

(defgroup ollama-buddy-fabric nil
  "Customization group for ollama-buddy-fabric."
  :group 'ollama-buddy
  :prefix "ollama-buddy-fabric-")

(defcustom ollama-buddy-fabric-repo-url "https://github.com/danielmiessler/fabric.git"
  "URL of the Fabric GitHub repository."
  :type 'string
  :group 'ollama-buddy-fabric)

(defcustom ollama-buddy-fabric-local-dir (expand-file-name "fabric" user-emacs-directory)
  "Local directory where Fabric patterns will be stored."
  :type 'directory
  :group 'ollama-buddy-fabric)

(defcustom ollama-buddy-fabric-patterns-subdir "patterns"
  "Subdirectory within the Fabric repo containing the patterns."
  :type 'string
  :group 'ollama-buddy-fabric)

(defcustom ollama-buddy-fabric-update-on-startup nil
  "Whether to automatically update patterns when Emacs starts."
  :type 'boolean
  :group 'ollama-buddy-fabric)

(defcustom ollama-buddy-fabric-pattern-categories '("universal" "code" "writing" "analysis")
  "List of pattern categories to focus on when listing patterns."
  :type '(repeat string)
  :group 'ollama-buddy-fabric)

(defvar ollama-buddy-fabric--patterns nil
  "Cache of available Fabric patterns.")

(defvar ollama-buddy-fabric--last-sync-time nil
  "Timestamp of the last synchronization with the remote repository.")

(defvar ollama-buddy-fabric--sync-in-progress nil
  "Flag indicating whether a sync operation is currently in progress.")

(defun ollama-buddy-fabric-set-system-prompt ()
  "Set the system prompt to a Fabric pattern without sending a request."
  (interactive)
  (unless ollama-buddy-fabric--patterns
    (ollama-buddy-fabric-populate-patterns))
  
  (let* ((formatted-patterns (mapcar #'ollama-buddy-fabric--format-pattern-name
                                     ollama-buddy-fabric--patterns))
         (pattern-alist (cl-mapcar #'cons formatted-patterns
                                   ollama-buddy-fabric--patterns))
         (selected-formatted (completing-read "Fabric pattern: " formatted-patterns nil t))
         (selected-pattern (cdr (assoc selected-formatted pattern-alist)))
         (system-file (expand-file-name (format "%s/%s/system.md"
                                                (ollama-buddy-fabric--patterns-path)
                                                selected-pattern)))
         (system-prompt (with-temp-buffer
                          (insert-file-contents system-file)
                          (buffer-string)))
         ;; Extract title from pattern name
         (title (replace-regexp-in-string "_" " " 
                                          (capitalize selected-pattern))))
    
    ;; Use the enhanced system prompt setting function
    (ollama-buddy--set-system-prompt-with-metadata system-prompt title "fabric")
    
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (ollama-buddy--open-chat))))

(defun ollama-buddy-fabric--patterns-path ()
  "Return the full path to the patterns directory."
  (expand-file-name ollama-buddy-fabric-patterns-subdir
                    ollama-buddy-fabric-local-dir))

(defun ollama-buddy-fabric--ensure-repo-exists ()
  "Ensure the Fabric repository exists locally, cloning it if needed."
  (let ((default-directory (file-name-directory ollama-buddy-fabric-local-dir)))
    (unless (file-exists-p ollama-buddy-fabric-local-dir)
      (make-directory ollama-buddy-fabric-local-dir t))
    
    (if (file-exists-p (expand-file-name ".git" ollama-buddy-fabric-local-dir))
        ;; Repo exists, initialize sparse checkout
        (ollama-buddy-fabric--setup-sparse-checkout)
      ;; Need to clone the repo with sparse checkout
      (ollama-buddy-fabric--clone-repo))))

(defun ollama-buddy-fabric--clone-repo ()
  "Clone the Fabric repository with sparse checkout."
  (let ((default-directory (file-name-directory ollama-buddy-fabric-local-dir)))
    (message "Cloning Fabric repository (sparse checkout)...")
    
    ;; Initialize empty repo
    (call-process "git" nil "*Fabric Clone Output*" nil
                  "init" ollama-buddy-fabric-local-dir)
    
    (let ((default-directory ollama-buddy-fabric-local-dir))
      ;; Add remote
      (call-process "git" nil "*Fabric Clone Output*" nil
                    "remote" "add" "origin" ollama-buddy-fabric-repo-url)
      
      ;; Set up sparse checkout
      (ollama-buddy-fabric--setup-sparse-checkout)
      
      ;; Pull the content
      (call-process "git" nil "*Fabric Clone Output*" nil
                    "pull" "origin" "main")
      
      (message "Fabric repository cloned successfully!")
      (setq ollama-buddy-fabric--last-sync-time (current-time)))))

(defun ollama-buddy-fabric--setup-sparse-checkout ()
  "Configure sparse checkout for the Fabric repository."
  (let ((default-directory ollama-buddy-fabric-local-dir))
    ;; Enable sparse checkout
    (call-process "git" nil "*Fabric Sparse Output*" nil
                  "config" "core.sparseCheckout" "true")
    
    ;; Create sparse-checkout file with patterns directory
    (with-temp-file (expand-file-name ".git/info/sparse-checkout" ollama-buddy-fabric-local-dir)
      (insert (format "/%s/\n" ollama-buddy-fabric-patterns-subdir))
      ;; Make sure we get the README for reference
      (insert "/README.md\n"))))

(defun ollama-buddy-fabric-sync-patterns ()
  "Sync the latest patterns from the Fabric GitHub repository."
  (interactive)
  (when ollama-buddy-fabric--sync-in-progress
    (user-error "Sync already in progress, please wait"))
  
  (setq ollama-buddy-fabric--sync-in-progress t)
  (message "Syncing Fabric patterns...")
  
  (let ((sync-buffer (get-buffer-create "*Fabric Sync*")))
    (with-current-buffer sync-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (view-mode -1)
        (insert "=== Syncing Fabric Patterns ===\n\n")))
    
    ;; Run the sync in a separate process to avoid blocking Emacs
    (make-process
     :name "fabric-sync"
     :buffer sync-buffer
     :command (list "bash" "-c"
                    (format "mkdir -p %s && cd %s && \
                           (git -c advice.detachedHead=false fetch origin main && \
                            git -c advice.detachedHead=false checkout FETCH_HEAD -- %s \
                            || echo 'Failed to sync patterns') 2>&1"
                            ollama-buddy-fabric-local-dir
                            ollama-buddy-fabric-local-dir
                            ollama-buddy-fabric-patterns-subdir))
     :sentinel (lambda (process event)
                 (when (string-match-p "finished" event)
                   (with-current-buffer (process-buffer process)
                     (let ((inhibit-read-only t))
                       (goto-char (point-max))
                       (insert "\n\n=== Sync completed ===\n")
                       (ollama-buddy-fabric-populate-patterns)
                       (insert (format "\nFound %d patterns\n"
                                       (length ollama-buddy-fabric--patterns)))
                       (view-mode 1)))
                   (setq ollama-buddy-fabric--last-sync-time (current-time))
                   (setq ollama-buddy-fabric--sync-in-progress nil)
                   (message "Fabric patterns synced successfully!")))
     :noquery t)))

(defun ollama-buddy-fabric-populate-patterns ()
  "Populate the list of available patterns from the local repository."
  (interactive)
  ;; Ensure the patterns directory exists
  (unless (file-exists-p (ollama-buddy-fabric--patterns-path))
    (ollama-buddy-fabric--ensure-repo-exists))
  
  (setq ollama-buddy-fabric--patterns
        (cl-remove-if-not
         (lambda (pattern)
           (let ((pattern-dir (expand-file-name pattern (ollama-buddy-fabric--patterns-path))))
             (and (file-directory-p pattern-dir)
                  ;; Check if system.md exists
                  (file-exists-p (expand-file-name "system.md" pattern-dir)))))
         ;; Get all directories that don't start with a dot
         (directory-files (ollama-buddy-fabric--patterns-path) nil "^[^.]" t)))
  
  ;; Sort patterns by category priority if possible
  (setq ollama-buddy-fabric--patterns
        (sort ollama-buddy-fabric--patterns
              (lambda (a b)
                (let ((a-cat (ollama-buddy-fabric--pattern-category a))
                      (b-cat (ollama-buddy-fabric--pattern-category b)))
                  (< (or (cl-position a-cat ollama-buddy-fabric-pattern-categories :test #'string=) 999)
                     (or (cl-position b-cat ollama-buddy-fabric-pattern-categories :test #'string=) 999)))))))

(defun ollama-buddy-fabric--pattern-category (pattern)
  "Extract the category from PATTERN name."
  (let ((parts (split-string pattern "_")))
    (if (> (length parts) 0)
        (car parts)
      "")))

(defun ollama-buddy-fabric--format-pattern-name (pattern)
  "Format PATTERN name for display in the completion UI."
  (let* ((parts (split-string pattern "_"))
         (category (if (> (length parts) 0) (car parts) ""))
         (name (mapconcat #'identity (cdr parts) "_")))
    (format "%s: %s"
            (propertize category 'face 'font-lock-type-face)
            (propertize name 'face 'font-lock-function-name-face))))

(defun ollama-buddy-fabric-yield-prompt ()
  "Select a Fabric pattern and return its content."
  (unless ollama-buddy-fabric--patterns
    (ollama-buddy-fabric-populate-patterns))
  
  (let* ((formatted-patterns (mapcar #'ollama-buddy-fabric--format-pattern-name
                                     ollama-buddy-fabric--patterns))
         (pattern-alist (cl-mapcar #'cons formatted-patterns
                                   ollama-buddy-fabric--patterns))
         (selected-formatted (completing-read "Fabric pattern: " formatted-patterns nil t))
         (selected-pattern (cdr (assoc selected-formatted pattern-alist)))
         (system-file (expand-file-name (format "%s/%s/system.md"
                                                (ollama-buddy-fabric--patterns-path)
                                                selected-pattern))))
    
    (with-temp-buffer
      (insert-file-contents system-file)
      (let ((content (buffer-string)))
        content))))

(defun ollama-buddy-fabric-send ()
  "Apply a Fabric pattern to the selected text and send to Ollama."
  (interactive)
  (unless ollama-buddy-fabric--patterns
    (ollama-buddy-fabric-populate-patterns))
  
  (let* ((formatted-patterns (mapcar #'ollama-buddy-fabric--format-pattern-name
                                     ollama-buddy-fabric--patterns))
         (pattern-alist (cl-mapcar #'cons formatted-patterns
                                   ollama-buddy-fabric--patterns))
         (selected-formatted (completing-read "Fabric pattern: " formatted-patterns nil t))
         (selected-pattern (cdr (assoc selected-formatted pattern-alist)))
         (system-file (expand-file-name (format "%s/%s/system.md"
                                                (ollama-buddy-fabric--patterns-path)
                                                selected-pattern)))
         (system-prompt (with-temp-buffer
                          (insert-file-contents system-file)
                          (buffer-string)))
         (title (replace-regexp-in-string "_" " " 
                                          (capitalize selected-pattern)))
         (selected-text (when (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning) (region-end)))))
    
    (unless selected-text
      (setq selected-text (read-string "Enter text to process: ")))
    
    ;; Set the system prompt with metadata
    (ollama-buddy--set-system-prompt-with-metadata system-prompt title "fabric")
    
    ;; Prepare the chat buffer
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (ollama-buddy--open-chat)
      (ollama-buddy--prepare-prompt-area t nil)  ;; New prompt, no content
      (insert (string-trim selected-text)))
    
    ;; Send the request
    (ollama-buddy--send selected-text)))

(defun ollama-buddy-fabric--extract-description-from-system (pattern)
  "Extract a brief description from PATTERN's system.md file.
Returns the first paragraph (up to 250 chars) as a description."
  (let ((system-file (expand-file-name (format "%s/%s/system.md"
                                               (ollama-buddy-fabric--patterns-path)
                                               pattern))))
    (when (file-exists-p system-file)
      (with-temp-buffer
        (insert-file-contents system-file)
        (goto-char (point-min))
        ;; Skip any YAML frontmatter if present (between --- markers)
        (when (looking-at "---")
          (forward-line 1)
          (if (re-search-forward "^---$" nil t)
              (forward-line 1)
            (goto-char (point-min))))
        
        ;; Skip any markdown heading at the beginning
        (when (looking-at "^#+ ")
          (forward-line 1))
        
        ;; Get the first paragraph
        (let ((start (point))
              (end (progn
                     (if (re-search-forward "\n\n" nil t)
                         (match-beginning 0)
                       (point-max)))))
          ;; Limit to ~250 chars for preview
          (let ((desc (buffer-substring-no-properties start end)))
              desc))))))

(defun ollama-buddy-fabric-list-patterns ()
  "Display a list of available Fabric patterns with descriptions."
  (interactive)
  (unless ollama-buddy-fabric--patterns
    (ollama-buddy-fabric-populate-patterns))
  
  (let ((buf (get-buffer-create "*Fabric Patterns List*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (setq-local org-hide-emphasis-markers t)
        (setq-local org-hide-leading-stars t)

        (insert "#+TITLE: Fabric Patterns\n\n")
        
        (if ollama-buddy-fabric--last-sync-time
            (insert (format "Last synced: %s\n\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S"
                                                ollama-buddy-fabric--last-sync-time)))
          (insert "Never synced with GitHub repository\n\n"))
        
        (let ((current-category ""))
          (dolist (pattern ollama-buddy-fabric--patterns)
            (let* ((parts (split-string pattern "_"))
                   (category (if (> (length parts) 0) (car parts) ""))
                   (name (mapconcat #'identity (cdr parts) "_"))
                   (desc-file (expand-file-name
                               (format "%s/%s/description.md"
                                       (ollama-buddy-fabric--patterns-path)
                                       pattern))))
              
              ;; Add category header if changed
              (unless (string= category current-category)
                (insert (format "* %s\n\n" (capitalize category)))
                (setq current-category category))
              
              ;; Pattern name and description
              (insert (format "** %s\n\n" name))
              (if (file-exists-p desc-file)
                  (progn
                    (insert-file-contents (string-trim desc-file))
                    (insert "\n\n"))
                (insert
                 (string-trim
                  (ollama-buddy-fabric--extract-description-from-system pattern))
                 "\n\n"))
              (goto-char (point-max))))))
      (my/org-hide-sublevels 2)
      (view-mode 1)
      (goto-char (point-min)))
    (display-buffer buf)))

(defun ollama-buddy-fabric-show-pattern (pattern)
  "Display the full content of a PATTERN."
  (interactive
   (list (completing-read "Show pattern: "
                          (mapcar #'ollama-buddy-fabric--format-pattern-name
                                  ollama-buddy-fabric--patterns))))
  
  (let* ((pattern-name (car (last (split-string pattern ": "))))
         (real-pattern (seq-find (lambda (p) (string-match-p pattern-name p))
                                 ollama-buddy-fabric--patterns))
         (system-file (expand-file-name (format "%s/%s/system.md"
                                                (ollama-buddy-fabric--patterns-path)
                                                real-pattern))))
    
    (with-current-buffer (get-buffer-create "*Fabric Pattern*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert-file-contents system-file)
        (ollama-buddy--md-to-org-convert-region (point-min) (point-max))
        (goto-char (point-min))
        (view-mode 1))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun ollama-buddy-fabric-setup ()
  "Set up the ollama-buddy-fabric package."
  (interactive)
  (ollama-buddy-fabric--ensure-repo-exists)
  (ollama-buddy-fabric-populate-patterns)
  (message "ollama-buddy-fabric setup complete. Found %d patterns."
           (length ollama-buddy-fabric--patterns)))

;; Initialize on load if configured
(when ollama-buddy-fabric-update-on-startup
  (with-eval-after-load 'ollama-buddy
    (run-with-idle-timer 3 nil #'ollama-buddy-fabric-sync-patterns)))

(provide 'ollama-buddy-fabric)
;;; ollama-buddy-fabric.el ends here
