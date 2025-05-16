;;; ollama-buddy-user-prompts.el --- User system prompts for ollama-buddy -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/captainflasmr/ollama-buddy
;;
;;; Commentary:
;; This package provides functionality for users to save and manage their own
;; system prompts for use with ollama-buddy. Prompts are stored as org-mode
;; files with a categorized naming convention.

;;; Code:

(require 'ollama-buddy-core)

(declare-function ollama-buddy--open-chat "ollama-buddy")
(declare-function ollama-buddy--update-status "ollama-buddy")

(defgroup ollama-buddy-user-prompts nil
  "Customization group for ollama-buddy user system prompts."
  :group 'ollama-buddy
  :prefix "ollama-buddy-user-prompts-")

(defcustom ollama-buddy-user-prompts-directory
  (expand-file-name "ollama-buddy-user-prompts" user-emacs-directory)
  "Directory where user system prompts are stored."
  :type 'directory
  :group 'ollama-buddy-user-prompts)

(defcustom ollama-buddy-user-prompts-default-categories
  '("general" "coding" "writing" "analysis" "creative" "technical" "documentation")
  "List of default categories for user system prompts."
  :type '(repeat string)
  :group 'ollama-buddy-user-prompts)

(defvar ollama-buddy-user-prompts--cache nil
  "Cache of available user system prompts.")

(defvar ollama-buddy-user-prompts--last-refresh nil
  "Timestamp of the last cache refresh.")

(defvar ollama-buddy-user-prompts--cache-ttl 5
  "Time-to-live for the prompts cache in seconds.")

(defun ollama-buddy-user-prompts--ensure-directory ()
  "Ensure the user prompts directory exists."
  (unless (file-directory-p ollama-buddy-user-prompts-directory)
    (make-directory ollama-buddy-user-prompts-directory t)))

(defun ollama-buddy-user-prompts--sanitize-title (title)
  "Sanitize TITLE for use in filename by replacing spaces with dashes."
  (replace-regexp-in-string "[[:space:]]+" "-" 
                            (replace-regexp-in-string "[^[:alnum:][:space:]-]" "" title)))

(defun ollama-buddy-user-prompts--format-filename (category title)
  "Format filename using CATEGORY and TITLE."
  (format "%s__%s__system.org" 
          (downcase category)
          (ollama-buddy-user-prompts--sanitize-title title)))

(defun ollama-buddy-user-prompts--parse-filename (filename)
  "Parse FILENAME to extract category and title.
Returns a plist with :category and :title, or nil if not a valid format."
  (when (string-match "^\\([^_]+\\)__\\(.+\\)__system\\.org$" filename)
    (list :category (match-string 1 filename)
          :title (replace-regexp-in-string "-" " " (match-string 2 filename)))))

(defun ollama-buddy-user-prompts--refresh-cache ()
  "Refresh the cache of available user system prompts."
  (ollama-buddy-user-prompts--ensure-directory)
  (let ((files (directory-files ollama-buddy-user-prompts-directory nil "\\.org$")))
    (setq ollama-buddy-user-prompts--cache
          (cl-remove-if-not
           (lambda (parsed)
             (and parsed (plist-get parsed :category) (plist-get parsed :title)))
           (mapcar (lambda (file)
                     (let ((parsed (ollama-buddy-user-prompts--parse-filename file)))
                       (when parsed
                         (plist-put parsed :file file))))
                   files)))
    (setq ollama-buddy-user-prompts--last-refresh (current-time))))

(defun ollama-buddy-user-prompts--get-prompts ()
  "Get list of available user prompts with caching."
  (let ((current-time (float-time)))
    (when (or (null ollama-buddy-user-prompts--last-refresh)
              (> (- current-time (float-time ollama-buddy-user-prompts--last-refresh))
                 ollama-buddy-user-prompts--cache-ttl))
      (ollama-buddy-user-prompts--refresh-cache)))
  ollama-buddy-user-prompts--cache)

(defun ollama-buddy-user-prompts--format-for-completion (prompt-info)
  "Format PROMPT-INFO for completion display."
  (let ((category (plist-get prompt-info :category))
        (title (plist-get prompt-info :title)))
    (format "%s: %s"
            (propertize category 'face 'font-lock-type-face)
            (propertize title 'face 'font-lock-function-name-face))))

(defun ollama-buddy-user-prompts--read-prompt-content (file)
  "Read the content of a system prompt FILE."
  (let ((file-path (expand-file-name file ollama-buddy-user-prompts-directory)))
    (when (file-exists-p file-path)
      (with-temp-buffer
        (insert-file-contents file-path)
        (buffer-string)))))

;;;###autoload
(defun ollama-buddy-user-prompts-save ()
  "Save the current system prompt to a file."
  (interactive)
  (unless ollama-buddy--current-system-prompt
    (user-error "No system prompt is currently set"))
  
  (ollama-buddy-user-prompts--ensure-directory)
  
  (let* ((category (completing-read 
                    "Category: "
                    ollama-buddy-user-prompts-default-categories
                    nil nil nil nil "general"))
         (title (read-string "Title for this system prompt: "))
         (filename (ollama-buddy-user-prompts--format-filename category title))
         (filepath (expand-file-name filename ollama-buddy-user-prompts-directory)))
    
    ;; Check if file already exists
    (when (file-exists-p filepath)
      (unless (y-or-n-p (format "File '%s' already exists. Overwrite? " filename))
        (user-error "Save cancelled")))
    
    ;; Write the system prompt to file
    (with-temp-file filepath
      (insert "#+TITLE: " title "\n")
      (insert "#+CATEGORY: " category "\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
      (insert ollama-buddy--current-system-prompt))
    
    ;; Refresh cache
    (ollama-buddy-user-prompts--refresh-cache)
    
    (message "System prompt saved as '%s'" filename)
    (ollama-buddy--update-status (format "Saved prompt: %s" title))))

;;;###autoload
(defun ollama-buddy-user-prompts-load ()
  "Load a user system prompt and apply it to the current chat."
  (interactive)
  (let ((prompts (ollama-buddy-user-prompts--get-prompts)))
    (unless prompts
      (user-error "No user system prompts found. Create one with `ollama-buddy-user-prompts-save'"))
    
    (let* ((formatted-prompts (mapcar #'ollama-buddy-user-prompts--format-for-completion prompts))
           (prompt-alist (cl-mapcar #'cons formatted-prompts prompts))
           (selected-formatted (completing-read "Load system prompt: " formatted-prompts nil t))
           (selected-prompt (cdr (assoc selected-formatted prompt-alist)))
           (file (plist-get selected-prompt :file))
           (content (ollama-buddy-user-prompts--read-prompt-content file)))
      
      (when content
        ;; Extract just the content without org headers
        (setq content (with-temp-buffer
                        (insert content)
                        (goto-char (point-min))
                        ;; Skip org headers
                        (while (looking-at "^#\\+")
                          (forward-line 1))
                        ;; Skip any empty lines after headers
                        (while (looking-at "^$")
                          (forward-line 1))
                        (buffer-substring-no-properties (point) (point-max))))
        
        ;; Set the system prompt
        (setq ollama-buddy--current-system-prompt content)
        
        ;; Ensure chat buffer is ready
        (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
          (ollama-buddy--open-chat))
        
        (let ((title (plist-get selected-prompt :title)))
          (message "Loaded system prompt: %s" title)
          (ollama-buddy--update-status (format "Loaded prompt: %s" title)))))))

;;;###autoload
(defun ollama-buddy-user-prompts-list ()
  "Display a list of all saved user system prompts."
  (interactive)
  (let ((prompts (ollama-buddy-user-prompts--get-prompts))
        (buf (get-buffer-create "*User System Prompts*")))
    
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: User System Prompts\n\n")
        
        (if (null prompts)
            (insert "No user system prompts found.\n\n")
          (let ((categories (cl-remove-duplicates 
                             (mapcar (lambda (p) (plist-get p :category)) prompts)
                             :test #'string=)))
            
            ;; Group by category
            (dolist (category (sort categories #'string<))
              (insert (format "* %s\n\n" (capitalize category)))
              
              (let ((category-prompts (cl-remove-if-not
                                       (lambda (p) (string= (plist-get p :category) category))
                                       prompts)))
                (dolist (prompt (sort category-prompts 
                                      (lambda (a b) (string< (plist-get a :title) 
                                                             (plist-get b :title)))))
                  (let ((title (plist-get prompt :title))
                        (file (plist-get prompt :file)))
                    
                    ;; Create a clickable link to load the prompt
                    (insert (format "** [[elisp:(ollama-buddy-user-prompts-load-by-file \"%s\")][%s]]\n"
                                    file title))
                    
                    ;; Add edit and delete buttons
                    (insert "   ")
                    (insert (format "[[elisp:(ollama-buddy-user-prompts-edit \"%s\")][Edit]] " file))
                    (insert (format "[[elisp:(ollama-buddy-user-prompts-delete \"%s\")][Delete]]" file))
                    (insert "\n\n")))))))
        
        (view-mode 1)
        (goto-char (point-min)))
    (display-buffer buf))))

(defun ollama-buddy-user-prompts-load-by-file (file)
  "Load a system prompt by FILE name."
  (let ((prompts (ollama-buddy-user-prompts--get-prompts)))
    (when-let ((prompt (cl-find file prompts :test #'string= :key (lambda (p) (plist-get p :file)))))
      (let ((content (ollama-buddy-user-prompts--read-prompt-content file)))
        (when content
          ;; Extract content without org headers
          (setq content (with-temp-buffer
                          (insert content)
                          (goto-char (point-min))
                          (while (looking-at "^#\\+")
                            (forward-line 1))
                          (while (looking-at "^$")
                            (forward-line 1))
                          (buffer-substring-no-properties (point) (point-max))))
          
          ;; Set the system prompt
          (setq ollama-buddy--current-system-prompt content)
          
          ;; Ensure chat buffer is ready
          (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
            (ollama-buddy--open-chat))
          
          (let ((title (plist-get prompt :title)))
            (message "Loaded system prompt: %s" title)
            (ollama-buddy--update-status (format "Loaded prompt: %s" title))))))))

;;;###autoload
(defun ollama-buddy-user-prompts-edit (file)
  "Edit a user system prompt FILE."
  (interactive
   (let ((prompts (ollama-buddy-user-prompts--get-prompts)))
     (unless prompts
       (user-error "No user system prompts found"))
     (let* ((formatted-prompts (mapcar #'ollama-buddy-user-prompts--format-for-completion prompts))
            (prompt-alist (cl-mapcar #'cons formatted-prompts prompts))
            (selected-formatted (completing-read "Edit system prompt: " formatted-prompts nil t))
            (selected-prompt (cdr (assoc selected-formatted prompt-alist))))
       (list (plist-get selected-prompt :file)))))
  
  (let ((filepath (expand-file-name file ollama-buddy-user-prompts-directory)))
    (find-file filepath)))

;;;###autoload
(defun ollama-buddy-user-prompts-delete (file)
  "Delete a user system prompt FILE."
  (interactive
   (let ((prompts (ollama-buddy-user-prompts--get-prompts)))
     (unless prompts
       (user-error "No user system prompts found"))
     (let* ((formatted-prompts (mapcar #'ollama-buddy-user-prompts--format-for-completion prompts))
            (prompt-alist (cl-mapcar #'cons formatted-prompts prompts))
            (selected-formatted (completing-read "Delete system prompt: " formatted-prompts nil t))
            (selected-prompt (cdr (assoc selected-formatted prompt-alist))))
       (list (plist-get selected-prompt :file)))))
  
  (let* ((prompts (ollama-buddy-user-prompts--get-prompts))
         (prompt (cl-find file prompts :test #'string= :key (lambda (p) (plist-get p :file))))
         (title (when prompt (plist-get prompt :title)))
         (filepath (expand-file-name file ollama-buddy-user-prompts-directory)))
    
    (when (yes-or-no-p (format "Really delete system prompt '%s'? " (or title file)))
      (delete-file filepath)
      (ollama-buddy-user-prompts--refresh-cache)
      (message "Deleted system prompt: %s" (or title file))
      (ollama-buddy--update-status (format "Deleted prompt: %s" (or title file))))))

;;;###autoload
(defun ollama-buddy-user-prompts-create-new ()
  "Create a new system prompt from scratch."
  (interactive)
  (ollama-buddy-user-prompts--ensure-directory)
  
  (let* ((category (completing-read 
                    "Category: "
                    ollama-buddy-user-prompts-default-categories
                    nil nil nil nil "general"))
         (title (read-string "Title for this system prompt: "))
         (filename (ollama-buddy-user-prompts--format-filename category title))
         (filepath (expand-file-name filename ollama-buddy-user-prompts-directory)))
    
    ;; Check if file already exists
    (when (file-exists-p filepath)
      (unless (y-or-n-p (format "File '%s' already exists. Overwrite? " filename))
        (user-error "Creation cancelled")))
    
    ;; Create the file with template
    (with-temp-file filepath
      (insert "#+TITLE: " title "\n")
      (insert "#+CATEGORY: " category "\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
      (insert "# Your system prompt goes here\n")
      (insert "# This prompt will be used as a system message for the AI model\n\n"))
    
    ;; Open the file for editing
    (find-file filepath)
    
    ;; Refresh cache
    (ollama-buddy-user-prompts--refresh-cache)
    
    (message "Created new system prompt: %s" filename)))

;;;###autoload
(defun ollama-buddy-user-prompts-export (file output-file)
  "Export a user system prompt FILE to OUTPUT-FILE in plain text format."
  (interactive
   (let ((prompts (ollama-buddy-user-prompts--get-prompts)))
     (unless prompts
       (user-error "No user system prompts found"))
     (let* ((formatted-prompts (mapcar #'ollama-buddy-user-prompts--format-for-completion prompts))
            (prompt-alist (cl-mapcar #'cons formatted-prompts prompts))
            (selected-formatted (completing-read "Export system prompt: " formatted-prompts nil t))
            (selected-prompt (cdr (assoc selected-formatted prompt-alist)))
            (file (plist-get selected-prompt :file))
            (title (plist-get selected-prompt :title))
            (default-output (concat (ollama-buddy-user-prompts--sanitize-title title) ".txt"))
            (output-file (read-file-name "Export to: " nil default-output)))
       (list file output-file))))
  
  (let ((content (ollama-buddy-user-prompts--read-prompt-content file)))
    (when content
      ;; Extract content without org headers
      (setq content (with-temp-buffer
                      (insert content)
                      (goto-char (point-min))
                      (while (looking-at "^#\\+")
                        (forward-line 1))
                      (while (looking-at "^$")
                        (forward-line 1))
                      (buffer-substring-no-properties (point) (point-max))))
      
      (with-temp-file output-file
        (insert content))
      
      (message "Exported system prompt to: %s" output-file))))

;;;###autoload
(defun ollama-buddy-user-prompts-import (file)
  "Import a text FILE as a new system prompt."
  (interactive "fImport file: ")
  (unless (file-exists-p file)
    (user-error "File does not exist: %s" file))
  
  (ollama-buddy-user-prompts--ensure-directory)
  
  (let* ((content (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
         (category (completing-read 
                    "Category: "
                    ollama-buddy-user-prompts-default-categories
                    nil nil nil nil "general"))
         (default-title (file-name-base file))
         (title (read-string "Title for this system prompt: " default-title))
         (filename (ollama-buddy-user-prompts--format-filename category title))
         (filepath (expand-file-name filename ollama-buddy-user-prompts-directory)))
    
    ;; Check if file already exists
    (when (file-exists-p filepath)
      (unless (y-or-n-p (format "File '%s' already exists. Overwrite? " filename))
        (user-error "Import cancelled")))
    
    ;; Write the imported content with org headers
    (with-temp-file filepath
      (insert "#+TITLE: " title "\n")
      (insert "#+CATEGORY: " category "\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n")
      (insert "#+IMPORTED_FROM: " file "\n\n")
      (insert content))
    
    ;; Refresh cache
    (ollama-buddy-user-prompts--refresh-cache)
    
    (message "Imported system prompt as '%s'" filename)
    (ollama-buddy--update-status (format "Imported prompt: %s" title))))

(provide 'ollama-buddy-user-prompts)
;;; ollama-buddy-user-prompts.el ends here
