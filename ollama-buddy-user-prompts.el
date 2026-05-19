;;; ollama-buddy-user-prompts.el --- User system prompts for ollama-buddy -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Package-Requires: ((emacs "29.1"))
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
  '("general" "coding" "writing" "analysis" "creative" "technical" "documentation" "emacs" "skills")
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
  "Ensure the user prompt directory exists."
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
  "Refresh the cache of available user system prompt."
  (ollama-buddy-user-prompts--ensure-directory)
  (let ((files (directory-files ollama-buddy-user-prompts-directory nil "\\.org$")))
    (setq ollama-buddy-user-prompts--cache
          (cl-remove-if-not
           (lambda (parsed)
             (and parsed (plist-get parsed :category) (plist-get parsed :title)))
           (mapcar (lambda (file)
                     (let ((parsed (ollama-buddy-user-prompts--parse-filename file)))
                       (when parsed
                         (plist-put parsed :file file)
                         ;; Extract description/first line from content
                         (let* ((raw (ollama-buddy-user-prompts--read-prompt-content file))
                                (desc (when raw
                                        (let* ((headers-stripped (ollama-buddy-user-prompts--strip-org-headers raw))
                                               (first-line (car (split-string headers-stripped "\n" t))))
                                          (if (and first-line (> (length first-line) 60))
                                              (concat (substring first-line 0 57) "...")
                                            first-line)))))
                           (plist-put parsed :description desc)))
                       parsed))
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
        (title (plist-get prompt-info :title))
        (description (plist-get prompt-info :description)))
    (concat
     (format "%s: %s"
             (propertize category 'face 'font-lock-type-face)
             (propertize title 'face 'font-lock-function-name-face))
     (if (and description (not (string-empty-p description)))
         (concat " " (propertize (format "-- %s" description) 'face 'shadow))
       ""))))

(defun ollama-buddy-user-prompts--read-prompt-content (file)
  "Read the content of a system prompt FILE."
  (let ((file-path (expand-file-name file ollama-buddy-user-prompts-directory)))
    (when (file-exists-p file-path)
      (with-temp-buffer
        (insert-file-contents file-path)
        (buffer-string)))))

(defun ollama-buddy-user-prompts--strip-org-headers (content)
  "Strip org file headers from CONTENT, returning the prompt text."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (while (and (not (eobp)) (looking-at "^#\\+"))
      (forward-line 1))
    (while (and (not (eobp)) (looking-at "^$"))
      (forward-line 1))
    (if (eobp) "" (string-trim (buffer-substring-no-properties (point) (point-max))))))

(defun ollama-buddy-user-prompts--extract-format (content)
  "Extract the #+FORMAT: header value from CONTENT.
Returns the parsed JSON value (string or alist), or nil if not present."
  (when (string-match "^#\\+FORMAT:\\s-*\\(.+\\)" content)
    (let ((value (string-trim (match-string 1 content))))
      (if (string= value "json")
          "json"
        (condition-case nil
            (json-read-from-string value)
          (error
           (message "Warning: could not parse #+FORMAT: value as JSON")
           nil))))))

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
      (unless (y-or-n-p (format "File '%s' already exists.  Overwrite? " filename))
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
      (user-error "No user system prompts found.  Use C-c O → I to install extras, or create one with `ollama-buddy-user-prompts-save'"))
    
    (let* ((formatted-prompts (mapcar #'ollama-buddy-user-prompts--format-for-completion prompts))
           (prompt-alist (cl-mapcar #'cons formatted-prompts prompts))
           (selected-formatted (completing-read "Load system prompt: " formatted-prompts nil t))
           (selected-prompt (cdr (assoc selected-formatted prompt-alist)))
           (file (plist-get selected-prompt :file))
           (title (plist-get selected-prompt :title))
           (content (ollama-buddy-user-prompts--read-prompt-content file)))
      
      (when content
        ;; Extract format header before stripping
        (let ((format-spec (ollama-buddy-user-prompts--extract-format content)))
          ;; Extract just the content without org headers
          (setq content (ollama-buddy-user-prompts--strip-org-headers content))

          ;; Check if we actually have content after stripping headers
          (if (string-empty-p content)
              (progn
                (message "Warning: User prompt '%s' contains no content after headers" title)
                (when (yes-or-no-p "The selected prompt file appears to be empty. Continue anyway? ")
                  ;; Set empty system prompt to effectively clear it
                  (setq ollama-buddy--current-system-prompt nil
                        ollama-buddy--current-system-prompt-title nil
                        ollama-buddy--current-system-prompt-source nil)
                  (ollama-buddy--update-status "Empty prompt loaded (system prompt cleared)")
                  (message "System prompt cleared due to empty file")))

            ;; We have actual content, proceed normally
            (ollama-buddy--set-system-prompt-with-metadata content title "user")

            ;; Apply response format if specified
            (if format-spec
                (progn
                  (setq ollama-buddy--response-format format-spec)
                  (message "Loaded user prompt: %s (format: %s)" title
                           (if (stringp format-spec) format-spec "schema")))
              ;; Clear format when loading a prompt without #+FORMAT:
              (when ollama-buddy--response-format
                (setq ollama-buddy--response-format nil)
                (message "Loaded user prompt: %s (format cleared)" title))
              (message "Loaded user prompt: %s" title))

            ;; Ensure chat buffer is ready
            (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
              (ollama-buddy--open-chat))))))))

(defvar ollama-buddy-user-prompts-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ollama-buddy-user-prompts-set-at-point)
    map)
  "Keymap for `ollama-buddy-user-prompts-list-mode'.")

(define-minor-mode ollama-buddy-user-prompts-list-mode
  "Minor mode for the *User System Prompts* browse buffer.
\\{ollama-buddy-user-prompts-list-mode-map}"
  :lighter nil
  :keymap ollama-buddy-user-prompts-list-mode-map
  (when ollama-buddy-user-prompts-list-mode
    (setq-local minor-mode-overriding-map-alist
                (cons (cons 'ollama-buddy-user-prompts-list-mode
                            ollama-buddy-user-prompts-list-mode-map)
                      minor-mode-overriding-map-alist))))

;;;###autoload
(defun ollama-buddy-user-prompts-set-at-point ()
  "Set the system prompt heading at point as the current system prompt."
  (interactive)
  (let ((prompts (ollama-buddy-user-prompts--get-prompts)))
    (unless prompts
      (user-error "No user system prompts available"))
    (save-excursion
      (condition-case nil
          (org-back-to-heading t)
        (error (user-error "Point is not under a heading")))
      (when (= (org-outline-level) 1)
        (user-error "Point is on a category heading — move to a prompt heading"))
      (let* ((title (org-get-heading t t t t))
             (prompt (cl-find title prompts :test #'string= :key (lambda (p) (plist-get p :title)))))
        (unless prompt
          (user-error "Could not find prompt matching heading '%s'" title))
        (let* ((file (plist-get prompt :file))
               (raw (ollama-buddy-user-prompts--read-prompt-content file))
               (content (ollama-buddy-user-prompts--strip-org-headers raw)))
          (if (string-empty-p content)
              (user-error "Prompt file '%s' has no content" file)
            (ollama-buddy--set-system-prompt-with-metadata content title "user")
            (message "System prompt set: %s" title)))))))

;;;###autoload
(defun ollama-buddy-user-prompts-list ()
  "Display a list of all saved user system prompt."
  (interactive)
  (let ((prompts (ollama-buddy-user-prompts--get-prompts))
        (buf (get-buffer-create "*User System Prompts*")))
    
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (setq-local org-hide-emphasis-markers t)
        (setq-local org-hide-leading-stars t)

        (insert "#+TITLE: User System Prompts\n")
        (insert "# RET on a prompt heading to set as current system prompt\n\n")
        
        (if (null prompts)
            (insert "No user system prompts found.\n\nUse C-c O → I to install extras, or create one with `ollama-buddy-user-prompts-save'.\n\n")
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
                  (let* ((title (plist-get prompt :title))
                        (file (plist-get prompt :file))
                        (content (ollama-buddy-user-prompts--read-prompt-content file)))
                    (insert (format "** %s\n\n" title))
                    (insert (concat content "\n\n"))))))))
        (goto-char (point-min))
        (view-mode 1)
        (ollama-buddy-user-prompts-list-mode 1)
        (org-content))
    (display-buffer buf))))

;;;###autoload
(defun ollama-buddy-user-prompts-edit (file)
  "Edit a user system prompt FILE."
  (interactive
   (let ((prompts (ollama-buddy-user-prompts--get-prompts)))
     (unless prompts
       (user-error "No user system prompts found.  Use C-c O → I to install extras"))
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
       (user-error "No user system prompts found.  Use C-c O → I to install extras"))
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
      (unless (y-or-n-p (format "File '%s' already exists.  Overwrite? " filename))
        (user-error "Creation cancelled")))
    
    ;; Create the file with template
    (with-temp-file filepath
      (insert "#+TITLE: " title "\n")
      (insert "#+CATEGORY: " category "\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n"))
    
    ;; Open the file for editing
    (find-file filepath)
    (goto-char (point-max))
    
    ;; Refresh cache
    (ollama-buddy-user-prompts--refresh-cache)

    (message "Created new system prompt: %s" filename)))

(defcustom ollama-buddy-skills-export-directory
  (expand-file-name "~/.claude/skills/")
  "Directory where exported SKILL.md files are written.
These are read by Claude Code, opencode, and compatible agents."
  :type 'directory
  :group 'ollama-buddy-user-prompts)

(defcustom ollama-buddy-skills-export-agents-directory
  nil
  "Secondary directory for skill exports, e.g. a project `.agents/skills/`.
When non-nil, skills are also written to this location."
  :type '(choice (const nil) directory)
  :group 'ollama-buddy-user-prompts)

(defun ollama-buddy-user-prompts--title-to-skill-name (title &optional category)
  "Convert TITLE to kebab-case skill name.
When CATEGORY is given and not \"general\" or \"skills\",
prepend it as a namespace prefix to avoid collisions."
  (let ((name (downcase
               (replace-regexp-in-string
                "-\\{2,\\}" "-"
                (replace-regexp-in-string
                 "[^a-z0-9-]" ""
                 (replace-regexp-in-string
                  "[[:space:]]+" "-"
                  (downcase title)))))))
    (if (and category (not (member category '("general" "skills")))
             (not (string-prefix-p (concat category "-") name)))
        (format "%s-%s" category name)
      name)))

(defun ollama-buddy-user-prompts--org-to-md (text)
  "Convert org =code= and *bold* markup in TEXT to markdown."
  (let ((result text))
    (setq result (replace-regexp-in-string "=\\([^=]+\\)=" "`\\1`" result))
    (setq result (replace-regexp-in-string "\\*\\([^*\n]+\\)\\*" "**\\1**" result))
    result))

(defun ollama-buddy-user-prompts--generate-trigger-phrases (title category body)
  "Generate trigger phrases from TITLE, CATEGORY, and first line of BODY."
  (let* ((title-lower (downcase title))
         (words (split-string title-lower))
         (first-line (car (split-string body "\n" t)))
         (first-action
          (when first-line
            (let ((cleaned (replace-regexp-in-string
                            "^[0-9. ]+\\|^[-*]+ " "" first-line)))
              (when cleaned
                (mapconcat #'identity
                           (cl-subseq (split-string cleaned nil t)
                                      0 (min 3 (length (split-string cleaned nil t))))
                           " ")))))
         (phrases
          (delq nil
                (list
                 title-lower
                 (when first-action (downcase first-action))
                 (when (and category (not (member category '("general" "skills"))))
                   (format "%s %s %s" category (car words) (or (cadr words) "")))
                 (when (and (car words) (cadr words))
                   (format "%s %s" (car words) (cadr words)))
                 (format "%s %s" title-lower "help")))))
    (string-join (cl-subseq phrases 0 (min 5 (length phrases))) "\", \"")))

(defun ollama-buddy-user-prompts--skill-frontmatter (name description trigger-phrases)
  "Generate SKILL.md YAML frontmatter."
  (format "---
name: %s
description: >
  %s
  Trigger on phrases like \"%s\" .
---
" name description trigger-phrases))

;;;###autoload
(defun ollama-buddy-user-prompts--write-skill-file (name frontmatter content target-dir)
  "Write a SKILL.md file for skill NAME in TARGET-DIR.
FRONTMATTER is the YAML string, CONTENT is the markdown body."
  (let ((skill-dir (expand-file-name name target-dir))
        (file (expand-file-name (format "%s/SKILL.md" name) target-dir)))
    (unless (file-directory-p skill-dir)
      (make-directory skill-dir t))
    (with-temp-file file
      (insert frontmatter)
      (insert content))
    file))

;;;###autoload
(defun ollama-buddy-user-prompts-export-skills (&optional claude-dir agents-dir no-confirm)
  "Export all user prompts as SKILL.md skills.

Prompts are read from `ollama-buddy-user-prompts-directory',
converted to the SKILL.md format used by Claude Code, opencode,
and compatible AI agent tools.

Each skill is written to CLAUDE-DIR (default
`ollama-buddy-skills-export-directory').  When AGENTS-DIR is
given (or `ollama-buddy-skills-export-agents-directory' is set),
skills are also written there.

When NO-CONFIRM is non-nil (or called with a prefix argument),
skip the confirmation prompt.

Interactively, with \\[universal-argument], also prompt for both
target directories."
  (interactive
   (let* ((claude (if current-prefix-arg
                      (read-directory-name "Claude skills dir: "
                                           ollama-buddy-skills-export-directory)
                    ollama-buddy-skills-export-directory))
          (agents (when (or current-prefix-arg
                            ollama-buddy-skills-export-agents-directory)
                    (if current-prefix-arg
                        (read-directory-name "Agents skills dir: "
                                             (or ollama-buddy-skills-export-agents-directory
                                                 default-directory))
                      ollama-buddy-skills-export-agents-directory)))
          (no-confirm current-prefix-arg))
     (list claude agents no-confirm)))
  (let* ((claude-dir (or claude-dir ollama-buddy-skills-export-directory))
         (agents-dir (or agents-dir ollama-buddy-skills-export-agents-directory))
         (prompts (ollama-buddy-user-prompts--get-prompts))
         (targets (delq nil (list claude-dir agents-dir)))
         (count 0)
         (errors 0))
    (unless prompts
      (user-error "No user prompts found to export"))
    (unless targets
      (user-error "No target directories specified"))
    (unless no-confirm
      (unless (y-or-n-p
               (format "Export %d prompts as skills to %s? "
                       (length prompts)
                       (mapconcat (lambda (d) (abbreviate-file-name d)) targets " and ")))
        (user-error "Export cancelled")))
    (dolist (prompt prompts)
      (condition-case err
          (let* ((title (plist-get prompt :title))
                 (category (plist-get prompt :category))
                 (file (plist-get prompt :file))
                 (raw (ollama-buddy-user-prompts--read-prompt-content file))
                 (body (ollama-buddy-user-prompts--strip-org-headers raw))
                  (name (ollama-buddy-user-prompts--title-to-skill-name title category))
                  (desc (car (split-string body "\n" t)))
                  (triggers (ollama-buddy-user-prompts--generate-trigger-phrases
                             title category body))
                 (content (ollama-buddy-user-prompts--org-to-md body))
                 (frontmatter (ollama-buddy-user-prompts--skill-frontmatter
                               name desc triggers)))
            (dolist (target targets)
              (ollama-buddy-user-prompts--write-skill-file
               name frontmatter (format "# %s\n\n%s" title content) target))
            (cl-incf count))
        (error
         (message "Failed to export '%s': %s"
                  (or (plist-get prompt :title) (plist-get prompt :file))
                  (error-message-string err))
         (cl-incf errors))))
    (message "Exported %d skills to %s%s"
             count
             (mapconcat (lambda (d) (abbreviate-file-name d)) targets " and ")
             (if (> errors 0)
                 (format " (%d errors)" errors)
               ""))))

;;;###autoload
(defun ollama-buddy-user-prompts-export-skill (prompt-info &optional claude-dir agents-dir)
  "Export a single prompt PROMPT-INFO as a SKILL.md skill.
See `ollama-buddy-user-prompts-export-skills' for target dir behavior."
  (interactive
   (let ((prompts (ollama-buddy-user-prompts--get-prompts)))
     (unless prompts
       (user-error "No user prompts found"))
     (let* ((formatted (mapcar #'ollama-buddy-user-prompts--format-for-completion prompts))
            (alist (cl-mapcar #'cons formatted prompts))
            (selected (completing-read "Export prompt as skill: " formatted nil t)))
       (list (cdr (assoc selected alist))
             ollama-buddy-skills-export-directory
             ollama-buddy-skills-export-agents-directory))))
  (let* ((title (plist-get prompt-info :title))
         (file (plist-get prompt-info :file))
         (category (plist-get prompt-info :category))
         (raw (ollama-buddy-user-prompts--read-prompt-content file))
         (body (ollama-buddy-user-prompts--strip-org-headers raw))
         (name (ollama-buddy-user-prompts--title-to-skill-name title category))
         (desc (car (split-string body "\n" t)))
         (triggers (ollama-buddy-user-prompts--generate-trigger-phrases
                    title category body))
         (content (ollama-buddy-user-prompts--org-to-md body))
         (frontmatter (ollama-buddy-user-prompts--skill-frontmatter
                       name desc triggers))
         (targets (delq nil (list claude-dir agents-dir))))
    (unless targets
      (user-error "No target directories specified"))
    (dolist (target targets)
      (ollama-buddy-user-prompts--write-skill-file
       name frontmatter (format "# %s\n\n%s" title content) target))
    (message "Exported '%s' as skill '%s'" title name)))

(provide 'ollama-buddy-user-prompts)
;;; ollama-buddy-user-prompts.el ends here
