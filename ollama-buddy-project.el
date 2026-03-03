;;; ollama-buddy-project.el --- Project integration for Ollama Buddy -*- lexical-binding: t; -*-

;; Author: James Dyer
;; Keywords: local, tools, project
;; Package-Requires: ((emacs "28.1") (ollama-buddy-core "3.3.0"))

;;; Commentary:
;; This module provides project-aware features for Ollama Buddy using the
;; built-in `project.el` library. It allows users to quickly attach
;; project-specific files and context to their conversations.

;;; Code:

(require 'project)
(require 'cl-lib)
(require 'ollama-buddy-core)

;; Forward declarations
(declare-function ollama-buddy-attach-file "ollama-buddy")

(defgroup ollama-buddy-project nil
  "Project integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-project-")

(defcustom ollama-buddy-project-standard-files
  '("README" "README.md" "README.org" "TODO" "TODO.md" "TODO.org" 
    "CHANGELOG" "CHANGELOG.md" "CHANGELOG.org" "LICENSE" "INSTALL" 
    "CONTRIBUTING" "package.json" "Makefile" "CMakeLists.txt")
  "List of standard project files to look for when attaching context."
  :type '(repeat string)
  :group 'ollama-buddy-project)

(defun ollama-buddy-project-current-root ()
  "Return the root of the current project, or nil."
  (when-let ((proj (project-current)))
    (if (fboundp 'project-root)
        (project-root proj)
      (car (project-roots proj)))))

;;;###autoload
(defun ollama-buddy-project-attach-file ()
  "Attach a file from the current project using completion."
  (interactive)
  (if-let ((root (ollama-buddy-project-current-root)))
      (let* ((proj (project-current))
             (files (project-files proj))
             (relative-files (mapcar (lambda (f) (file-relative-name f root)) files))
             (choice (completing-read "Attach project file: " relative-files nil t)))
        (when choice
          (ollama-buddy-attach-file (expand-file-name choice root))))
    (message "Not in a project")))

;;;###autoload
(defun ollama-buddy-project-attach-context ()
  "Automatically attach standard project meta-files if they exist.
Looks for files defined in `ollama-buddy-project-standard-files' within
the current project root."
  (interactive)
  (if-let ((root (ollama-buddy-project-current-root)))
      (let ((attached-count 0))
        (dolist (name ollama-buddy-project-standard-files)
          (let ((path (expand-file-name name root)))
            (when (file-exists-p path)
              (condition-case nil
                  (progn
                    (ollama-buddy-attach-file path)
                    (setq attached-count (1+ attached-count)))
                (error nil)))))
        (if (> attached-count 0)
            (message "Attached %d project context files from %s" attached-count (abbreviate-file-name root))
          (message "No standard project context files found in %s" (abbreviate-file-name root))))
    (message "Not in a project")))

;;;###autoload
(defun ollama-buddy-project-info ()
  "Show information about the current project in the echo area."
  (interactive)
  (if-let ((root (ollama-buddy-project-current-root)))
      (let* ((proj (project-current))
             (files (project-files proj)))
        (message "Project Root: %s (%d files indexed)" (abbreviate-file-name root) (length files)))
    (message "Not in a project")))

;;;###autoload
(defun ollama-buddy-project-get-status-string ()
  "Return a concise project status string for the welcome screen."
  (if-let ((root (ollama-buddy-project-current-root)))
      (let* ((proj (project-current))
             (files (project-files proj)))
        (format "Project: %s (%d files)" (abbreviate-file-name root) (length files)))
    "Not in a project"))

(provide 'ollama-buddy-project)
;;; ollama-buddy-project.el ends here
