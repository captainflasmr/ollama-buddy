;;; ollama-buddy-project.el --- Project integration for Ollama Buddy -*- lexical-binding: t; -*-

;; Author: James Dyer
;; Keywords: local, tools, project
;; Package-Requires: ((emacs "29.1") (ollama-buddy-core "3.3.0"))

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
(declare-function ollama-buddy-update-mode-line "ollama-buddy")
(declare-function ollama-buddy-mode "ollama-buddy")
(declare-function ollama-buddy-rag-clear-attached "ollama-buddy-rag")

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
      (with-no-warnings (car (project-roots proj))))))

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
        (format "Project: [[file:%s][%s]] (%d files)" root (abbreviate-file-name root) (length files)))
    "Not in a project"))

(defcustom ollama-buddy-project-summary-file ".ollama-buddy-project.org"
  "File name for the cached project summary, relative to project root."
  :type 'string
  :group 'ollama-buddy-project)

(declare-function ollama-buddy--open-chat "ollama-buddy")
(declare-function ollama-buddy--send-backend "ollama-buddy-core")

(defvar-local ollama-buddy-project--pending-save-path nil
  "When non-nil, the path where the project summary should be saved.
Set by `ollama-buddy-project-init' when generating a new summary,
cleared after the save prompt.")

;;;###autoload
(defun ollama-buddy-project-init ()
  "Generate or load a project summary for the current project.
If a cached summary exists in the project root (see
`ollama-buddy-project-summary-file'), attach it as context.
Otherwise, gather the project structure and key files, send them
to the current model for summarisation, and save the result."
  (interactive)
  (unless (featurep 'ollama-buddy-project)
    (user-error "ollama-buddy-project module not loaded"))
  (let ((root (ollama-buddy-project-current-root)))
    (unless root
      (user-error "Not in a project"))
    (let ((summary-path (expand-file-name ollama-buddy-project-summary-file root)))
      (if (file-exists-p summary-path)
          ;; Load existing summary as attachment
          (progn
            (ollama-buddy-attach-file summary-path)
            (message "Project summary loaded from %s" (abbreviate-file-name summary-path)))
        ;; Generate new summary
        (ollama-buddy-project--generate-summary root summary-path)))))

(defun ollama-buddy-project--generate-summary (root summary-path)
  "Generate a project summary for ROOT and save to SUMMARY-PATH.
The summary streams into the chat buffer.  When the response
completes, the user is prompted to save the result."
  (let* ((proj (project-current))
         (all-files (project-files proj))
         ;; Build tree of top-level entries
         (top-entries (directory-files root nil "^[^.]"))
         (tree-lines
          (mapcar (lambda (f)
                    (if (file-directory-p (expand-file-name f root))
                        (format "  %s/" f)
                      (format "  %s" f)))
                  top-entries))
         ;; Collect content of key files (README, Makefile, etc.)
         (key-file-contents
          (let ((parts nil))
            (dolist (name ollama-buddy-project-standard-files)
              (let ((path (expand-file-name name root)))
                (when (file-exists-p path)
                  (condition-case nil
                      (let ((content (with-temp-buffer
                                       (insert-file-contents path nil nil 8192)
                                       (buffer-string))))
                        (push (format "### %s\n\n%s" name content) parts))
                    (error nil)))))
            (nreverse parts)))
         ;; Build the prompt
         (prompt
          (concat
           "Analyse the following project and produce a concise org-mode summary "
           "covering: project name and purpose, key technologies, directory structure, "
           "main entry points, and build/test commands. Use org headings. "
           "This summary will be reused as context in future AI sessions.\n\n"
           (format "Project root: %s\nTotal files: %d\n\n"
                   (abbreviate-file-name root) (length all-files))
           "## Directory structure\n\n"
           (mapconcat #'identity tree-lines "\n")
           "\n\n"
           (when key-file-contents
             (concat "## Key files\n\n"
                     (mapconcat #'identity key-file-contents "\n\n"))))))
    ;; Open chat and send (skip inline @file/@search/@rag processing
    ;; since the prompt embeds raw file contents that may contain those patterns)
    (ollama-buddy--open-chat)
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (let ((inhibit-read-only t))
        (setq ollama-buddy-project--pending-save-path summary-path)
        (goto-char (point-max))
        (insert prompt)))
    (let ((ollama-buddy--skip-inline-processing t))
      (ollama-buddy--send-backend prompt))))

(defun ollama-buddy-project--maybe-save-summary ()
  "Save the project summary if a generation is pending.
Called automatically from the stream sentinel after a response
completes.  Checks `ollama-buddy-project--pending-save-path'."
  (when-let ((summary-path ollama-buddy-project--pending-save-path))
    (setq ollama-buddy-project--pending-save-path nil)
    (let* ((model (or ollama-buddy--current-model ollama-buddy-default-model))
           (history (gethash model ollama-buddy--conversation-history-by-model nil))
           (last-assistant
            (cl-find-if (lambda (msg) (equal (alist-get 'role msg) "assistant"))
                        (reverse history))))
      (when last-assistant
        (let ((content (alist-get 'content last-assistant)))
          (when (and content (not (string-empty-p content)))
            (with-temp-file summary-path
              (insert content))
            (ollama-buddy-attach-file summary-path)
            (message "Project summary saved and attached from %s"
                     (abbreviate-file-name summary-path))))))))

;;;###autoload
(defun ollama-buddy-project-auto-load-summary ()
  "Attach the cached project summary if it exists.
Called during chat buffer initialization to automatically provide
project context."
  (when-let ((root (ollama-buddy-project-current-root)))
    (let ((summary-path (expand-file-name ollama-buddy-project-summary-file root)))
      (when (file-exists-p summary-path)
        (ollama-buddy-attach-file summary-path)
        (message "Project summary loaded from %s"
                 (abbreviate-file-name summary-path))))))

;;;###autoload
(defun ollama-buddy-project-switch-directory ()
  "Switch the chat buffer's working directory and load project context.
Prompts for a new directory, updates `default-directory', clears
existing file attachments, and auto-loads the new project summary
if one exists.  Re-displays the welcome screen showing the new
project.  Conversation history is preserved."
  (interactive)
  (let ((new-dir (read-directory-name "Switch to directory: " nil nil t)))
    (unless (file-directory-p new-dir)
      (user-error "Not a directory: %s" new-dir))
    (setq new-dir (file-name-as-directory (expand-file-name new-dir)))
    ;; Switch to the chat buffer so all buffer-local state is visible
    (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
    (setq default-directory new-dir)
    ;; Clear file attachments (silently, no confirmation)
    (setq ollama-buddy--current-attachments nil)
    (when (boundp 'ollama-buddy-web-search--current-results)
      (setq ollama-buddy-web-search--current-results nil))
    (when (featurep 'ollama-buddy-rag)
      (ollama-buddy-rag-clear-attached))
    ;; Auto-load project summary if available in new directory
    (ollama-buddy-project-auto-load-summary)
    ;; Re-display welcome screen so the user sees the new project
    (let ((inhibit-read-only t))
      (erase-buffer)
      (ollama-buddy-mode 1)
      (insert (ollama-buddy--create-intro-message))
      (save-excursion
        (when (re-search-backward "^\\*\\* More Commands$" nil t)
          (org-fold-hide-subtree)))
      (ollama-buddy--prepare-prompt-area))
    (goto-char (point-max))
    (ollama-buddy-update-mode-line)
    (ollama-buddy--update-status
     (format "Switched to %s" (abbreviate-file-name new-dir)))))

(provide 'ollama-buddy-project)
;;; ollama-buddy-project.el ends here
