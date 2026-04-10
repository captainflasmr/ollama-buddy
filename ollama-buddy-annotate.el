;;; ollama-buddy-annotate.el --- Annotation generation for simply-annotate -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/captainflasmr/ollama-buddy
;;
;;; Commentary:
;;
;; Converts line-based annotation JSON (as produced by the Annotate Project
;; system prompt with structured output) into a simply-annotate database file.
;;
;; Workflow:
;;   1. Load the "Annotate Project" skill via /skill or /system
;;      (this sets the JSON schema format automatically)
;;   2. Attach or paste source code and send to the model
;;   3. Copy the JSON response
;;   4. M-x ollama-buddy-annotate-apply to convert and write the database
;;
;; The JSON format expected is:
;;   {"annotations": [{"sl": 1, "el": -1, "text": "...", "level": "file"}, ...]}
;;
;; Or for multi-file input:
;;   {"/path/to/file.el": [{"sl": 1, "el": -1, "text": "...", "level": "file"}, ...]}

;;; Code:

(require 'json)
(require 'cl-lib)

(defcustom ollama-buddy-annotate-db-strategy 'project
  "Where to write the annotation database.
`project' -- write to .simply-annotations.el at the project root
`global'  -- write to the global simply-annotate database"
  :type '(choice (const :tag "Project root" project)
                 (const :tag "Global database" global))
  :group 'ollama-buddy)

(defun ollama-buddy-annotate--project-root ()
  "Find the project root by looking for common markers."
  (let ((root (locate-dominating-file
               default-directory
               (lambda (dir)
                 (cl-some (lambda (f) (file-exists-p (expand-file-name f dir)))
                          '(".git" ".hg" ".svn" "package.json" "Cargo.toml"
                            "pyproject.toml" "go.mod" "Makefile" "Cask"))))))
    (or root default-directory)))

(defun ollama-buddy-annotate--db-path ()
  "Return the database path based on `ollama-buddy-annotate-db-strategy'."
  (pcase ollama-buddy-annotate-db-strategy
    ('project
     (expand-file-name ".simply-annotations.el"
                       (ollama-buddy-annotate--project-root)))
    ('global
     (expand-file-name "simply-annotations.el" user-emacs-directory))))

(defun ollama-buddy-annotate--line-to-point (buf line-number)
  "In BUF, return the point at the beginning of LINE-NUMBER (1-based)."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (if (and (> line-number 0)
               (zerop (forward-line (1- line-number))))
          (point)
        (point-max)))))

(defun ollama-buddy-annotate--end-of-line-region (buf line-number)
  "In BUF, return the point at the end of LINE-NUMBER."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (if (and (> line-number 0)
               (zerop (forward-line (1- line-number))))
          (line-end-position)
        (point-max)))))

(defun ollama-buddy-annotate--convert-file (filepath annotations)
  "Convert ANNOTATIONS for FILEPATH from line numbers to point positions.
Each annotation is an alist with keys sl, el, text, level."
  (if (not (file-exists-p filepath))
      (progn
        (message "WARNING: File not found, skipping: %s" filepath)
        nil)
    (let ((buf (generate-new-buffer " *annotate-temp*"))
          result)
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert-file-contents filepath))
            (dolist (ann annotations)
              (let* ((sl (alist-get 'sl ann))
                     (el (alist-get 'el ann))
                     (text (alist-get 'text ann))
                     (level (intern (alist-get 'level ann)))
                     (start (ollama-buddy-annotate--line-to-point buf sl))
                     (end (if (= el -1)
                              (with-current-buffer buf (point-max))
                            (ollama-buddy-annotate--end-of-line-region buf el))))
                (push `((start . ,start)
                        (end . ,end)
                        (text . ,text)
                        (level . ,level))
                      result))))
        (kill-buffer buf))
      (nreverse result))))

(defun ollama-buddy-annotate--load-existing (db-path)
  "Load existing database from DB-PATH if it exists."
  (when (file-exists-p db-path)
    (with-temp-buffer
      (insert-file-contents db-path)
      (let ((content (string-trim (buffer-string))))
        (unless (string-empty-p content)
          (condition-case err
              (car (read-from-string content))
            (error
             (message "WARNING: Could not parse existing database: %s"
                      (error-message-string err))
             nil)))))))

(defun ollama-buddy-annotate--write-db (db db-path)
  "Write annotation DB to DB-PATH in simply-annotate format."
  (let ((sorted (sort db (lambda (a b) (string< (car a) (car b))))))
    (with-temp-file db-path
      (insert ";;; Simply Annotate Database\n")
      (insert ";;; This file is auto-generated. Do not edit manually.\n\n")
      (let ((print-level nil)
            (print-length nil))
        (prin1 sorted (current-buffer)))
      (insert "\n"))))

(defun ollama-buddy-annotate--parse-json (json-string)
  "Parse JSON-STRING into the annotation alist format.
Handles both single-file format ({\"annotations\": [...]})
and multi-file format ({\"/path/file\": [...], ...})."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (data (json-read-from-string json-string)))
    (if (assq 'annotations data)
        ;; Single-file format: {"annotations": [...]}
        data
      ;; Multi-file format: {"/path/file": [...], ...}
      data)))

;;;###autoload
(defun ollama-buddy-annotate-apply (filepath json-string)
  "Apply annotation JSON-STRING for FILEPATH to the database.
FILEPATH is the absolute path to the source file that was annotated.
JSON-STRING is the model's structured JSON output."
  (interactive
   (let* ((file (read-file-name "Source file that was annotated: "
                                nil buffer-file-name t))
          (json (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "JSON (or paste): "))))
     (list (expand-file-name file) json)))
  (let* ((db-path (ollama-buddy-annotate--db-path))
         (existing-db (ollama-buddy-annotate--load-existing db-path))
         (parsed (ollama-buddy-annotate--parse-json json-string))
         (new-db (copy-sequence (or existing-db '())))
         (file-count 0))
    ;; Backup existing db
    (when (file-exists-p db-path)
      (copy-file db-path (concat db-path ".bak") t))
    (if (assq 'annotations parsed)
        ;; Single-file format
        (let ((converted (ollama-buddy-annotate--convert-file
                          filepath (alist-get 'annotations parsed))))
          (when converted
            (cl-incf file-count)
            (if (assoc filepath new-db #'string=)
                (setcdr (assoc filepath new-db #'string=) converted)
              (push (cons filepath converted) new-db))))
      ;; Multi-file format
      (dolist (entry parsed)
        (let* ((fpath (symbol-name (car entry)))
               (anns (cdr entry))
               (converted (ollama-buddy-annotate--convert-file fpath anns)))
          (when converted
            (cl-incf file-count)
            (if (assoc fpath new-db #'string=)
                (setcdr (assoc fpath new-db #'string=) converted)
              (push (cons fpath converted) new-db))))))
    (ollama-buddy-annotate--write-db new-db db-path)
    (message "Annotations applied: %d file(s) -> %s" file-count db-path)))

;;;###autoload
(defun ollama-buddy-annotate-apply-last-response ()
  "Apply annotations from the last Ollama Buddy response.
Prompts for the source file path, then extracts JSON from the
last response in the chat buffer and writes to the database."
  (interactive)
  (let* ((filepath (expand-file-name
                    (read-file-name "Source file that was annotated: "
                                   nil buffer-file-name t)))
         (json-string
          (with-current-buffer (get-buffer-create
                                (if (boundp 'ollama-buddy--chat-buffer)
                                    ollama-buddy--chat-buffer
                                  "*Ollama Buddy Chat*"))
            (save-excursion
              (goto-char (point-max))
              ;; Find the last JSON block in the response
              (if (re-search-backward "\\({[[:space:]\n]*\"annotations\"" nil t)
                  (let ((start (point)))
                    (forward-sexp)
                    (buffer-substring-no-properties start (point)))
                (user-error "No JSON annotation block found in last response"))))))
    (ollama-buddy-annotate-apply filepath json-string)))

;;;###autoload
(defun ollama-buddy-annotate-validate (db-path)
  "Validate the simply-annotate database at DB-PATH."
  (interactive
   (list (read-file-name "Database file: "
                         nil (ollama-buddy-annotate--db-path) t)))
  (let ((errors 0)
        (warnings 0)
        (total 0)
        db)
    (condition-case err
        (setq db (ollama-buddy-annotate--load-existing db-path))
      (error
       (message "FAIL: Cannot parse database: %s" (error-message-string err))
       (cl-return-from ollama-buddy-annotate-validate)))
    (unless (listp db)
      (message "FAIL: Database is not a list")
      (cl-return-from ollama-buddy-annotate-validate))
    (dolist (entry db)
      (let ((fpath (car entry))
            (annotations (cdr entry)))
        (unless (stringp fpath)
          (cl-incf errors))
        (unless (file-exists-p fpath)
          (cl-incf warnings))
        (dolist (ann annotations)
          (cl-incf total)
          (let ((start (alist-get 'start ann))
                (end (alist-get 'end ann))
                (text (alist-get 'text ann))
                (level (alist-get 'level ann)))
            (unless (and start end text level)
              (cl-incf errors))
            (when (and start end (integerp start) (integerp end) (> start end))
              (cl-incf errors))
            (when (and level (not (memq level '(file defun line))))
              (cl-incf errors))))))
    (message "Validation: %d files, %d annotations, %d errors, %d warnings — %s"
             (length db) total errors warnings
             (if (zerop errors) "OK" "FAIL"))))

(provide 'ollama-buddy-annotate)
;;; ollama-buddy-annotate.el ends here
