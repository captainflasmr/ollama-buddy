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

(defun ollama-buddy-annotate--make-context (buf start end)
  "Return up to 150 chars of BUF between START and END for relocation."
  (with-current-buffer buf
    (let ((text (buffer-substring-no-properties start (min end (point-max)))))
      (if (> (length text) 150)
          (substring text 0 150)
        text))))

(defun ollama-buddy-annotate--thread-id ()
  "Generate a simply-annotate thread id."
  (format "thread-%s-%06d" (format-time-string "%s") (random 1000000)))

(defun ollama-buddy-annotate--comment-id ()
  "Generate a simply-annotate comment id."
  (format "c-%s-%06d" (format-time-string "%s") (random 1000000)))

(defun ollama-buddy-annotate--make-thread (text tag)
  "Return a simply-annotate thread alist for TEXT tagged with TAG.
TAG is a string like \"file\", \"defun\", or \"line\"."
  (let ((ts (format-time-string "%Y-%m-%dT%H:%M:%S")))
    `((id . ,(ollama-buddy-annotate--thread-id))
      (created . ,ts)
      (status . "open")
      (priority . "normal")
      (tags . ,(if (and tag (not (string-empty-p tag)))
                   (list tag)
                 '()))
      (comments . (((id . ,(ollama-buddy-annotate--comment-id))
                    (parent-id . nil)
                    (author . "ollama-buddy")
                    (timestamp . ,ts)
                    (text . ,text)
                    (type . "comment")))))))

(defun ollama-buddy-annotate--convert-file (filepath annotations)
  "Convert ANNOTATIONS for FILEPATH into the simply-annotate on-disk format.
Each input annotation is an alist with keys sl, el, text, and (new) tag
or (legacy) level.  Output entries are alists with start, end, text,
text-hash, and text-context — matching `simply-annotate--serialize-annotations'."
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
                     (raw-text (or (alist-get 'text ann) ""))
                     (tag (or (alist-get 'tag ann)
                              (alist-get 'level ann)))
                     (thread (ollama-buddy-annotate--make-thread raw-text tag))
                     (start (ollama-buddy-annotate--line-to-point buf sl))
                     (end (if (= el -1)
                              (with-current-buffer buf (point-max))
                            (ollama-buddy-annotate--end-of-line-region buf el))))
                (when (and start end (> end start))
                  (let* ((region (with-current-buffer buf
                                   (buffer-substring-no-properties start end)))
                         (text-hash (sxhash-equal region))
                         (context (ollama-buddy-annotate--make-context
                                   buf start end)))
                    (push `((start . ,start)
                            (end . ,end)
                            (text . ,thread)
                            (text-hash . ,text-hash)
                            (text-context . ,context))
                          result))))))
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
         (project-root (ollama-buddy-annotate--project-root))
         (existing-db (ollama-buddy-annotate--load-existing db-path))
         (parsed (ollama-buddy-annotate--parse-json json-string))
         (new-db (copy-sequence (or existing-db '())))
         (file-count 0))
    ;; Backup existing db
    (when (file-exists-p db-path)
      (copy-file db-path (concat db-path ".bak") t))
    (cl-labels
        ((db-key (abs-path)
           (if (eq ollama-buddy-annotate-db-strategy 'project)
               (file-relative-name abs-path project-root)
             abs-path))
         (store (abs-path converted)
           (let ((key (db-key abs-path)))
             (if (assoc key new-db #'string=)
                 (setcdr (assoc key new-db #'string=) converted)
               (push (cons key converted) new-db)))))
      (if (assq 'annotations parsed)
          ;; Single-file format
          (let ((converted (ollama-buddy-annotate--convert-file
                            filepath (alist-get 'annotations parsed))))
            (when converted
              (cl-incf file-count)
              (store filepath converted)))
        ;; Multi-file format
        (dolist (entry parsed)
          (let* ((fpath (expand-file-name (symbol-name (car entry))))
                 (anns (cdr entry))
                 (converted (ollama-buddy-annotate--convert-file fpath anns)))
            (when converted
              (cl-incf file-count)
              (store fpath converted))))))
    (ollama-buddy-annotate--write-db new-db db-path)
    (message "Annotations applied: %d file(s) -> %s" file-count db-path)))

(defun ollama-buddy-annotate--parse-json-prefix (text)
  "Return the longest parseable JSON prefix of TEXT that is an annotations object.
Rejects parses that yield a non-object or an object without an
`annotations' key, so a bare string like `\"annotations\"' is not
mistaken for a successful parse."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (condition-case nil
        (let* ((result (json-parse-buffer :object-type 'alist
                                          :array-type 'list))
               (end-pos (point)))
          (when (and (listp result)
                     (alist-get 'annotations result))
            (buffer-substring-no-properties (point-min) end-pos)))
      (error nil))))

(defun ollama-buddy-annotate--extract-last-json ()
  "Return the JSON string from the last annotation response in the chat buffer.
`ollama-buddy-convert-markdown-to-org' typically strips the leading `{'
line from the response, so we always try parsing the raw region first
then retry with a synthesized outer `{...}' wrapper."
  (let ((buf (get-buffer (if (boundp 'ollama-buddy--chat-buffer)
                             ollama-buddy--chat-buffer
                           "*Ollama Buddy Chat*"))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-max))
          (when (re-search-backward "\"annotations\"[[:space:]]*:[[:space:]]*\\[" nil t)
            (let* ((start (line-beginning-position))
                   (section-end (save-excursion
                                  (goto-char start)
                                  (forward-line 1)
                                  (if (re-search-forward "^\\*+ " nil t)
                                      (match-beginning 0)
                                    (point-max))))
                   (raw (buffer-substring-no-properties start section-end)))
              (or (ollama-buddy-annotate--parse-json-prefix raw)
                  (ollama-buddy-annotate--parse-json-prefix
                   (concat "{" raw "\n}"))))))))))

;;;###autoload
(defun ollama-buddy-annotate-apply-last-response (&optional filepath)
  "Apply annotations from the last Ollama Buddy response.
With no FILEPATH, prompt for the source file path.  Extracts JSON from
the last response in the chat buffer and writes to the database."
  (interactive)
  (let* ((filepath (expand-file-name
                    (or filepath
                        (read-file-name "Source file that was annotated: "
                                        nil buffer-file-name t))))
         (json-string (ollama-buddy-annotate--extract-last-json)))
    (unless json-string
      (user-error "No JSON annotation block found in last response"))
    (ollama-buddy-annotate-apply filepath json-string)))

;; Directory-based batch annotation

(defcustom ollama-buddy-annotate-directory-extensions
  '("el" "py" "js" "ts" "tsx" "jsx" "rs" "go" "c" "h" "cpp" "hpp" "cc" "hh"
    "java" "kt" "scala" "rb" "sh" "bash" "zsh" "fish" "lua" "pl" "pm"
    "php" "swift" "m" "mm" "dart" "ex" "exs" "erl" "hrl" "clj" "cljs"
    "hs" "ml" "mli" "fs" "fsx" "vb" "cs" "r" "jl" "nim" "zig" "d"
    "adb" "ads" "ada" "f" "f90" "f95" "pas" "pp" "tcl" "sql")
  "File extensions considered by `ollama-buddy-annotate-directory'."
  :type '(repeat string)
  :group 'ollama-buddy)

(defcustom ollama-buddy-annotate-context-reserve 0.5
  "Fraction of the model context window reserved for the response and overhead.
A file is skipped when its estimated token cost exceeds
\(1 - this) of the context window.  Token cost is estimated as
file-size-in-characters / 4."
  :type 'float
  :group 'ollama-buddy)

(defcustom ollama-buddy-annotate-directory-skill
  "skills__Annotate-Project__system.org"
  "Filename of the user prompt skill used for directory annotation.
Looked up inside `ollama-buddy-user-prompts-directory'."
  :type 'string
  :group 'ollama-buddy)

(defvar ollama-buddy-annotate--queue nil
  "Remaining files queued for annotation by `ollama-buddy-annotate-directory'.")

(defvar ollama-buddy-annotate--queue-total 0
  "Total number of files originally queued in the current batch.")

(defvar ollama-buddy-annotate--current-file nil
  "File currently being annotated (waiting for the model response).")

(defvar ollama-buddy-annotate--queue-model nil
  "Model used for the current directory annotation batch.")

(declare-function ollama-buddy-attach-file "ollama-buddy" (file))
(declare-function ollama-buddy-clear-attachments "ollama-buddy" ())
(declare-function ollama-buddy--send "ollama-buddy" (&optional prompt specified-model tool-continuation-p))
(declare-function ollama-buddy--get-model-context-size "ollama-buddy-core" (model))
(declare-function ollama-buddy--update-status "ollama-buddy-core" (status &optional original-model))
(declare-function ollama-buddy-user-prompts-load "ollama-buddy-user-prompts" ())
(defvar ollama-buddy--current-model)
(defvar ollama-buddy--current-system-prompt)
(defvar ollama-buddy--current-attachments)
(defvar ollama-buddy--response-format)
(defvar ollama-buddy-user-prompts-directory)
(defvar ollama-buddy-post-response-hook)

(defun ollama-buddy-annotate--collect-files (directory recursive)
  "Return a sorted list of source files in DIRECTORY.
When RECURSIVE is non-nil, descend into subdirectories."
  (let* ((dir (expand-file-name (file-name-as-directory directory)))
         (ext-regexp (concat "\\.\\("
                             (mapconcat #'regexp-quote
                                        ollama-buddy-annotate-directory-extensions
                                        "\\|")
                             "\\)\\'"))
         (files (if recursive
                    (directory-files-recursively dir ext-regexp nil
                                                 (lambda (d)
                                                   (not (string-match-p
                                                         "\\`\\."
                                                         (file-name-nondirectory
                                                          (directory-file-name d))))))
                  (directory-files dir t ext-regexp))))
    (sort (cl-remove-if-not #'file-regular-p files) #'string<)))

(defun ollama-buddy-annotate--estimate-tokens (file)
  "Return an estimated token count for FILE (chars / 4)."
  (ceiling (/ (float (or (file-attribute-size (file-attributes file)) 0)) 4.0)))

(defun ollama-buddy-annotate--fits-context-p (file model)
  "Return non-nil when FILE should fit MODEL's context window.
Leaves `ollama-buddy-annotate-context-reserve' free for the response."
  (let* ((ctx (or (ollama-buddy--get-model-context-size model) 8192))
         (budget (floor (* ctx (- 1.0 ollama-buddy-annotate-context-reserve))))
         (estimated (ollama-buddy-annotate--estimate-tokens file)))
    (cons (<= estimated budget)
          (list :estimated estimated :budget budget :context ctx))))

(defun ollama-buddy-annotate--ensure-skill-loaded ()
  "Ensure the Annotate Project skill is active as the system prompt.
Loads the skill file non-interactively if a matching one is found."
  (when (and (featurep 'ollama-buddy-user-prompts)
             (boundp 'ollama-buddy-user-prompts-directory)
             ollama-buddy-user-prompts-directory)
    (let ((skill-file (expand-file-name
                       ollama-buddy-annotate-directory-skill
                       ollama-buddy-user-prompts-directory)))
      (when (and (file-exists-p skill-file)
                 (fboundp 'ollama-buddy-user-prompts--read-prompt-content)
                 (fboundp 'ollama-buddy-user-prompts--extract-format)
                 (fboundp 'ollama-buddy-user-prompts--strip-org-headers)
                 (fboundp 'ollama-buddy--set-system-prompt-with-metadata))
        (let* ((content (funcall 'ollama-buddy-user-prompts--read-prompt-content
                                 skill-file))
               (format-spec (funcall 'ollama-buddy-user-prompts--extract-format
                                     content))
               (stripped (funcall 'ollama-buddy-user-prompts--strip-org-headers
                                  content)))
          (when format-spec
            (setq ollama-buddy--response-format format-spec))
          (funcall 'ollama-buddy--set-system-prompt-with-metadata
                   stripped "Annotate Project" "skill"))))))

(defun ollama-buddy-annotate--progress-label ()
  "Return a short progress label for the current queue."
  (let* ((remaining (length ollama-buddy-annotate--queue))
         (done (- ollama-buddy-annotate--queue-total remaining)))
    (format "annotate %d/%d" done ollama-buddy-annotate--queue-total)))

(defun ollama-buddy-annotate--send-next ()
  "Attach the next queued file and send the annotation request.
Skips files that exceed the model's context budget."
  (let ((model (or ollama-buddy-annotate--queue-model
                   ollama-buddy--current-model)))
    (while (and ollama-buddy-annotate--queue
                (not ollama-buddy-annotate--current-file))
      (let* ((next (pop ollama-buddy-annotate--queue))
             (fit (ollama-buddy-annotate--fits-context-p next model))
             (plist (cdr fit)))
        (if (not (car fit))
            (message "Annotate: skipping %s (est %d tok > budget %d)"
                     (file-name-nondirectory next)
                     (plist-get plist :estimated)
                     (plist-get plist :budget))
          (setq ollama-buddy-annotate--current-file next)
          (ignore-errors
            (setq ollama-buddy--current-attachments nil))
          ;; Suppress the "file type may not be well-supported" y-or-n-p
          ;; prompt during batch runs — the user has already confirmed the
          ;; whole batch.
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (ollama-buddy-attach-file next))
          (ollama-buddy--update-status
           (format "%s — %s"
                   (ollama-buddy-annotate--progress-label)
                   (file-name-nondirectory next)))
          (ollama-buddy--send
           (format "Annotate the attached file %s and return only the JSON object described by the schema."
                   (file-name-nondirectory next))
           model nil))))
    (unless (or ollama-buddy-annotate--current-file
                ollama-buddy-annotate--queue)
      (ollama-buddy-annotate--finish-batch))))

(defun ollama-buddy-annotate--finish-batch ()
  "Tear down batch state and remove the post-response handler."
  (remove-hook 'ollama-buddy-post-response-hook
               #'ollama-buddy-annotate--post-response-handler)
  (setq ollama-buddy-annotate--queue nil
        ollama-buddy-annotate--queue-total 0
        ollama-buddy-annotate--current-file nil
        ollama-buddy-annotate--queue-model nil)
  (ollama-buddy--update-status "Annotate batch finished")
  (message "Annotate directory batch finished"))

(defun ollama-buddy-annotate--post-response-handler (_model)
  "Apply annotations for the just-completed file and advance the queue."
  (when ollama-buddy-annotate--current-file
    (let ((file ollama-buddy-annotate--current-file))
      (setq ollama-buddy-annotate--current-file nil)
      (condition-case err
          (ollama-buddy-annotate-apply-last-response file)
        (error
         (message "Annotate: failed on %s: %s"
                  (file-name-nondirectory file)
                  (error-message-string err))))))
  (if ollama-buddy-annotate--queue
      (run-with-timer 0.25 nil #'ollama-buddy-annotate--send-next)
    (ollama-buddy-annotate--finish-batch)))

;;;###autoload
(defun ollama-buddy-annotate-directory (directory &optional recursive)
  "Annotate every source file under DIRECTORY via the current model.
With prefix arg, descend RECURSIVE-ly into subdirectories.
Each file is attached and sent as a separate request; the post-response
hook applies annotations automatically and queues the next file.  Files
whose estimated token cost exceeds the model context budget are skipped."
  (interactive
   (list (read-directory-name "Annotate directory: " nil nil t)
         current-prefix-arg))
  (when ollama-buddy-annotate--queue
    (user-error "Annotate batch already running; use `ollama-buddy-annotate-directory-cancel' first"))
  (let* ((directory (expand-file-name directory))
         (files (ollama-buddy-annotate--collect-files directory recursive)))
    (unless files
      (user-error "No matching source files%s under %s (extensions: %s). Customize `ollama-buddy-annotate-directory-extensions' to add more"
                  (if recursive "" " (non-recursive)")
                  directory
                  (mapconcat #'identity
                             ollama-buddy-annotate-directory-extensions " ")))
    (unless (y-or-n-p (format "Annotate %d file(s) under %s? "
                              (length files) directory))
      (user-error "Cancelled"))
    (ollama-buddy-annotate--ensure-skill-loaded)
    (setq ollama-buddy-annotate--queue files
          ollama-buddy-annotate--queue-total (length files)
          ollama-buddy-annotate--current-file nil
          ollama-buddy-annotate--queue-model ollama-buddy--current-model)
    (add-hook 'ollama-buddy-post-response-hook
              #'ollama-buddy-annotate--post-response-handler)
    (message "Annotate: queued %d file(s) for %s"
             (length files) ollama-buddy--current-model)
    (ollama-buddy-annotate--send-next)))

;;;###autoload
(defun ollama-buddy-annotate-directory-cancel ()
  "Cancel an in-progress `ollama-buddy-annotate-directory' batch."
  (interactive)
  (if (or ollama-buddy-annotate--queue
          ollama-buddy-annotate--current-file)
      (progn
        (ollama-buddy-annotate--finish-batch)
        (message "Annotate batch cancelled"))
    (message "No annotate batch in progress")))

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
