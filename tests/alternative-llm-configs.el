;; alternative-llm-configs.el
;;
;; Standalone configurations for gptel and ellama — two alternative Emacs LLM
;; packages for testing against ollama-buddy functionality.  Provided as a
;; reference/comparison file; eval the blocks you want rather than loading
;; from init.el.
;;
;; Requires: gptel, ellama, llm (all available on MELPA).
;;           Ollama running locally on localhost:11434.
;;
;; Feature comparison (Ollama only):
;;
;;   Feature              ollama-buddy   gptel          ellama
;;   -------------------  -----------    -----          ------
;;   Ollama (local)       yes            yes            yes
;;   Transient menu       yes            yes (polished) no
;;   In-buffer rewrite    yes            yes (built-in) yes (inline)
;;   Tool calling         yes            yes (recent)   yes (recent)
;;   Vision               yes            yes            yes
;;   Thinking models      yes (org)      partial        partial
;;   Org-mode output      yes            yes            partial
;;   Custom commands      yes (extensive) directives    built-in set


;;; ============================================================
;;; gptel
;;; ============================================================
;;
;; Closest in philosophy to ollama-buddy: streaming, transient menu,
;; org-mode output, multi-backend support.
;;
;; gptel feature mapping:
;;
;;   ollama-buddy                 gptel equivalent
;;   --------------------------   ------------------------------------
;;   C-c O  transient menu        gptel-menu
;;   C-c W  in-buffer rewrite     gptel-rewrite  (built-in, very polished)
;;   C-c m  model switch          gptel-menu -> Model / gptel-change-model
;;   system prompt                gptel-system-prompt (or per-buffer via menu)
;;   tool calling                 gptel-tools / gptel-make-tool
;;   context attachments          gptel-add  (region / buffer / file)
;;   org output                   gptel-default-mode 'org-mode
;;   command definitions          gptel-directives  (alist of name . prompt)

(use-package gptel
  :ensure t
  :bind (("C-c g"   . gptel)          ; open / switch to chat buffer
         ("C-c G"   . gptel-menu)     ; transient menu
         ("C-c M-r" . gptel-rewrite)  ; in-buffer rewrite on region
         ("C-c M-a" . gptel-add))     ; add region/buffer to context
  :config
  (require 'cl-lib)

  ;; --- Primary backend: local Ollama ---
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t
          :models '(tinyllama:1.1b
                    deepseek-r1:1.5b
                    llama3.1:8b)))

  (setq gptel-model 'tinyllama:1.1b)

  ;; --- Output format (org-mode like ollama-buddy) ---
  (setq gptel-default-mode 'org-mode)
  (setq gptel-use-header-line t)

  ;; --- Default system prompt ---
  (setq gptel-system-prompt
        "You are a helpful assistant. Be concise and precise.")

  ;; --- Streaming ---
  (setq gptel-stream t)

  ;; --- Tool calling ---
  (setq gptel-use-tools nil)            ; set t to enable for a session
  (setq gptel-include-tool-results t)
  (setq gptel-confirm-tool-calls t)


  ;; ============================================================
  ;; EDIFF-BASED FILE EDITING
  ;; ============================================================

  (defvar gptel-tool--ediff-target-file nil
    "The file being edited via gptel ediff.")

  (defvar gptel-tool--ediff-result nil
    "Result of the last gptel ediff operation.")

  (defvar gptel-tool--ediff-buffers nil
    "Cons of (original-buf . modified-buf) for cleanup.")

  (defun gptel-tool--ediff-quit-hook ()
    "Hook run when ediff quits.  Prompts to save and cleans up."
    (when gptel-tool--ediff-target-file
      (let ((modified-content (with-current-buffer ediff-buffer-B
                                (buffer-substring-no-properties (point-min) (point-max))))
            (original-content (with-current-buffer ediff-buffer-A
                                (buffer-substring-no-properties (point-min) (point-max)))))
        (if (string= modified-content original-content)
            (setq gptel-tool--ediff-result
                  (format "No changes made to %s" gptel-tool--ediff-target-file))
          (if (y-or-n-p (format "Save changes to %s? " gptel-tool--ediff-target-file))
              (progn
                (with-temp-file gptel-tool--ediff-target-file
                  (insert modified-content))
                (when-let ((buf (find-buffer-visiting gptel-tool--ediff-target-file)))
                  (with-current-buffer buf
                    (revert-buffer t t t)))
                (setq gptel-tool--ediff-result
                      (format "Applied changes to %s" gptel-tool--ediff-target-file)))
            (setq gptel-tool--ediff-result
                  (format "Discarded changes to %s" gptel-tool--ediff-target-file)))))

      ;; Cleanup
      (when gptel-tool--ediff-buffers
        (ignore-errors (kill-buffer (car gptel-tool--ediff-buffers)))
        (ignore-errors (kill-buffer (cdr gptel-tool--ediff-buffers))))
      (setq gptel-tool--ediff-target-file nil)
      (setq gptel-tool--ediff-buffers nil)))

  (defun gptel-tool--ediff-replace (path old-string new-string)
    "Replace OLD-STRING with NEW-STRING in file at PATH using ediff."
    (let* ((full-path (expand-file-name path))
           (original-content
            (condition-case err
                (with-temp-buffer
                  (insert-file-contents full-path)
                  (buffer-string))
              (error (format "Error reading file: %s" (error-message-string err))))))

      (when (string-prefix-p "Error" original-content)
        (cl-return-from gptel-tool--ediff-replace original-content))

      (let ((count (with-temp-buffer
                     (insert original-content)
                     (how-many (regexp-quote old-string) (point-min) (point-max)))))
        (cond
         ((= count 0)
          (format "Error: String not found in %s" path))
         ((> count 1)
          (format "Error: String found %d times in %s. Must be unique for safe replacement."
                  count path))
         (t
          (let* ((new-content (replace-regexp-in-string
                               (regexp-quote old-string) new-string original-content t t))
                 (original-buf
                  (generate-new-buffer
                   (format "*original: %s*" (file-name-nondirectory path))))
                 (modified-buf
                  (generate-new-buffer
                   (format "*proposed: %s*" (file-name-nondirectory path)))))

            (with-current-buffer original-buf
              (insert original-content)
              (goto-char (point-min))
              (let ((buffer-file-name full-path)) (set-auto-mode) (font-lock-ensure))
              (set-buffer-modified-p nil)
              (setq buffer-read-only t))

            (with-current-buffer modified-buf
              (insert new-content)
              (goto-char (point-min))
              (let ((buffer-file-name full-path)) (set-auto-mode) (font-lock-ensure))
              (set-buffer-modified-p nil))

            (let ((action
                   (read-char-choice
                    (format "Change in %s:\n\n  -%s\n  +%s\n\n[a]pply, [e]diff, [s]kip: "
                            (file-name-nondirectory path)
                            (truncate-string-to-width old-string 70 nil nil "...")
                            (truncate-string-to-width new-string 70 nil nil "..."))
                    '(?a ?e ?s))))
              (pcase action
                (?a
                 (with-temp-file full-path (insert new-content))
                 (kill-buffer original-buf)
                 (kill-buffer modified-buf)
                 (when-let ((buf (find-buffer-visiting full-path)))
                   (with-current-buffer buf (revert-buffer t t t)))
                 (format "Applied change to %s" path))

                (?e
                 (setq gptel-tool--ediff-target-file full-path)
                 (setq gptel-tool--ediff-buffers (cons original-buf modified-buf))
                 (setq gptel-tool--ediff-result nil)
                 (add-hook 'ediff-quit-hook #'gptel-tool--ediff-quit-hook)
                 (ediff-buffers original-buf modified-buf)
                 (format "Ediff started for %s. \
Use 'n'/'p' to navigate hunks, 'a'/'b' to choose version, 'q' to quit." path))

                (?s
                 (kill-buffer original-buf)
                 (kill-buffer modified-buf)
                 (format "Skipped change to %s" path))))))))))


  ;; ============================================================
  ;; DIFF ALTERNATIVE (simpler confirmation flow)
  ;; ============================================================

  (defun gptel-tool--diff-replace (path old-string new-string)
    "Replace OLD-STRING with NEW-STRING in PATH, showing a diff for confirmation."
    (let* ((full-path (expand-file-name path))
           (original-content
            (condition-case err
                (with-temp-buffer
                  (insert-file-contents full-path)
                  (buffer-string))
              (error nil))))
      (unless original-content
        (cl-return-from gptel-tool--diff-replace
          (format "Error: Cannot read file %s" path)))

      (let ((count (with-temp-buffer
                     (insert original-content)
                     (how-many (regexp-quote old-string) (point-min) (point-max)))))
        (cond
         ((= count 0)
          (format "Error: String not found in %s" path))
         ((> count 1)
          (format "Error: String found %d times, must be unique" count))
         (t
          (let* ((new-content (replace-regexp-in-string
                               (regexp-quote old-string) new-string original-content t t))
                 (diff-output
                  (let ((old-temp (make-temp-file "gptel-old"))
                        (new-temp (make-temp-file "gptel-new")))
                    (unwind-protect
                        (progn
                          (with-temp-file old-temp (insert original-content))
                          (with-temp-file new-temp (insert new-content))
                          (shell-command-to-string
                           (format "diff -u --color=never %s %s | tail -n +3"
                                   (shell-quote-argument old-temp)
                                   (shell-quote-argument new-temp))))
                      (delete-file old-temp)
                      (delete-file new-temp)))))

            (with-current-buffer (get-buffer-create "*gptel-diff*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (propertize (format "Proposed changes to: %s\n" path)
                                    'face 'font-lock-keyword-face))
                (insert (make-string 60 ?─) "\n\n")
                (insert diff-output)
                (goto-char (point-min))
                (diff-mode)
                (view-mode 1))
              (display-buffer (current-buffer)
                              '((display-buffer-reuse-window
                                 display-buffer-below-selected)
                                (window-height . 0.4))))

            (if (y-or-n-p (format "Apply change to %s? " path))
                (progn
                  (with-temp-file full-path (insert new-content))
                  (when-let ((buf (find-buffer-visiting full-path)))
                    (with-current-buffer buf (revert-buffer t t t)))
                  (when-let ((diff-buf (get-buffer "*gptel-diff*")))
                    (kill-buffer diff-buf))
                  (format "Applied change to %s" path))
              (when-let ((diff-buf (get-buffer "*gptel-diff*")))
                (kill-buffer diff-buf))
              (format "Skipped change to %s" path))))))))


  ;; ============================================================
  ;; TOOLS
  ;; ============================================================

  (setq gptel-tools
        (list

         ;; ============ FILE OPERATIONS ============

         (gptel-make-tool
          :function (lambda (path)
                      (condition-case err
                          (with-temp-buffer
                            (insert-file-contents (expand-file-name path))
                            (buffer-string))
                        (error (format "Error reading file: %s"
                                       (error-message-string err)))))
          :name "read_file"
          :description "Read the contents of a file."
          :args (list '(:name "path" :type string
                        :description "The path to the file"))
          :category "filesystem")

         (gptel-make-tool
          :function (lambda (path content)
                      (let* ((full-path (expand-file-name path))
                             (exists (file-exists-p full-path))
                             (prompt (if exists
                                         (format "Overwrite %s (%d bytes)? "
                                                 path (length content))
                                       (format "Create %s (%d bytes)? "
                                               path (length content)))))
                        (if (y-or-n-p prompt)
                            (progn
                              (make-directory (file-name-directory full-path) t)
                              (with-temp-file full-path (insert content))
                              (format "Wrote %d bytes to %s" (length content) path))
                          (format "Skipped writing to %s" path))))
          :name "write_file"
          :description "Write content to a file, creating or overwriting it."
          :args (list '(:name "path" :type string
                        :description "The path to the file")
                      '(:name "content" :type string
                        :description "The content to write"))
          :category "filesystem")

         (gptel-make-tool
          :function #'gptel-tool--ediff-replace
          :name "str_replace_in_file"
          :description "Replace a unique string in a file. \
Shows diff and asks for confirmation via [a]pply / [e]diff / [s]kip."
          :args (list '(:name "path" :type string
                        :description "The path to the file")
                      '(:name "old_string" :type string
                        :description "The exact string to replace (must be unique)")
                      '(:name "new_string" :type string
                        :description "The replacement string"))
          :category "filesystem")

         (gptel-make-tool
          :function (lambda (directory)
                      (condition-case err
                          (mapconcat
                           (lambda (f)
                             (if (file-directory-p (expand-file-name f directory))
                                 (concat f "/") f))
                           (directory-files (expand-file-name directory) nil "^[^.]")
                           "\n")
                        (error (format "Error: %s" (error-message-string err)))))
          :name "list_directory"
          :description "List files and directories.  Directories have a trailing slash."
          :args (list '(:name "directory" :type string
                        :description "The directory path"))
          :category "filesystem")

         (gptel-make-tool
          :function (lambda (directory pattern)
                      (shell-command-to-string
                       (format "find %s -type f -name %s 2>/dev/null | head -50"
                               (shell-quote-argument (expand-file-name directory))
                               (shell-quote-argument pattern))))
          :name "find_files"
          :description "Find files matching a glob pattern."
          :args (list '(:name "directory" :type string
                        :description "The root directory to search from")
                      '(:name "pattern" :type string
                        :description "Glob pattern, e.g. \"*.el\""))
          :category "filesystem")

         ;; ============ CODE SEARCH ============

         (gptel-make-tool
          :function (lambda (pattern directory)
                      (let ((cmd (if (executable-find "rg")
                                     (format "rg -n --no-heading %s %s 2>/dev/null | head -100"
                                             (shell-quote-argument pattern)
                                             (shell-quote-argument
                                              (expand-file-name directory)))
                                   (format "grep -rn %s %s 2>/dev/null | head -100"
                                           (shell-quote-argument pattern)
                                           (shell-quote-argument
                                            (expand-file-name directory))))))
                        (let ((result (shell-command-to-string cmd)))
                          (if (string-empty-p result) "No matches found." result))))
          :name "search_code"
          :description "Search for a pattern in files using ripgrep or grep."
          :args (list '(:name "pattern" :type string
                        :description "The search pattern (regex)")
                      '(:name "directory" :type string
                        :description "Directory to search in"))
          :category "code")

         (gptel-make-tool
          :function (lambda (symbol directory)
                      (let ((cmd (if (executable-find "rg")
                                     (format "rg -n --no-heading -w %s %s 2>/dev/null | head -50"
                                             (shell-quote-argument symbol)
                                             (shell-quote-argument
                                              (expand-file-name directory)))
                                   (format "grep -rnw %s %s 2>/dev/null | head -50"
                                           (shell-quote-argument symbol)
                                           (shell-quote-argument
                                            (expand-file-name directory))))))
                        (let ((result (shell-command-to-string cmd)))
                          (if (string-empty-p result) "No matches found." result))))
          :name "find_symbol"
          :description "Find a symbol (function, variable, class) as a whole word."
          :args (list '(:name "symbol" :type string
                        :description "The symbol name to find")
                      '(:name "directory" :type string
                        :description "Directory to search in"))
          :category "code")

         ;; ============ SHELL ============

         (gptel-make-tool
          :function (lambda (command directory)
                      (let ((default-directory
                             (expand-file-name (or directory default-directory))))
                        (if (y-or-n-p (format "Run: %s\nin: %s? "
                                              command default-directory))
                            (shell-command-to-string command)
                          "Command cancelled by user")))
          :name "run_shell_command"
          :description "Run a shell command and return its output."
          :args (list '(:name "command" :type string
                        :description "The shell command to run")
                      '(:name "directory" :type string
                        :description "Working directory (optional, defaults to current)"))
          :category "system")

         ;; ============ GIT ============

         (gptel-make-tool
          :function (lambda (directory)
                      (let ((default-directory (expand-file-name directory)))
                        (shell-command-to-string
                         "git status --short 2>/dev/null || echo 'Not a git repo'")))
          :name "git_status"
          :description "Get the git status of a repository."
          :args (list '(:name "directory" :type string
                        :description "Path to the git repository"))
          :category "git")

         (gptel-make-tool
          :function (lambda (directory)
                      (let ((default-directory (expand-file-name directory)))
                        (shell-command-to-string "git diff 2>/dev/null")))
          :name "git_diff"
          :description "Show unstaged git changes."
          :args (list '(:name "directory" :type string
                        :description "Path to the git repository"))
          :category "git")

         (gptel-make-tool
          :function (lambda (directory n)
                      (let ((default-directory (expand-file-name directory)))
                        (shell-command-to-string
                         (format "git log --oneline -n %d 2>/dev/null" (or n 10)))))
          :name "git_log"
          :description "Show recent git commits."
          :args (list '(:name "directory" :type string
                        :description "Path to the git repository")
                      '(:name "n" :type integer
                        :description "Number of commits to show (default 10)"))
          :category "git")

         ;; ============ EMACS ============

         (gptel-make-tool
          :function (lambda (buffer)
                      (if (buffer-live-p (get-buffer buffer))
                          (with-current-buffer buffer
                            (buffer-substring-no-properties (point-min) (point-max)))
                        (format "Buffer '%s' not found" buffer)))
          :name "read_buffer"
          :description "Read the full contents of an Emacs buffer."
          :args (list '(:name "buffer" :type string
                        :description "The buffer name"))
          :category "emacs")

         (gptel-make-tool
          :function (lambda ()
                      (format "Buffer: %s\nFile: %s\nMode: %s\nDirectory: %s"
                              (buffer-name)
                              (or buffer-file-name "none")
                              major-mode
                              default-directory))
          :name "current_context"
          :description "Get current buffer name, file path, major mode, and directory."
          :args nil
          :category "emacs")

         (gptel-make-tool
          :function (lambda ()
                      (mapconcat
                       (lambda (b)
                         (format "%s [%s]" (buffer-name b)
                                 (buffer-local-value 'major-mode b)))
                       (seq-filter
                        (lambda (b) (not (string-prefix-p " " (buffer-name b))))
                        (buffer-list))
                       "\n"))
          :name "list_buffers"
          :description "List all open Emacs buffers with their major modes."
          :args nil
          :category "emacs"))))

;; --- Custom directives (equivalent of ollama-buddy-command-definitions) ---
;;
;; Access via gptel-menu -> Directives, or call gptel-rewrite with a prompt.

(with-eval-after-load 'gptel
  (add-to-list 'gptel-directives
               '(refactor
                 . "You are an expert programmer. Refactor the provided code \
for clarity and efficiency. Return only the improved code with no explanation."))

  (add-to-list 'gptel-directives
               '(proofread
                 . "You are a professional editor. Fix grammar, style, and \
clarity. Return only the corrected text."))

  (add-to-list 'gptel-directives
               '(explain
                 . "Explain the following code clearly and concisely."))

  (add-to-list 'gptel-directives
               '(summarise
                 . "Summarise the following text in bullet points.")))


;;; ============================================================
;;; ellama
;;; ============================================================
;;
;; More command-oriented than gptel — similar to ollama-buddy's custom menu
;; actions.  Built on llm.el which provides a unified backend abstraction.
;;
;; ellama feature mapping:
;;
;;   ollama-buddy                 ellama equivalent
;;   --------------------------   ------------------------------------
;;   chat buffer                  ellama-chat
;;   command definitions          built-in: ellama-improve-wording,
;;                                  ellama-make-concise, ellama-summarize,
;;                                  ellama-complete, etc.
;;   in-buffer rewrite            ellama-improve-grammar-wording, ellama-change
;;   model / provider switch      ellama-change-provider
;;   code actions                 ellama-code-improve, ellama-code-review,
;;                                  ellama-code-add, ellama-code-complete
;;   RAG / context                ellama-context-* commands
;;   sessions                     ellama-load-session, ellama-save-session

(use-package llm
  :ensure t)

(use-package ellama
  :ensure t
  :bind-keymap ("C-c l" . ellama-keymap-command)
  :init
  (require 'llm-ollama)

  :config

  ;; --- Primary provider: local Ollama ---
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "tinyllama:1.1b"
           :embedding-model "nomic-embed-text:latest"))

  ;; --- Named providers — switch with M-x ellama-change-provider ---
  (setopt ellama-providers
          `(("tinyllama"
             . ,(make-llm-ollama :chat-model "tinyllama:1.1b"))
            ("deepseek-r1"
             . ,(make-llm-ollama :chat-model "deepseek-r1:1.5b"))
            ("llama3.1"
             . ,(make-llm-ollama :chat-model "llama3.1:8b"))))

  ;; --- Behaviour ---
  (setopt ellama-language "English")
  (setopt ellama-fill-paragraphs nil)   ; don't reflow; cleaner for code

  ;; Name chat sessions automatically via the LLM
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm))
