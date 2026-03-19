;;; ollama-buddy-tools.el --- Tool calling support for Ollama Buddy -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 2.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This module provides tool calling (function calling) support for Ollama Buddy.
;; It allows LLMs to invoke Emacs functions to perform tasks like file operations,
;; shell commands, org-mode queries, and more.
;;
;; Tool calling enables the LLM to:
;; - Read and write files
;; - Execute shell commands
;; - Search and modify buffers
;; - Query org-mode agendas and notes
;; - Perform calculations
;; - And more...
;;
;; Usage:
;;
;;   (require 'ollama-buddy-tools)
;;
;;   ;; Enable tool calling globally
;;   (setq ollama-buddy-tools-enabled t)
;;
;;   ;; Or enable for specific sessions
;;   (ollama-buddy-tools-toggle)
;;
;;   ;; Register custom tools
;;   (ollama-buddy-tools-register
;;    'my-custom-tool
;;    "Performs a custom operation"
;;    '((type . "object")
;;      (required . ["input"])
;;      (properties . ((input . ((type . "string")
;;                               (description . "Input parameter"))))))
;;    (lambda (args)
;;      (let ((input (alist-get 'input args)))
;;        (format "Processed: %s" input))))
;;
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'calc)
(require 'project)
(require 'ollama-buddy-core)

;; Web search functions (optional, loaded when available)
(declare-function ollama-buddy-web-search--fetch-sync "ollama-buddy-web-search")

;; RAG functions (optional, loaded when available)
(declare-function ollama-buddy-rag--list-index-names "ollama-buddy-rag")
(declare-function ollama-buddy-rag--load-index "ollama-buddy-rag")
(declare-function ollama-buddy-rag--get-embedding-async "ollama-buddy-rag")
(declare-function ollama-buddy-rag--search-chunks "ollama-buddy-rag")
(declare-function ollama-buddy-rag--format-results-for-context "ollama-buddy-rag")
(defvar ollama-buddy-rag-top-k)
(defvar ollama-buddy-rag-similarity-threshold)

;;; Customization

(defgroup ollama-buddy-tools nil
  "Tool calling configuration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-tools-")

(defcustom ollama-buddy-tools-enabled nil
  "Whether to enable tool calling functionality.
When non-nil, the LLM can invoke registered tools during conversations."
  :type 'boolean
  :group 'ollama-buddy-tools)

(defcustom ollama-buddy-tools-auto-execute nil
  "Whether to automatically execute tool calls without confirmation.
When nil, prompt the user before executing each tool call."
  :type 'boolean
  :group 'ollama-buddy-tools)

(defcustom ollama-buddy-tools-max-iterations 20
  "Maximum number of tool-call iterations per user prompt.
This prevents infinite loops if the LLM keeps calling tools."
  :type 'integer
  :group 'ollama-buddy-tools)

(defcustom ollama-buddy-tools-safe-mode nil
  "When non-nil, restrict tools to safe read-only operations.
Disables tools that can modify files or execute arbitrary commands."
  :type 'boolean
  :group 'ollama-buddy-tools)

(defcustom ollama-buddy-tools-unguarded nil
  "When non-nil, bypass ALL safety prompts and let tools run to completion.
This disables confirmation for unsafe tools (e.g. `execute_shell'),
large result warnings, and safe-mode restrictions.  Auto-execute is
implied.  The header line turns red as a visual warning."
  :type 'boolean
  :group 'ollama-buddy-tools)

(defcustom ollama-buddy-tools-builtin-enabled t
  "Whether to enable built-in tools.
When nil, only custom user-registered tools are available."
  :type 'boolean
  :group 'ollama-buddy-tools)

(defcustom ollama-buddy-tools-large-result-threshold 2000
  "Token count threshold for warning about large tool results.
When a tool result exceeds this many estimated tokens, the user is
prompted even when `ollama-buddy-tools-auto-execute' is active.
Set to 0 to disable the warning."
  :type 'integer
  :group 'ollama-buddy-tools)

;;; Internal Variables

(defvar ollama-buddy-tools--registry (make-hash-table :test 'equal)
  "Hash table storing registered tools.
Keys are tool names (strings), values are plists with:
  :description - Tool description
  :parameters  - JSON schema for parameters
  :function    - Elisp function to execute
  :safe        - Whether tool is safe (read-only)")

(defvar ollama-buddy-tools--schema-cache nil
  "Cached schema vector from `ollama-buddy-tools--generate-schema'.
Set to nil to invalidate.")

(defvar ollama-buddy-tools--current-iteration 0
  "Current tool-call iteration count for the active request.")

(defvar ollama-buddy-tools--pending-calls nil
  "List of pending tool calls waiting to be executed.")

(defvar ollama-buddy-tools--pause-continuation nil
  "When non-nil, suppress automatic tool-continuation after the current tool batch.
Tools that open interactive UI sessions (e.g. ediff) set this so the LLM does
not keep calling tools while the user is busy in that session.  The user can
send a new message to resume the conversation normally.")

(defvar ollama-buddy-tools--session-paused nil
  "When non-nil, the chat session is paused waiting for the user to finish an
interactive tool UI (e.g. ediff).  Shown as ⏸ in the status line.
Cleared automatically when the user sends a new message.")

(defvar ollama-buddy-tools--stop-after-batch nil
  "When non-nil, end the tool batch without auto-continuing the LLM.
Set when any :terminal tool executes.  Cleared after the batch.")

;;; Tool Registration

(defun ollama-buddy-tools-register (name description parameters function &optional safe terminal)
  "Register a tool for use with Ollama Buddy.

NAME is the tool name (symbol or string).
DESCRIPTION is a human-readable description of what the tool does.
PARAMETERS is a JSON schema object describing the tool's parameters.
FUNCTION is the Elisp function to execute when the tool is called.
  It receives an alist of arguments and should return a string result.
SAFE indicates if the tool is safe (read-only) - defaults to nil.
TERMINAL when non-nil, stops auto-continuation after this tool executes.
  Use for tools that launch interactive Emacs UI sessions (e.g. ediff)
  where you want the LLM to stop and wait for the user's next message.

Example:

  (ollama-buddy-tools-register
   \\='read-file
   \"Read the contents of a file\"
   \\='((type . \"object\")
     (required . [\"path\"])
     (properties . ((path . ((type . \"string\")
                             (description . \"File path to read\"))))))
   (lambda (args)
     (let ((path (alist-get \\='path args)))
       (with-temp-buffer
         (insert-file-contents path)
         (buffer-string))))
   t)"
  (let ((name-str (if (symbolp name) (symbol-name name) name)))
    (puthash name-str
             (list :description description
                   :parameters parameters
                   :function function
                   :safe (or safe nil)
                   :terminal (or terminal nil))
             ollama-buddy-tools--registry)
    (setq ollama-buddy-tools--schema-cache nil)
    (message "Registered tool: %s" name-str)))

(defun ollama-buddy-tools-unregister (name)
  "Unregister tool NAME from the registry."
  (let ((name-str (if (symbolp name) (symbol-name name) name)))
    (remhash name-str ollama-buddy-tools--registry)
    (setq ollama-buddy-tools--schema-cache nil)
    (message "Unregistered tool: %s" name-str)))

(defun ollama-buddy-tools-list ()
  "Return a list of all registered tool names."
  (let (tools)
    (maphash (lambda (name _spec) (push name tools))
             ollama-buddy-tools--registry)
    (sort tools #'string<)))

(defun ollama-buddy-tools-get (name)
  "Get tool specification for NAME."
  (let ((name-str (if (symbolp name) (symbol-name name) name)))
    (gethash name-str ollama-buddy-tools--registry)))

(defun ollama-buddy-tools-clear ()
  "Clear all registered tools."
  (interactive)
  (clrhash ollama-buddy-tools--registry)
  (setq ollama-buddy-tools--schema-cache nil)
  (message "Cleared all registered tools"))

;;; Tool Schema Generation

(defvar ollama-buddy-tools--schema-cache-safe-mode nil
  "Value of `ollama-buddy-tools-safe-mode' when the schema cache was built.")

(defun ollama-buddy-tools--generate-schema ()
  "Generate the tools schema for the Ollama API request.
Returns a vector of tool definitions in the format expected by Ollama.
Uses a cached result when the registry and safe-mode have not changed."
  (if (and ollama-buddy-tools--schema-cache
           (eq ollama-buddy-tools--schema-cache-safe-mode
               ollama-buddy-tools-safe-mode))
      ollama-buddy-tools--schema-cache
    (let (tools-list)
      (maphash
       (lambda (name spec)
         (when (or (not ollama-buddy-tools-safe-mode)
                   (plist-get spec :safe))
           (push `((type . "function")
                   (function . ((name . ,name)
                                (description . ,(plist-get spec :description))
                                (parameters . ,(plist-get spec :parameters)))))
                 tools-list)))
       ollama-buddy-tools--registry)
      (setq ollama-buddy-tools--schema-cache-safe-mode ollama-buddy-tools-safe-mode
            ollama-buddy-tools--schema-cache
            (when tools-list
              (vconcat (nreverse tools-list)))))))

;;; Fragment Merging

(defun ollama-buddy-tools--merge-fragment (original fragment)
  "Merge FRAGMENT into ORIGINAL file content, returning the full file.
Tries to find the best overlap between the fragment and the original
by searching for matching lines at the start and end of the fragment,
then splicing the fragment into the corresponding region of the original."
  (let* ((orig-lines (split-string original "\n"))
         (frag-lines (split-string fragment "\n"))
         ;; Strip leading/trailing blank lines from fragment for matching
         (frag-trimmed (cl-loop for l in frag-lines
                                unless (string-blank-p l) collect l))
         (first-frag-line (car frag-trimmed))
         (last-frag-line (car (last frag-trimmed)))
         (start-idx nil)
         (end-idx nil))
    ;; Find where the first non-blank fragment line appears in original
    (when first-frag-line
      (cl-loop for i from 0
               for line in orig-lines
               when (string= (string-trim line) (string-trim first-frag-line))
               do (setq start-idx i) and return nil))
    ;; Find where the last non-blank fragment line appears in original
    ;; Search from the end to handle repeated lines correctly
    (when last-frag-line
      (cl-loop for i downfrom (1- (length orig-lines)) to 0
               when (string= (string-trim (nth i orig-lines))
                              (string-trim last-frag-line))
               do (setq end-idx i) and return nil))
    (if (and start-idx end-idx (>= end-idx start-idx))
        ;; Splice: keep original before start, insert fragment, keep original after end
        (mapconcat #'identity
                   (append (cl-subseq orig-lines 0 start-idx)
                           frag-lines
                           (cl-subseq orig-lines (1+ end-idx)))
                   "\n")
      ;; Could not locate fragment in original — return fragment as-is
      fragment)))

;;; Tool Execution

(defun ollama-buddy-tools--format-args-for-display (arguments)
  "Format ARGUMENTS alist for display, truncating long string values.
ARGUMENTS may be an alist or a JSON string (as returned by remote providers)."
  (let ((args (if (stringp arguments)
                  (condition-case nil
                      (json-read-from-string arguments)
                    (error nil))
                arguments)))
    (if (or (null args) (not (listp args)))
        (format "%s" (or arguments ""))
      (mapconcat
       (lambda (pair)
         (let* ((key (car pair))
                (val (cdr pair))
                (val-str (if (stringp val)
                             (if (> (length val) 60)
                                 (format "\"%s\"… (%d chars)"
                                         (substring val 0 60) (length val))
                               (format "%S" val))
                           (format "%S" val))))
           (format "%s: %s" key val-str)))
       args
       ", "))))

(defun ollama-buddy-tools--check-result-size (result tool-name)
  "Check RESULT size from TOOL-NAME and warn if it exceeds the threshold.
Returns the result string, possibly truncated, or signals an error if cancelled."
  (let ((threshold ollama-buddy-tools-large-result-threshold))
    (if (or ollama-buddy-tools-unguarded
            (<= threshold 0) (not (stringp result)))
        result
      (let ((token-estimate (ollama-buddy--estimate-token-count result)))
        (if (<= token-estimate threshold)
            result
          ;; Large result — always prompt, even during auto-execute
          (let ((answer (read-char-choice
                         (format "%s returned ~%d tokens (%d chars). (p)roceed (t)runcate to %d tokens (c)ancel: "
                                 tool-name token-estimate (length result) threshold)
                         '(?p ?t ?c))))
            (message nil)
            (pcase answer
              (?p result)
              (?t (ollama-buddy-tools--truncate-to-tokens result threshold))
              (?c (error "Tool result too large, cancelled by user")))))))))

(defun ollama-buddy-tools--truncate-to-tokens (text max-tokens)
  "Truncate TEXT to approximately MAX-TOKENS tokens.
Cuts at a word boundary and appends a truncation notice."
  (let* ((words (split-string text))
         ;; ~1.3 tokens per word, so max-tokens / 1.3 words
         (max-words (max 1 (round (/ max-tokens 1.3))))
         (kept (seq-take words max-words))
         (truncated (mapconcat #'identity kept " ")))
    (format "%s\n\n[TRUNCATED: result was ~%d tokens, showing first ~%d]"
            truncated
            (ollama-buddy--estimate-token-count text)
            max-tokens)))

(defun ollama-buddy-tools--execute (name arguments)
  "Execute tool NAME with ARGUMENTS.
Returns the result as a string, or an error message if execution fails."
  (condition-case err
      (let* ((name-str (if (symbolp name) (symbol-name name) name))
             (spec (ollama-buddy-tools-get name-str))
             (func (plist-get spec :function)))
        (unless spec
          (error "Tool not found: %s" name-str))
        (unless func
          (error "Tool has no function: %s" name-str))
        ;; Safety check (skipped in unguarded mode)
        (when (and ollama-buddy-tools-safe-mode
                   (not ollama-buddy-tools-unguarded)
                   (not (plist-get spec :safe)))
          (error "Tool %s is not safe for execution in safe mode" name-str))
        ;; Confirmation check — unsafe tools always require confirmation
        ;; (skipped entirely in unguarded mode)
        (unless ollama-buddy-tools-unguarded
          (when (or (not ollama-buddy-tools-auto-execute)
                    (not (plist-get spec :safe)))
            (let ((answer (read-char-choice
                           (format "Execute tool %s(%s)? (y)es (n)o (a)ccept all: "
                                   name-str
                                   (ollama-buddy-tools--format-args-for-display arguments))
                           '(?y ?n ?a))))
              (message nil)
              (pcase answer
                (?a (setq ollama-buddy-tools-auto-execute t))
                (?n (error "Tool execution cancelled by user"))))))
        ;; If this tool is marked terminal, flag that auto-continuation should
        ;; stop after this batch — set before calling in case of error.
        (when (plist-get spec :terminal)
          (setq ollama-buddy-tools--stop-after-batch t))
        ;; Some models send arguments as a JSON string rather than a JSON object.
        ;; Parse it into an alist so tool functions can use alist-get normally.
        (let* ((parsed-args (if (stringp arguments)
                                (condition-case nil
                                    (json-read-from-string arguments)
                                  (error arguments))
                              arguments))
               (result (funcall func parsed-args))
               (result-str (if (stringp result) result (format "%S" result))))
          ;; Guard: warn on large results that could consume significant context
          (ollama-buddy-tools--check-result-size result-str name-str)))
    (error
     (format "Error executing tool %s: %s" name (error-message-string err)))))

;;; Tool Call Processing

(defun ollama-buddy-tools--process-tool-calls (tool-calls)
  "Process TOOL-CALLS from the LLM response.
Returns a list of tool result messages to append to the conversation."
  (let (results)
    (dolist (call tool-calls)
      (let* ((function (alist-get 'function call))
             (name (alist-get 'name function))
             (arguments (alist-get 'arguments function))
             (result (ollama-buddy-tools--execute name arguments)))
        (push `((role . "tool")
                (tool_name . ,name)
                (content . ,result))
              results)))
    (nreverse results)))

;;; Built-in Tools

(defun ollama-buddy-tools--init-builtin ()
  "Initialize built-in tools."
  (when ollama-buddy-tools-builtin-enabled
    
    ;; read_file - Read file contents
    (ollama-buddy-tools-register
     'read_file
     "Read the contents of a file from the filesystem"
     '((type . "object")
       (required . ["path"])
       (properties . ((path . ((type . "string")
                               (description . "Path to the file to read"))))))
     (lambda (args)
       (let ((path (alist-get 'path args)))
         (if (and path (file-exists-p path) (file-readable-p path))
             (with-temp-buffer
               (insert-file-contents path)
               (buffer-string))
           (format "Error: Cannot read file %s" path))))
     t) ; safe
    
    ;; write_file - Write file contents (unsafe)
    (ollama-buddy-tools-register
     'write_file
     "Write content to a file on the filesystem"
     '((type . "object")
       (required . ["path" "content"])
       (properties . ((path . ((type . "string")
                               (description . "Path to the file to write")))
                      (content . ((type . "string")
                                  (description . "Content to write to the file"))))))
     (lambda (args)
       (let ((path (alist-get 'path args))
             (content (alist-get 'content args)))
         (condition-case err
             (progn
               (with-temp-file path
                 (insert content))
               (format "Successfully wrote to %s" path))
           (error (format "Error writing file: %s" (error-message-string err))))))
     nil) ; not safe
    
    ;; list_directory - List directory contents
    (ollama-buddy-tools-register
     'list_directory
     "List the contents of a directory"
     '((type . "object")
       (required . ["path"])
       (properties . ((path . ((type . "string")
                               (description . "Path to the directory to list"))))))
     (lambda (args)
       (let ((path (alist-get 'path args)))
         (if (and path (file-directory-p path))
             (mapconcat #'identity (directory-files path nil "^[^.]") "\n")
           (format "Error: Cannot access directory %s" path))))
     t) ; safe
    
    ;; execute_shell - Execute shell command (unsafe)
    (ollama-buddy-tools-register
     'execute_shell
     "Execute a shell command and return its output"
     '((type . "object")
       (required . ["command"])
       (properties . ((command . ((type . "string")
                                  (description . "Shell command to execute"))))))
     (lambda (args)
       (let ((command (alist-get 'command args)))
         (condition-case err
             (shell-command-to-string command)
           (error (format "Error executing command: %s" (error-message-string err))))))
     nil) ; not safe
    
    ;; get_buffer_content - Get content of an Emacs buffer
    (ollama-buddy-tools-register
     'get_buffer_content
     "Get the content of an Emacs buffer"
     '((type . "object")
       (required . ["buffer"])
       (properties . ((buffer . ((type . "string")
                                 (description . "Name of the buffer to read"))))))
     (lambda (args)
       (let ((buffer-name (alist-get 'buffer args)))
         (if (get-buffer buffer-name)
             (with-current-buffer buffer-name
               (buffer-string))
           (format "Error: Buffer %s not found" buffer-name))))
     t) ; safe
    
    ;; list_buffers - List open buffers
    (ollama-buddy-tools-register
     'list_buffers
     "List open Emacs buffers in a structured format showing modified/read-only flags, name, size, mode and filename. Internal buffers (names starting with a space) are hidden by default unless a pattern is given."
     '((type . "object")
       (properties . ((pattern . ((type . "string")
                                  (description . "Optional regex pattern to filter buffer names. When given, internal buffers are also included."))))))
     (lambda (args)
       (let* ((pattern (alist-get 'pattern args))
              (buffers (seq-filter
                        (lambda (b)
                          (let ((name (buffer-name b)))
                            (if pattern
                                (string-match-p pattern name)
                              ;; By default hide internal buffers (space-prefixed)
                              (not (string-prefix-p " " name)))))
                        (buffer-list)))
              (header (format " %-2s  %-24s  %6s  %-18s %s\n %s  %s  %s  %s  %s\n"
                              "MR" "Name" "Size" "Mode" "Filename/Process"
                              "--" (make-string 24 ?-) "------"
                              (make-string 18 ?-) (make-string 16 ?-)))
              (rows (mapcar
                     (lambda (buf)
                       (let* ((name (buffer-name buf))
                              (display-name (if (> (length name) 24)
                                               (concat (substring name 0 21) "...")
                                             name))
                              (size (buffer-size buf))
                              (mode-str (let ((m (format-mode-line
                                                  (buffer-local-value 'mode-name buf)
                                                  nil nil buf)))
                                          (if (> (length m) 18)
                                              (concat (substring m 0 15) "...")
                                            m)))
                              (modified (if (buffer-modified-p buf) "*" " "))
                              (read-only (if (buffer-local-value 'buffer-read-only buf) "%" " "))
                              (filename (let ((f (or (buffer-file-name buf)
                                                     (and (local-variable-p
                                                           'list-buffers-directory buf)
                                                          (buffer-local-value
                                                           'list-buffers-directory buf)))))
                                          (if f (abbreviate-file-name f) ""))))
                         (format " %s%s  %-24s  %6d  %-18s %s"
                                 modified read-only display-name size mode-str filename)))
                     buffers))
              (total (length buffers))
              (file-count (length (seq-filter #'buffer-file-name buffers)))
              (footer (format "\n %d buffer%s  %d file%s"
                              total (if (= total 1) "" "s")
                              file-count (if (= file-count 1) "" "s"))))
         (concat header (mapconcat #'identity rows "\n") footer)))
     t) ; safe
    
    ;; calculate - Perform calculation
    (ollama-buddy-tools-register
     'calculate
     "Evaluate a mathematical expression"
     '((type . "object")
       (required . ["expression"])
       (properties . ((expression . ((type . "string")
                                     (description . "Mathematical expression to evaluate"))))))
     (lambda (args)
       (let ((expr (alist-get 'expression args)))
         (condition-case err
             (format "%s" (calc-eval expr))
           (error (format "Error evaluating expression: %s" (error-message-string err))))))
     t) ; safe
    
    ;; search_buffer - Search in current buffer
    (ollama-buddy-tools-register
     'search_buffer
     "Search for a pattern in the current buffer"
     '((type . "object")
       (required . ["pattern"])
       (properties . ((pattern . ((type . "string")
                                  (description . "Regex pattern to search for")))
                      (buffer . ((type . "string")
                                (description . "Optional buffer name to search in"))))))
     (lambda (args)
       (let ((pattern (alist-get 'pattern args))
             (buffer-name (alist-get 'buffer args))
             matches)
         (with-current-buffer (or (and buffer-name (get-buffer buffer-name))
                                 (current-buffer))
           (save-excursion
             (goto-char (point-min))
             (while (re-search-forward pattern nil t)
               (push (format "Line %d: %s"
                           (line-number-at-pos)
                           (string-trim (thing-at-point 'line t)))
                     matches))))
         (if matches
             (mapconcat #'identity (nreverse matches) "\n")
           "No matches found")))
     t) ; safe

    ;; search_files - Grep for a pattern across files in a directory
    (ollama-buddy-tools-register
     'search_files
     "Search for a regex pattern across files in a directory tree. Returns matching lines with file paths and line numbers. Skips binary files and hidden directories (e.g. .git). Use the glob parameter to restrict to specific file types."
     '((type . "object")
       (required . ["pattern" "directory"])
       (properties . ((pattern . ((type . "string")
                                  (description . "Regex pattern to search for in file contents")))
                      (directory . ((type . "string")
                                    (description . "Root directory to search in")))
                      (glob . ((type . "string")
                               (description . "Optional file name glob to filter files (e.g. \"*.el\", \"*.py\")")))
                      (max_results . ((type . "integer")
                                      (description . "Maximum number of matching lines to return (default 100, max 500)"))))))
     (lambda (args)
       (let* ((pattern (alist-get 'pattern args))
              (directory (expand-file-name (alist-get 'directory args)))
              (glob (alist-get 'glob args))
              (max-results (max 1 (min (or (alist-get 'max_results args) 100) 500)))
              (file-regexp (if glob
                              (wildcard-to-regexp glob)
                            ""))
              (results nil)
              (count 0))
         (if (not (file-directory-p directory))
             (format "Error: Directory %s not found" directory)
           (let ((files (directory-files-recursively directory file-regexp)))
             ;; Filter out hidden directories (.git, etc.)
             (setq files (seq-remove
                          (lambda (f)
                            (string-match-p
                             "/\\."
                             (file-relative-name f directory)))
                          files))
             (catch 'max-reached
               (dolist (file files)
                 (when (and (file-readable-p file)
                            (not (file-directory-p file)))
                   (condition-case nil
                       (with-temp-buffer
                         (insert-file-contents file nil nil 1048576) ; 1MB limit per file
                         (goto-char (point-min))
                         ;; Skip binary files (null bytes in first 512 chars)
                         (unless (re-search-forward "\0" (min (point-max) 512) t)
                           (goto-char (point-min))
                           (while (re-search-forward pattern nil t)
                             (push (format "%s:%d: %s"
                                           (file-relative-name file directory)
                                           (line-number-at-pos)
                                           (string-trim (thing-at-point 'line t)))
                                   results)
                             (setq count (1+ count))
                             (when (>= count max-results)
                               (throw 'max-reached nil))
                             (forward-line 1))))
                     (error nil)))))
           (if results
               (concat (mapconcat #'identity (nreverse results) "\n")
                       (if (>= count max-results)
                           (format "\n\n[Results truncated at %d matches]" max-results)
                         ""))
             "No matches found")))))
     t) ; safe

    ;; propose_file_changes - Provide full new file content and review via Emacs ediff
    (ollama-buddy-tools-register
     'propose_file_changes
     "Propose changes to a file by providing the COMPLETE new file content with all modifications applied. Opens Emacs ediff so the user can review every change interactively and selectively copy hunks across. IMPORTANT: You MUST provide the ENTIRE file contents including all unchanged lines — not just the changed portion. Read the file first if needed, then return the full file with your modifications applied. Providing only a snippet will result in a poor diff experience."
     '((type . "object")
       (required . ["file_path" "new_content"])
       (properties . ((file_path . ((type . "string")
                                    (description . "Absolute path to the existing file to modify.")))
                      (new_content . ((type . "string")
                                      (description . "The COMPLETE new content for the entire file, with all proposed changes applied. This MUST be the full file from first line to last line, not a fragment, snippet, or diff. Include every unchanged line as-is."))))))
     (lambda (args)
       (let* ((file-path (alist-get 'file_path args))
              (new-content (alist-get 'new_content args)))
         (cond
          ((not (stringp file-path))
           "Error: file_path is required (must be a string).")
          ((not (stringp new-content))
           "Error: new_content is required (must be a string).")
          ((not (file-exists-p file-path))
           (format "Error: file not found: %s" file-path))
          (t
           (let* ((original-content (with-temp-buffer
                                      (insert-file-contents file-path)
                                      (buffer-string)))
                  (original-lines (length (split-string original-content "\n")))
                  (new-lines (length (split-string new-content "\n")))
                  ;; Detect fragment: new content is less than 40% of original
                  (fragment-p (and (> original-lines 10)
                                  (< new-lines (/ (* original-lines 40) 100))))
                  (final-content
                   (if fragment-p
                       (ollama-buddy-tools--merge-fragment
                        original-content new-content)
                     new-content))
                  (ext (file-name-extension file-path t))
                  (proposed-tmp (make-temp-file "ob-proposed-" nil ext)))
             (with-temp-file proposed-tmp
               (insert final-content))
             (require 'ediff)
             (let* ((proposed-buf (find-file-noselect proposed-tmp))
                    (original-buf (find-file-noselect file-path))
                    (ediff-ok nil))
               (unwind-protect
                   (progn
                     (save-window-excursion
                       (ediff-buffers proposed-buf original-buf))
                     (setq ediff-ok t)
                     ;; Clean up proposed buffer and temp file when ediff quits
                     (let ((pb proposed-buf)
                           (pt proposed-tmp))
                       (add-hook 'ediff-quit-hook
                                 (lambda ()
                                   (when (buffer-live-p pb)
                                     (kill-buffer pb))
                                   (when (file-exists-p pt)
                                     (delete-file pt)))
                                 nil t)))
                 (unless ediff-ok
                   (when (buffer-live-p proposed-buf)
                     (kill-buffer proposed-buf))
                   (when (file-exists-p proposed-tmp)
                     (delete-file proposed-tmp)))))
             (format "Ediff session created for %s%s (left: proposed, right: original). Inform the user they can open it at any time with M-x ediff-show-registry — for example in a new frame or tab. In ediff: 'n'/'p' navigate differences, 'b' copies a hunk left-to-right into the original, then save the file."
                     file-path
                     (if fragment-p
                         " [fragment detected — merged into full file]"
                       "")))))))
     nil  ; not safe — launches ediff UI
     t)   ; terminal — stop LLM auto-continuation after ediff session created

    ;; get_project_info - Get current project information via project.el
    (ollama-buddy-tools-register
     'get_project_info
     "Get information about the current Emacs project using project.el. Returns the project root directory, project name, and top-level files/directories. Useful for understanding what project the user is working in."
     `((type . "object")
       (properties . ,(make-hash-table)))
     (lambda (_args)
       (if-let ((proj (project-current)))
           (let* ((root (project-root proj))
                  (name (file-name-nondirectory (directory-file-name root)))
                  (entries (directory-files root nil "^[^.]"))
                  (dirs (seq-filter
                         (lambda (f) (file-directory-p (expand-file-name f root)))
                         entries))
                  (files (seq-remove
                          (lambda (f) (file-directory-p (expand-file-name f root)))
                          entries)))
             (format "Project: %s\nRoot: %s\nDirectories: %s\nFiles: %s"
                     name root
                     (if dirs (mapconcat #'identity dirs ", ") "(none)")
                     (if files (mapconcat #'identity files ", ") "(none)")))
         "No project found for the current buffer. Try opening a file inside a project first."))
     t) ; safe

    ;; get_region - Get the active region/selection
    (ollama-buddy-tools-register
     'get_region
     "Get the currently selected text (active region) in Emacs. Returns the selected text along with the buffer name and line range. Useful when the user says \"explain this\", \"refactor this\", etc."
     `((type . "object")
       (properties . ,(make-hash-table)))
     (lambda (_args)
       (let ((buf (or (and (bound-and-true-p ollama-buddy--caller-buffer)
                           (buffer-live-p ollama-buddy--caller-buffer)
                           ollama-buddy--caller-buffer)
                      (window-buffer (selected-window)))))
         (with-current-buffer buf
           (if (use-region-p)
               (let ((start (region-beginning))
                     (end (region-end)))
                 (format "Buffer: %s\nLines %d-%d:\n%s"
                         (buffer-name)
                         (line-number-at-pos start)
                         (line-number-at-pos end)
                         (buffer-substring-no-properties start end)))
             "No active region/selection"))))
     t) ; safe

    ;; get_buffer_info - Get buffer metadata without full contents
    (ollama-buddy-tools-register
     'get_buffer_info
     "Get metadata about an Emacs buffer without returning its full contents. Returns: file path, major mode, point position, line count, modified state, read-only state, and narrowing info. Cheaper than get_buffer_content when you just need context about what the user is working on."
     '((type . "object")
       (properties . ((buffer . ((type . "string")
                                 (description . "Buffer name. If omitted, uses the buffer the user was in when they started the chat."))))))
     (lambda (args)
       (let* ((buffer-name (alist-get 'buffer args))
              (buf (or (and buffer-name (get-buffer buffer-name))
                       (and (bound-and-true-p ollama-buddy--caller-buffer)
                            (buffer-live-p ollama-buddy--caller-buffer)
                            ollama-buddy--caller-buffer)
                       (current-buffer))))
         (if (buffer-live-p buf)
             (with-current-buffer buf
               (format "Buffer: %s\nFile: %s\nMajor mode: %s\nPoint: %d (line %d, column %d)\nLines: %d\nSize: %d bytes\nModified: %s\nRead-only: %s\nNarrowed: %s"
                       (buffer-name)
                       (or (buffer-file-name) "(no file)")
                       major-mode
                       (point) (line-number-at-pos) (current-column)
                       (count-lines (point-min) (point-max))
                       (buffer-size)
                       (if (buffer-modified-p) "yes" "no")
                       (if buffer-read-only "yes" "no")
                       (if (buffer-narrowed-p)
                           (format "yes (lines %d-%d of %d)"
                                   (line-number-at-pos (point-min))
                                   (line-number-at-pos (point-max))
                                   (count-lines 1 (1+ (buffer-size))))
                         "no")))
           (format "Error: Buffer %s not found" (or buffer-name "(default)")))))
     t) ; safe

    ;; describe_symbol - Look up Emacs documentation for a symbol
    (ollama-buddy-tools-register
     'describe_symbol
     "Look up the documentation for an Emacs Lisp symbol (function or variable). Uses Emacs's built-in describe-function and describe-variable to return the docstring, signature, and source location."
     '((type . "object")
       (required . ["symbol"])
       (properties . ((symbol . ((type . "string")
                                 (description . "The Emacs Lisp symbol name to look up (e.g. \"mapcar\", \"buffer-file-name\")"))))))
     (lambda (args)
       (let* ((sym-name (alist-get 'symbol args))
              (sym (intern-soft sym-name)))
         (if (not sym)
             (format "Symbol '%s' not found" sym-name)
           (let ((parts nil))
             (when (fboundp sym)
               (push (format "=== Function: %s ===\n\nSignature: %s\n\n%s"
                             sym-name
                             (or (elisp-get-fnsym-args-string sym) "(unknown)")
                             (or (documentation sym t) "(no documentation)"))
                     parts))
             (when (boundp sym)
               (let ((val (symbol-value sym)))
                 (push (format "=== Variable: %s ===\n\nValue: %s\n\n%s"
                               sym-name
                               (let ((printed (format "%S" val)))
                                 (if (> (length printed) 200)
                                     (concat (substring printed 0 200) "...")
                                   printed))
                               (or (documentation-property sym 'variable-documentation t)
                                   "(no documentation)"))
                       parts)))
             (when (facep sym)
               (push (format "=== Face: %s ===\n\n%s"
                             sym-name
                             (or (documentation-property sym 'face-documentation t)
                                 "(no documentation)"))
                     parts))
             (if parts
                 (mapconcat #'identity (nreverse parts) "\n\n")
               (format "Symbol '%s' exists but is not a function, variable, or face" sym-name))))))
     t) ; safe

    ;; eval_elisp - Evaluate an Emacs Lisp expression
    (ollama-buddy-tools-register
     'eval_elisp
     "Evaluate an Emacs Lisp expression and return the result. This is a powerful tool that can do almost anything in Emacs: navigate buffers, modify settings, run commands, query state, etc. The expression is evaluated in the context of the current Emacs session."
     '((type . "object")
       (required . ["expression"])
       (properties . ((expression . ((type . "string")
                                     (description . "Emacs Lisp expression to evaluate (e.g. \"(+ 1 2)\", \"(buffer-list)\", \"(message \\\"hello\\\")\" )"))))))
     (lambda (args)
       (let ((expr-str (alist-get 'expression args)))
         (condition-case err
             (let* ((expr (car (read-from-string expr-str)))
                    (result (eval expr t)))
               (format "%S" result))
           (error (format "Error: %s" (error-message-string err))))))
     nil) ; not safe

    ;; web_search - Search the web for current information
    (when (featurep 'ollama-buddy-web-search)
      (ollama-buddy-tools-register
       'web_search
       "Search the web for current information on a topic. Returns titles and snippets from web results. Use this when the user asks about recent events, current facts, or anything that may require up-to-date information."
       '((type . "object")
         (required . ["query"])
         (properties . ((query . ((type . "string")
                                  (description . "The search query to look up on the web"))))))
       (lambda (args)
         (let* ((query (alist-get 'query args))
                (result (ollama-buddy-web-search--fetch-sync query)))
           (if (and result (car result))
               ;; Format as plain text for LLM consumption — org markup in tool
               ;; results confuses the model and API content fields may be empty.
               (let ((results (cdr result)))
                 (concat
                  (format "Web search results for: \"%s\"\n\n" query)
                  (mapconcat
                   (lambda (r)
                     (let ((title (or (alist-get 'title r) "Untitled"))
                           (url (or (alist-get 'url r) (alist-get 'link r) ""))
                           (snippet (or (alist-get 'content r)
                                        (alist-get 'snippet r)
                                        (alist-get 'description r)
                                        (alist-get 'text r)
                                        (alist-get 'body r)
                                        "")))
                       (format "Title: %s\nURL: %s\n%s" title url snippet)))
                   results
                   "\n\n")))
             (format "Web search failed: %s" (or (cdr result) "unknown error")))))
       t))  ; safe

    ;; rag_search - Search local RAG indexes for relevant documents
    (when (featurep 'ollama-buddy-rag)
      (ollama-buddy-tools-register
       'rag_search
       "Search local document indexes (RAG) for relevant content. Returns matching text chunks from previously indexed directories. Use this when the user asks about local codebases, documentation, or files that have been indexed."
       '((type . "object")
         (required . ["query"])
         (properties . ((query . ((type . "string")
                                  (description . "The search query to find relevant document chunks")))
                        (index . ((type . "string")
                                  (description . "Name of the RAG index to search. If omitted, searches all available indexes."))))))
       (lambda (args)
         (let* ((query (alist-get 'query args))
                (index-name (alist-get 'index args))
                (available (ollama-buddy-rag--list-index-names)))
           (if (null available)
               "No RAG indexes found. Index a directory first with M-x ollama-buddy-rag-index-directory."
             (let ((indexes-to-search
                    (if (and index-name (member index-name available))
                        (list index-name)
                      available))
                   (all-results nil))
               ;; Get query embedding synchronously
               (let ((embedding (ollama-buddy-rag--get-embedding-sync query)))
                 (if (null embedding)
                     "Error: Failed to generate embedding for query."
                   ;; Search each index
                   (dolist (idx indexes-to-search)
                     (when-let ((index (ollama-buddy-rag--load-index idx)))
                       (let* ((chunks (plist-get index :chunks))
                              (results (ollama-buddy-rag--search-chunks
                                        embedding chunks
                                        ollama-buddy-rag-top-k
                                        ollama-buddy-rag-similarity-threshold)))
                         (when results
                           (push (ollama-buddy-rag--format-results-for-context
                                  results query idx)
                                 all-results)))))
                   (if all-results
                       (mapconcat #'identity (nreverse all-results) "\n\n")
                     (format "No results found for \"%s\" in %s."
                             query
                             (mapconcat #'identity indexes-to-search ", ")))))))))
       t))  ; safe

    (message "Initialized %d built-in tools" (hash-table-count ollama-buddy-tools--registry))))

;;; Interactive Commands

(defun ollama-buddy-tools-toggle ()
  "Toggle tool calling on/off.
When enabling, warns if the current model does not support tool calling."
  (interactive)
  (let ((enabling (not ollama-buddy-tools-enabled))
        (current-model (bound-and-true-p ollama-buddy--current-model)))
    (if (and enabling
             current-model
             (fboundp 'ollama-buddy--model-supports-tools)
             (not (ollama-buddy--model-supports-tools current-model)))
        (message "Model '%s' does not support tool calling - tools not enabled. Switch to a tool-capable model first."
                 current-model)
      (setq ollama-buddy-tools-enabled enabling)
      (when (fboundp 'ollama-buddy--update-status)
        (ollama-buddy--update-status
         (if ollama-buddy-tools-enabled "Tools enabled" "Tools disabled"))))))

(defun ollama-buddy-tools-toggle-auto-execute ()
  "Toggle auto-execution of tool calls (skip confirmation prompts)."
  (interactive)
  (setq ollama-buddy-tools-auto-execute (not ollama-buddy-tools-auto-execute))
  (when (fboundp 'ollama-buddy--update-status)
    (ollama-buddy--update-status
     (if ollama-buddy-tools-auto-execute "Tool auto-execute enabled" "Tool auto-execute disabled"))))

(defun ollama-buddy-tools-toggle-safe-mode ()
  "Toggle safe mode for tool execution."
  (interactive)
  (setq ollama-buddy-tools-safe-mode (not ollama-buddy-tools-safe-mode))
  (message "Tool safe mode %s" (if ollama-buddy-tools-safe-mode "enabled" "disabled")))

(defun ollama-buddy-tools-toggle-unguarded ()
  "Toggle unguarded mode — bypass ALL safety prompts for tool execution.
When enabled, no confirmation is asked for any tool (including unsafe
ones like `execute_shell'), large-result warnings are suppressed, and
safe-mode is overridden.  The header line turns red as a warning."
  (interactive)
  (setq ollama-buddy-tools-unguarded (not ollama-buddy-tools-unguarded))
  (when (fboundp 'ollama-buddy--update-unguarded-header-face)
    (ollama-buddy--update-unguarded-header-face))
  (when (fboundp 'ollama-buddy--update-status)
    (ollama-buddy--update-status
     (if ollama-buddy-tools-unguarded
         "UNGUARDED mode enabled — all safety prompts bypassed"
       "Unguarded mode disabled"))))

(defun ollama-buddy-tools-info ()
  "Display information about registered tools."
  (interactive)
  (let ((tools (ollama-buddy-tools-list))
        (buf (get-buffer-create "*Ollama Buddy Tools*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "#+title: Ollama Buddy Tools\n\n")
        (insert "* Status\n\n")
        (insert (format "- *Enabled:*      %s\n" (if ollama-buddy-tools-enabled "Yes" "No")))
        (insert (format "- *Safe Mode:*    %s\n" (if ollama-buddy-tools-safe-mode "On" "Off")))
        (insert (format "- *Auto Execute:* %s\n" (if ollama-buddy-tools-auto-execute "Yes" "No")))
        (insert (format "- *Unguarded:*    %s\n" (if ollama-buddy-tools-unguarded "YES — all prompts bypassed!" "No")))
        (insert (format "- *Total Tools:*  %d\n\n" (length tools)))
        (insert "* Registered Tools\n\n")
        (dolist (name tools)
          (let* ((spec (ollama-buddy-tools-get name))
                 (desc (plist-get spec :description))
                 (safe (plist-get spec :safe))
                 (params (plist-get spec :parameters)))
            (insert (format "** %s  [%s]\n\n" name (if safe "safe" "unsafe")))
            (insert (format "%s\n\n" (or desc "")))
            (when params
              (let* ((props-raw (if (hash-table-p params)
                                    (gethash "properties" params)
                                  (alist-get 'properties params)))
                     (props (cond
                             ((hash-table-p props-raw)
                              (let (pairs)
                                (maphash (lambda (k v) (push (cons (intern k) v) pairs))
                                         props-raw)
                                pairs))
                             (t props-raw))))
                (dolist (prop props)
                  (let* ((prop-name (car prop))
                         (prop-spec (cdr prop))
                         (ptype (if (hash-table-p prop-spec)
                                    (gethash "type" prop-spec)
                                  (alist-get 'type prop-spec)))
                         (pdesc (if (hash-table-p prop-spec)
                                    (gethash "description" prop-spec)
                                  (alist-get 'description prop-spec))))
                    (insert (format "- *%s* (%s) :: %s\n"
                                    prop-name
                                    (or ptype "any")
                                    (or pdesc ""))))))
              (insert "\n"))))
        (org-mode)
        (setq-local org-hide-emphasis-markers t)
        (setq-local org-hide-leading-stars t)
        (goto-char (point-min))
        (view-mode 1)))
    (display-buffer buf)))

;;; Setup

;;;###autoload
(defun ollama-buddy-tools-setup ()
  "Initialize the ollama-buddy-tools module."
  (interactive)
  (ollama-buddy-tools--init-builtin)
  (message "Ollama Buddy Tools initialized"))

;; Auto-initialize on load if built-in tools are enabled
(when ollama-buddy-tools-builtin-enabled
  (ollama-buddy-tools--init-builtin))

(provide 'ollama-buddy-tools)
;;; ollama-buddy-tools.el ends here
