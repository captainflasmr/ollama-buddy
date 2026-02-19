;;; ollama-buddy-tools.el --- Tool calling support for Ollama Buddy -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.1"))
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
(require 'ollama-buddy-core)

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

(defcustom ollama-buddy-tools-auto-execute t
  "Whether to automatically execute tool calls without confirmation.
When nil, prompt the user before executing each tool call."
  :type 'boolean
  :group 'ollama-buddy-tools)

(defcustom ollama-buddy-tools-max-iterations 10
  "Maximum number of tool-call iterations per user prompt.
This prevents infinite loops if the LLM keeps calling tools."
  :type 'integer
  :group 'ollama-buddy-tools)

(defcustom ollama-buddy-tools-safe-mode t
  "When non-nil, restrict tools to safe read-only operations.
Disables tools that can modify files or execute arbitrary commands."
  :type 'boolean
  :group 'ollama-buddy-tools)

(defcustom ollama-buddy-tools-builtin-enabled t
  "Whether to enable built-in tools.
When nil, only custom user-registered tools are available."
  :type 'boolean
  :group 'ollama-buddy-tools)

;;; Internal Variables

(defvar ollama-buddy-tools--registry (make-hash-table :test 'equal)
  "Hash table storing registered tools.
Keys are tool names (strings), values are plists with:
  :description - Tool description
  :parameters  - JSON schema for parameters
  :function    - Elisp function to execute
  :safe        - Whether tool is safe (read-only)")

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

;;; Tool Registration

(defun ollama-buddy-tools-register (name description parameters function &optional safe)
  "Register a tool for use with Ollama Buddy.

NAME is the tool name (symbol or string).
DESCRIPTION is a human-readable description of what the tool does.
PARAMETERS is a JSON schema object describing the tool's parameters.
FUNCTION is the Elisp function to execute when the tool is called.
  It receives an alist of arguments and should return a string result.
SAFE indicates if the tool is safe (read-only) - defaults to nil.

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
                   :safe (or safe nil))
             ollama-buddy-tools--registry)
    (message "Registered tool: %s" name-str)))

(defun ollama-buddy-tools-unregister (name)
  "Unregister tool NAME from the registry."
  (let ((name-str (if (symbolp name) (symbol-name name) name)))
    (remhash name-str ollama-buddy-tools--registry)
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
  (message "Cleared all registered tools"))

;;; Tool Schema Generation

(defun ollama-buddy-tools--generate-schema ()
  "Generate the tools schema for the Ollama API request.
Returns a vector of tool definitions in the format expected by Ollama."
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
    (when tools-list
      (vconcat (nreverse tools-list)))))

;;; Tool Execution

(defun ollama-buddy-tools--format-args-for-display (arguments)
  "Format ARGUMENTS alist for display, truncating long string values."
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
   arguments
   ", "))

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
        ;; Safety check
        (when (and ollama-buddy-tools-safe-mode
                   (not (plist-get spec :safe)))
          (error "Tool %s is not safe for execution in safe mode" name-str))
        ;; Confirmation check
        (when (and (not ollama-buddy-tools-auto-execute)
                   (not (yes-or-no-p (format "Execute tool %s(%s)? "
                                             name-str
                                             (ollama-buddy-tools--format-args-for-display arguments)))))
          (error "Tool execution cancelled by user"))
        ;; Execute the function
        (let ((result (funcall func arguments)))
          (if (stringp result)
              result
            (format "%S" result))))
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
    
    ;; propose_file_changes - Provide full new file content and review via Emacs ediff
    (ollama-buddy-tools-register
     'propose_file_changes
     "Propose changes to a file by providing the COMPLETE new file content with all modifications applied. Opens Emacs ediff so the user can review every change interactively and selectively copy hunks across. Use this whenever you want to suggest edits to an existing file — provide the full updated file, not a diff or patch."
     '((type . "object")
       (required . ["file_path" "new_content"])
       (properties . ((file_path . ((type . "string")
                                    (description . "Absolute path to the existing file to modify.")))
                      (new_content . ((type . "string")
                                      (description . "The complete new content for the file, with all proposed changes already applied. Must be the entire file, not just a fragment or diff."))))))
     (lambda (args)
       (let* ((file-path (alist-get 'file_path args))
              (new-content (alist-get 'new_content args)))
         (cond
          ((not file-path)
           "Error: file_path is required.")
          ((not new-content)
           "Error: new_content is required.")
          ((not (file-exists-p file-path))
           (format "Error: file not found: %s" file-path))
          (t
           (let* ((ext (file-name-extension file-path t))
                  (proposed-tmp (make-temp-file "ob-proposed-" nil ext)))
             (with-temp-file proposed-tmp
               (insert new-content))
             (require 'ediff)
             (let* ((proposed-buf (find-file-noselect proposed-tmp))
                    (original-buf (find-file-noselect file-path)))
               ;; Pause tool continuation and mark session as paused.
               ;; ollama-buddy.el will insert *** PAUSED and refresh the header.
               (setq ollama-buddy-tools--pause-continuation t)
               (setq ollama-buddy-tools--session-paused t)
               ;; Use the startup hook to: (a) attach the quit notification,
               ;; (b) schedule ediff-next-difference via an idle timer so
               ;; ediff properly repositions both windows (direct window
               ;; manipulation fights ediff's own recenter calls).
               (let ((ediff-startup-hook
                      (cons (lambda ()
                              (let ((ctrl-buf (current-buffer)))
                                ;; Clear paused state on quit
                                (add-hook 'ediff-quit-hook
                                          (lambda ()
                                            (setq ollama-buddy-tools--session-paused nil)
                                            ;; Trigger tool continuation so the LLM can
                                            ;; respond to the tool results (summary, more
                                            ;; tool calls, etc.) exactly as in the normal
                                            ;; tool flow.  Use an idle timer so ediff has
                                            ;; fully cleaned up before we start a request.
                                            (run-with-idle-timer
                                             0 nil
                                             (lambda ()
                                               (let ((model (bound-and-true-p ollama-buddy--current-model))
                                                     (chat-buf (and (boundp 'ollama-buddy--chat-buffer)
                                                                    (get-buffer ollama-buddy--chat-buffer))))
                                                 (when (and model chat-buf (fboundp 'ollama-buddy--send))
                                                   (with-current-buffer chat-buf
                                                     (ollama-buddy--update-status "Resuming...")
                                                     (ollama-buddy--send nil model t)))))))
                                          nil t)
                                ;; Navigate to first difference — this is what
                                ;; properly repositions both windows in ediff.
                                (run-with-idle-timer
                                 0 nil
                                 (lambda ()
                                   (when (buffer-live-p ctrl-buf)
                                     (let ((ctrl-win (get-buffer-window ctrl-buf t)))
                                       (when ctrl-win
                                         ;; Inject a synthetic "n" keypress into
                                         ;; the command loop so ediff-next-difference
                                         ;; runs with full redisplay, matching what
                                         ;; happens when the user presses n manually.
                                         (select-window ctrl-win)
                                         (setq unread-command-events
                                               (listify-key-sequence (kbd "n"))))))))))
                            ediff-startup-hook)))
                 (ediff-buffers proposed-buf original-buf)))
             (format "Opened ediff for %s\nLeft: proposed changes  Right: original\nUse 'n'/'p' to navigate hunks, 'b' to apply a hunk (left to right), then save the original file when done.\nTool calling is paused until you send a new chat message."
                     file-path))))))
     nil) ; not safe — launches ediff UI

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
      ;; Refresh header line to update tool indicator
      (when (fboundp 'ollama-buddy--update-status)
        (ollama-buddy--update-status (or (bound-and-true-p ollama-buddy--status) "")))
      (message "Tool calling %s" (if ollama-buddy-tools-enabled "enabled" "disabled")))))

(defun ollama-buddy-tools-toggle-safe-mode ()
  "Toggle safe mode for tool execution."
  (interactive)
  (setq ollama-buddy-tools-safe-mode (not ollama-buddy-tools-safe-mode))
  (message "Tool safe mode %s" (if ollama-buddy-tools-safe-mode "enabled" "disabled")))

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
        (insert (format "- *Total Tools:*  %d\n\n" (length tools)))
        (insert "* Registered Tools\n\n")
        (dolist (name tools)
          (let* ((spec (ollama-buddy-tools-get name))
                 (desc (plist-get spec :description))
                 (safe (plist-get spec :safe))
                 (params (plist-get spec :parameters)))
            (insert (format "** %s  [%s]\n\n" name (if safe "safe" "unsafe")))
            (insert (format "   %s\n\n" (or desc "")))
            (when params
              (let ((props (alist-get 'properties params)))
                (dolist (prop props)
                  (let ((prop-name (car prop))
                        (prop-spec (cdr prop)))
                    (insert (format "   - *%s* (%s) :: %s\n"
                                    prop-name
                                    (or (alist-get 'type prop-spec) "any")
                                    (or (alist-get 'description prop-spec) ""))))))
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
