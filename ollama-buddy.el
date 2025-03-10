;;; ollama-buddy.el --- Ollama Buddy: Your Friendly AI Assistant -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.6.1
;; Package-Requires: ((emacs "26.1"))
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
;; Ollama Buddy is an Emacs package that provides a friendly AI assistant
;; for various tasks such as code refactoring, generating commit messages,
;; dictionary lookups, and more.  It interacts with the Ollama server to
;; perform these tasks.
;;
;;; Quick Start
;;
;; (use-package ollama-buddy
;;    :load-path "path/to/ollama-buddy"
;;    :bind ("C-c o" . ollama-buddy-menu)
;;    :custom ollama-buddy-default-model "llama:latest")
;;
;; OR
;;
;; (add-to-list 'load-path "path/to/ollama-buddy")
;; (require 'ollama-buddy)
;; (global-set-key (kbd "C-c o") #'ollama-buddy-menu)
;; (setq ollama-buddy-default-model "llama:latest")
;;
;; OR (when added to MELPA)
;;
;; (use-package ollama-buddy
;;    :ensure t
;;    :bind ("C-c o" . ollama-buddy-menu)
;;    :custom ollama-buddy-default-model "llama:latest")
;;
;;; Usage
;;
;; M-x ollama-buddy-menu
;;
;; OR
;;
;; C-c o
;;
;;; Code:

(require 'json)
(require 'subr-x)
(require 'url)
(require 'cl-lib)
(require 'dired)
(require 'org)

(defgroup ollama-buddy nil
  "Customization group for Ollama Buddy."
  :group 'applications
  :prefix "ollama-buddy-")

(defcustom ollama-buddy-command-definitions
  '(    
    ;; General Commands
    (open-chat
     :key ?o
     :description "Open chat buffer"
     :action ollama-buddy--open-chat)

    (help
     :key ?h
     :description "Help assistant"
     :action ollama-buddy--menu-help-assistant)
    
    (swap-model
     :key ?m
     :description "Swap model"
     :action ollama-buddy--swap-model)
    
    (show-models
     :key ?v
     :description "View model status"
     :action ollama-buddy-show-model-status)

    (send-region
     :key ?l
     :description "Send region"
     :action (lambda () (ollama-buddy--send-with-command 'send-region)))

    (kill-request
     :key ?x
     :description "Kill request"
     :action (lambda ()
               (delete-process ollama-buddy--active-process)))

    (switch-role
     :key ?R
     :description "Switch roles"
     :model nil
     :action ollama-buddy-roles-switch-role)

    (create-role
     :key ?N
     :description "Create new role"
     :model nil
     :action ollama-buddy-role-creator-create-new-role)

    (open-roles-directory
     :key ?D
     :description "Open roles directory"
     :model nil
     :action ollama-buddy-roles-open-directory)

    ;; Custom commands
    (refactor-code
     :key ?r
     :description "Refactor code"
     :prompt "refactor the following code:"
     :action (lambda () (ollama-buddy--send-with-command 'refactor-code)))
    
    (git-commit
     :key ?g
     :description "Git commit message"
     :prompt "write a concise git commit message for the following:"
     :action (lambda () (ollama-buddy--send-with-command 'git-commit)))
    
    (describe-code
     :key ?c
     :description "Describe code"
     :prompt "describe the following code:"
     :action (lambda () (ollama-buddy--send-with-command 'describe-code)))
    
    (dictionary-lookup
     :key ?d
     :description "Dictionary Lookup"
     :prompt "For the following word provide a typical dictionary definition:"
     :action (lambda () (ollama-buddy--send-with-command 'dictionary-lookup)))
    
    (synonym
     :key ?n
     :description "Word synonym"
     :prompt "list synonyms for word:"
     :action (lambda () (ollama-buddy--send-with-command 'synonym)))
    
    (proofread
     :key ?p
     :description "Proofread text"
     :prompt "proofread the following:"
     :action (lambda () (ollama-buddy--send-with-command 'proofread)))
    
    (make-concise
     :key ?z
     :description "Make concise"
     :prompt "reduce wordiness while preserving meaning:"
     :action (lambda () (ollama-buddy--send-with-command 'make-concise)))

    ;; System Commands
    (custom-prompt
     :key ?e
     :description "Custom prompt"
     :action ollama-buddy--menu-custom-prompt)
    
    (minibuffer-prompt
     :key ?i
     :description "Minibuffer Prompt"
     :action ollama-buddy--menu-minibuffer-prompt)
    
    (toggle-colors
     :key ?C
     :description "Toggle Colors"
     :action ollama-buddy-toggle-model-colors)

    (token-stats
     :key ?t
     :description "Token Usage Stats"
     :action ollama-buddy-display-token-stats)

    (toggle-history
     :key ?H
     :description "Toggle conversation history"
     :action ollama-buddy-toggle-history)

    (clear-history
     :key ?X
     :description "Clear conversation history"
     :action (lambda () (ollama-buddy-clear-history 1)))

    (show-history
     :key ?V
     :description "View conversation history"
     :action (lambda () (ollama-buddy--display-history 1)))

    (new-session
     :key ?E
     :description "New session"
     :action ollama-buddy-sessions-new)

    (load-session
     :key ?L
     :description "Load session"
     :action ollama-buddy-sessions-load)

    (save-session
     :key ?S
     :description "Save session"
     :action ollama-buddy-sessions-save)

    (list-sessions
     :key ?Y
     :description "List sessions"
     :action ollama-buddy-sessions-list)

    (delete-sessions
     :key ?K
     :description "Delete session"
     :action ollama-buddy-sessions-delete)
    
    (quit
     :key ?q
     :description "Quit"
     :action (lambda () (message "Quit Ollama Shell menu.")))
    )
  "Comprehensive command definitions for Ollama Buddy.
Each command is defined with:
  :key - Character for menu selection
  :description - String describing the action
  :model - Specific Ollama model to use (nil means use default)
  :prompt - Optional system prompt
  :action - Function to execute"
  :type '(repeat
          (list :tag "Command Definition"
                (symbol :tag "Command Name")
                (plist :inline t
                       :options
                       ((:key (character :tag "Menu Key Character"))
                        (:description (string :tag "Command Description"))
                        (:model (choice :tag "Specific Model"
                                        (const :tag "Use Default" nil)
                                        (string :tag "Model Name")))
                        (:prompt (string :tag "Static Prompt Text"))
                        (:action (choice :tag "Action"
                                         (function :tag "Existing Function")
                                         (sexp :tag "Lambda Expression")))))))
  :group 'ollama-buddy)

(defcustom ollama-buddy-convert-markdown-to-org t
  "Whether to automatically convert markdown to org-mode format in responses."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-sessions-directory
  (expand-file-name "ollama-buddy-sessions" user-emacs-directory)
  "Directory containing ollama-buddy session files."
  :type 'directory
  :group 'ollama-buddy)

(defcustom ollama-buddy-enable-model-colors t
  "Whether to show model colors."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-enable-background-monitor nil
  "Whether to enable background monitoring of Ollama connection.
When nil, status checks only occur during user interactions."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-host "localhost"
  "Host where Ollama server is running."
  :type 'string
  :group 'ollama-buddy)

(defcustom ollama-buddy-port 11434
  "Port where Ollama server is running."
  :type 'integer
  :group 'ollama-buddy)

(defcustom ollama-buddy-menu-columns 5
  "Number of columns to display in the Ollama Buddy menu."
  :type 'integer
  :group 'ollama-buddy)

(defcustom ollama-buddy-default-model nil
  "Default Ollama model to use."
  :type 'string
  :group 'ollama-buddy)

(defcustom ollama-buddy-roles-directory
  (expand-file-name "ollama-buddy-presets" user-emacs-directory)
  "Directory containing ollama-buddy role preset files."
  :type 'directory
  :group 'ollama-buddy)

(defcustom ollama-buddy-connection-check-interval 5
  "Interval in seconds to check Ollama connection status."
  :type 'integer
  :group 'ollama-buddy)

(defcustom ollama-buddy-history-enabled t
  "Whether to use conversation history in Ollama requests."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-max-history-length 10
  "Maximum number of message pairs to keep in conversation history."
  :type 'integer
  :group 'ollama-buddy)

(defcustom ollama-buddy-show-history-indicator t
  "Whether to show the history indicator in the header line."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-default-temperature 0.7
  "Default temperature setting for Ollama requests.
Lower values (0.0-0.5) make responses more deterministic and focused.
Higher values (0.7-1.0+) increase randomness and creativity."
  :type 'float
  :group 'ollama-buddy)

(defvar ollama-buddy--current-request-temporary-model nil
  "For the current request don't make current model permanent.")

(defvar ollama-buddy--response-start-position nil
  "Marker for the start position of the current response.")

(defvar ollama-buddy--current-response nil
  "The current response text being accumulated.")

(defvar-local ollama-buddy--response-start-position nil
  "Buffer-local marker for the start position of the current response.")

(defvar ollama-buddy--current-prompt nil
  "The current prompt.")

(defvar ollama-buddy--current-temperature ollama-buddy-default-temperature
  "The currently active temperature value.")

(defvar ollama-buddy--current-session nil
  "Name of the currently active session, or nil if none.")

(defvar ollama-buddy--conversation-history-by-model (make-hash-table :test 'equal)
  "Hash table mapping model names to their conversation histories.")

(defvar ollama-buddy--conversation-history nil
  "Current model's conversation history (alias for backward compatibility).")

(defvar ollama-buddy--token-usage-history nil
  "History of token usage for ollama-buddy interactions.")

(defvar ollama-buddy--current-token-count 0
  "Counter for tokens in the current response.")

(defvar ollama-buddy--current-token-start-time nil
  "Timestamp when the current response started.")

(defcustom ollama-buddy-display-token-stats nil
  "Whether to display token usage statistics in responses."
  :type 'boolean
  :group 'ollama-buddy)

(defvar ollama-buddy--token-update-interval 0.5
  "How often to update the token rate display, in seconds.")

(defvar ollama-buddy--token-update-timer nil
  "Timer for updating token rate display.")

(defvar ollama-buddy--last-token-count 0
  "Token count at last update interval.")

(defvar ollama-buddy--last-update-time nil
  "Timestamp of last token rate update.")

(defvar ollama-buddy--prompt-history nil
  "History of prompts used in ollama-buddy.")

(defvar ollama-buddy--last-status-check nil
  "Timestamp of last Ollama status check.")

(defvar ollama-buddy--status-cache nil
  "Cached status of Ollama connection.")

(defvar ollama-buddy--status-cache-ttl 5
  "Time in seconds before status cache expires.")

(defvar ollama-buddy--current-model nil
  "Timer for checking Ollama connection status.")

(defvar ollama-buddy--connection-timer nil
  "Timer for checking Ollama connection status.")

(defvar ollama-buddy--chat-buffer "*Ollama Buddy Chat*"
  "Chat interaction buffer.")

(defvar ollama-buddy--active-process nil
  "Active Ollama process.")

(defvar ollama-buddy--status "Idle"
  "Current status of the Ollama request.")

(defvar ollama-buddy--model-letters nil
  "Alist mapping letters to model names.")

(defvar ollama-buddy--multishot-sequence nil
  "Current sequence of models for multishot execution.")

(defvar ollama-buddy--multishot-progress 0
  "Progress through current multishot sequence.")

(defvar ollama-buddy--multishot-prompt nil
  "The prompt being used for the current multishot sequence.")

;; Keep track of model colors
(defvar ollama-buddy--model-colors (make-hash-table :test 'equal)
  "Hash table mapping model names to their colors.")

(defun ollama-buddy--md-to-org-convert-region (start end)
  "Convert the region from START to END from Markdown to Org-mode format"
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      ;; Lists: Translate `-`, `*`, or `+` lists to Org-mode lists
      (goto-char (point-min))
      (while (re-search-forward "^\\([ \t]*\\)[*-+] \\(.*\\)$" nil t)
        (replace-match (concat (match-string 1) "- \\2")))
      ;; Bold: `**bold**` -> `*bold*` only if directly adjacent
      (goto-char (point-min))
      (while (re-search-forward "\\*\\*\\([^ ]\\(.*?\\)[^ ]\\)\\*\\*" nil t)
        (replace-match "*\\1*"))
      ;; Italics: `_italic_` -> `/italic/`
      (goto-char (point-min))
      (while (re-search-forward "\\([ \n]\\)_\\([^ ].*?[^ ]\\)_\\([ \n]\\)" nil t)
        (replace-match "\\1/\\2/\\3"))
      ;; Links: `[text](url)` -> `[[url][text]]`
      (goto-char (point-min))
      (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
        (replace-match "[[\\2][\\1]]"))
      ;; Code blocks: Markdown ```lang ... ``` to Org #+begin_src ... #+end_src
      (goto-char (point-min))
      (while (re-search-forward "```\\(.*?\\)\\(?:\n\\|\\s-\\)\\(\\(?:.\\|\n\\)*?\\)```" nil t)
        (replace-match "#+begin_src \\1\n\\2#+end_src"))
      ;; Inline code: `code` -> =code=
      (goto-char (point-min))
      (while (re-search-forward "`\\(.*?\\)`" nil t)
        (replace-match "=\\1="))
      ;; Horizontal rules: `---` or `***` -> `-----`
      (goto-char (point-min))
      (while (re-search-forward "^\\(-{3,}\\|\\*{3,}\\)$" nil t)
        (replace-match "-----"))
      ;; Images: `![alt text](url)` -> `[[url]]`
      (goto-char (point-min))
      (while (re-search-forward "!\\[.*?\\](\\(.*?\\))" nil t)
        (replace-match "[[\\1]]"))
      (goto-char (point-min))
      ;; Headers: Adjust '#'
      (while (re-search-forward "^\\(#+\\) " nil t)
        (replace-match (make-string (length (match-string 1)) ?*) nil nil nil 1))
      (goto-char (point-min))
      ;; any extra characters
      (while (re-search-forward "—" nil t)
        (replace-match ", ")))))


(defun ollama-buddy--prepare-prompt-area (&optional new-prompt keep-content)
  "Prepare the prompt area in the buffer.
When NEW-PROMPT is non-nil, replace the existing prompt area.
When KEEP-CONTENT is non-nil, preserve the existing prompt content."
  (let* ((model (or ollama-buddy--current-model
                   ollama-buddy-default-model
                   "Default:latest"))
         (color (ollama-buddy--get-model-color model))
         (existing-content (when keep-content (ollama-buddy--text-after-prompt))))
    
    ;; Clean up existing prompt
    (goto-char (point-max))
    (when (re-search-backward "\\* .*>> PROMPT:" nil t)
      (beginning-of-line)
      (if (or new-prompt
              (not (string-match-p "[[:alnum:]]" (ollama-buddy--text-after-prompt))))
          ;; Either replacing prompt or current prompt is empty
          (progn
            (skip-chars-backward "\n")
            (delete-region (point) (point-max))
            (goto-char (point-max)))
        ;; Keeping prompt with content
        (goto-char (point-max))))
    
    ;; Insert new prompt header
    (let ((start (point)))
      (insert (format "\n\n* %s [T:%.1f] %s"
                     model
                     ollama-buddy--current-temperature
                     ">> PROMPT: "))
      
      ;; Apply overlay for model name
      (let ((overlay (make-overlay start (+ start 4 (length model)))))
        (overlay-put overlay 'face `(:foreground ,color :weight bold))))
    
    ;; Restore content if requested
    (when (and keep-content existing-content)
      (insert existing-content))))

(defun ollama-buddy--get-prompt-content ()
  "Extract the current prompt content from the buffer.
Returns a cons cell (TEXT . POINT) with the prompt text and point position."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward ">> PROMPT:\\s-*" nil t)
        (let ((start-point (point)))
          (search-forward ":")
          (cons (string-trim (buffer-substring-no-properties
                             (point) (point-max)))
                start-point))
      (cons "" nil))))

(defun ollama-buddy--prepare-command-prompt (command-name &optional selected-text)
  "Prepare prompt for COMMAND-NAME with optional SELECTED-TEXT.
Returns the full prompt text ready to be sent."
  (let* ((cmd-prompt (ollama-buddy--get-command-prop command-name :prompt))
         (model (ollama-buddy--get-command-prop command-name :model))
         (content (or selected-text ""))
         (full-prompt (if cmd-prompt
                         (concat cmd-prompt "\n\n" content)
                       content)))
    
    ;; Temporarily switch model if command has its own model
    (when model
      (setq ollama-buddy--current-request-temporary-model ollama-buddy--current-model)
      (setq ollama-buddy--current-model model))
    
    ;; Prepare the chat buffer
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (pop-to-buffer (current-buffer))
      (ollama-buddy--prepare-prompt-area t nil)  ;; New prompt, no content
      (goto-char (point-max))
      (insert (string-trim full-prompt)))
    
    full-prompt))

(defun ollama-buddy-toggle-markdown-conversion ()
  "Toggle automatic conversion of markdown to org-mode format."
  (interactive)
  (setq ollama-buddy-convert-markdown-to-org 
        (not ollama-buddy-convert-markdown-to-org))
  (ollama-buddy--update-status
   (if ollama-buddy-convert-markdown-to-org "Markdown conversion enabled" "Markdown conversion disabled"))
  (message "Markdown to Org conversion: %s"
           (if ollama-buddy-convert-markdown-to-org "enabled" "disabled")))

(defun ollama-buddy--process-markdown-to-org (text)
  "Convert markdown-formatted TEXT to org-mode format."
  ;; This is a basic implementation that handles common markdown elements
  ;; Implement your custom conversion logic here instead of using pandoc
  (let ((processed-text text))
    ;; Convert code blocks (```language ... ```) to org code blocks (#+BEGIN_SRC language ... #+END_SRC)
    (setq processed-text 
          (replace-regexp-in-string 
           "```\\([a-zA-Z0-9]*\\)\\([^`]+\\)```" 
           "#+BEGIN_SRC \\1\\2#+END_SRC" 
           processed-text))
    
    ;; Convert inline code (`code`) to org verbatim (=code=)
    (setq processed-text 
          (replace-regexp-in-string 
           "`\\([^`\n]+\\)`" 
           "=\\1=" 
           processed-text))
    
    ;; Convert headings (## Heading) to org headings (*** Heading)
    (setq processed-text 
          (replace-regexp-in-string 
           "^\\(#+\\) \\(.*\\)$" 
           (lambda (match)
             (let* ((hash-marks (match-string 1 match))
                    (heading-text (match-string 2 match))
                    (level (+ 2 (length hash-marks))))  ;; Start at level 3 since we already use levels 1-2
               (format "%s %s" (make-string level ?*) heading-text)))
           processed-text))
    
    ;; Convert bold (**text**) to org bold (*text*)
    (setq processed-text 
          (replace-regexp-in-string 
           "\\*\\*\\([^*]+\\)\\*\\*" 
           "*\\1*" 
           processed-text))
    
    ;; Convert italic (*text*) to org italic (/text/)
    (setq processed-text 
          (replace-regexp-in-string 
           "\\*\\([^*]+\\)\\*" 
           "/\\1/" 
           processed-text))
    
    processed-text))

(defun ollama-buddy-temperature-preset (preset)
  "Set temperature to a PRESET value."
  (interactive 
   (list (intern (completing-read 
                  "Temperature preset: " 
                  '("creative" "balanced" "focused" "precise") 
                  nil t))))
  (let ((temp (pcase preset
                ('creative 0.9)
                ('balanced 0.7)
                ('focused 0.3)
                ('precise 0.1)
                (_ ollama-buddy-default-temperature))))
    (setq ollama-buddy--current-temperature temp)
    (ollama-buddy--prepare-prompt-area)
    (message "Temperature set to %.2f (%s)" temp preset)
    (ollama-buddy--update-status 
     (format "Temperature: %.2f (%s)" temp preset))))

(defun ollama-buddy-set-temperature ()
  "Set the temperature for Ollama requests.
Temperature should be a float value, typically between 0.0 and 1.0."
  (interactive)
  (let ((temp (read-number "Temperature (0.0-1.0, default 0.7): " 
                           ollama-buddy--current-temperature)))
    ;; Validate input and set limits
    (setq temp (min 2.0 (max 0.0 temp)))
    (setq ollama-buddy--current-temperature temp)
    (message "Ollama temperature set to %.2f" temp)
    (ollama-buddy--prepare-prompt-area)
    ;; Update the status to show the new temperature
    (ollama-buddy--update-status 
     (format "Temperature: %.2f" ollama-buddy--current-temperature))))

(defun ollama-buddy-reset-temperature ()
  "Reset the temperature to the default value."
  (interactive)
  (setq ollama-buddy--current-temperature ollama-buddy-default-temperature)
  (message "Ollama temperature reset to default: %.2f" 
           ollama-buddy-default-temperature)
  (ollama-buddy--prepare-prompt-area)
  ;; Update the status to show the new temperature
  (ollama-buddy--update-status 
   (format "Temperature reset to %.2f" ollama-buddy--current-temperature)))

(defun ollama-buddy-adjust-temperature ()
  "Adjust the temperature interactively with a slider."
  (interactive)
  (let* ((initial (round (* 100 ollama-buddy--current-temperature)))
         (value (widget-create 'slider
                               :size 20
                               :min 0
                               :max 100
                               :tag "Temperature"
                               :format "%t: %v%%"
                               :value initial
                               :step 5))
         (temp (/ (float (widget-value value)) 100.0)))
    (ollama-buddy--prepare-prompt-area)
    (setq ollama-buddy--current-temperature temp)
    (message "Temperature set to %.2f" temp)))

(defun ollama-buddy--ensure-sessions-directory ()
  "Create the ollama-buddy sessions directory if it doesn't exist."
  (unless (file-directory-p ollama-buddy-sessions-directory)
    (make-directory ollama-buddy-sessions-directory t)))

(defun ollama-buddy-sessions-save (&optional session-name)
  "Save the current conversation state to a session file.
If SESSION-NAME is not provided, prompt for a name."
  (interactive)
  (ollama-buddy--ensure-sessions-directory)
  
  (let* ((current-name (or ollama-buddy--current-session ""))
         (default-name (if (string-empty-p current-name) 
                           "default-session" 
                         current-name))
         (session-name (or session-name 
                           (read-string 
                            (format "Session name (default: %s): " default-name)
                            nil nil default-name)))
         (session-file (expand-file-name 
                        (format "ollama-buddy-session--%s.el" session-name) 
                        ollama-buddy-sessions-directory)))
    
    ;; Write the session file
    (with-temp-file session-file
      (insert ";; ollama-buddy session file\n")
      (insert (format ";; Session: %s\n" session-name))
      (insert (format ";; Created: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      
      ;; Save current model
      (insert (format "(setq ollama-buddy--current-model %S)\n\n" 
                      (or ollama-buddy--current-model 
                          ollama-buddy-default-model)))
      
      ;; Save conversation history by model
      (insert "(setq ollama-buddy--conversation-history-by-model\n")
      (insert "      (let ((table (make-hash-table :test 'equal)))\n")
      
      ;; For each model with history
      (maphash 
       (lambda (model history)
         (when history  ;; Only include non-empty histories
           (insert (format "        (puthash %S\n" model))
           (insert (format "                 '%S\n" history))
           (insert "                 table)\n")))
       ollama-buddy--conversation-history-by-model)
      
      (insert "        table))\n\n")
      
      ;; Set the current history variable for backward compatibility
      (insert "(setq ollama-buddy--conversation-history\n")
      (insert (format "      (gethash %S ollama-buddy--conversation-history-by-model nil))\n\n" 
                      (or ollama-buddy--current-model 
                          ollama-buddy-default-model)))
      
      ;; Set current session name
      (insert (format "(setq ollama-buddy--current-session %S)\n" session-name)))
    
    ;; Update current session name
    (setq ollama-buddy--current-session session-name)
    
    ;; Provide feedback
    (message "Session saved as '%s'" session-name)
    session-file))

(defun ollama-buddy-sessions-load (&optional session-name)
  "Load a saved conversation session.
If SESSION-NAME is not provided, prompt for a name."
  (interactive)
  (ollama-buddy--ensure-sessions-directory)
  
  (let* ((session-files (directory-files 
                         ollama-buddy-sessions-directory nil 
                         "^ollama-buddy-session--.*\\.el$"))
         (session-names (mapcar 
                         (lambda (file)
                           (when (string-match "ollama-buddy-session--\\(.*\\)\\.el$" file)
                             (match-string 1 file)))
                         session-files)))
    
    ;; Check if we have any sessions
    (if (null session-names)
        (message "No saved sessions found in %s" ollama-buddy-sessions-directory)
      
      (let* ((chosen-name (or session-name
                              (completing-read 
                               "Load session: " 
                               session-names nil t)))
             (session-file (expand-file-name 
                            (format "ollama-buddy-session--%s.el" chosen-name) 
                            ollama-buddy-sessions-directory)))
        
        ;; Check if file exists
        (if (not (file-exists-p session-file))
            (message "Session file not found: %s" session-file)
          (progn
            ;; Load the session
            (load-file session-file)
            
            ;; Update the chat buffer to reflect restored state
            (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
              (let ((inhibit-read-only t))
                (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
                (org-mode)
                (visual-line-mode 1)
                ;; Clear the buffer and reinitialize
                (erase-buffer)
                (ollama-buddy-mode 1)
                
                ;; Add session header
                (insert (ollama-buddy--create-intro-message))
                (insert (format "\n\n[Session '%s' restored]" chosen-name))
                
                ;; Collect all messages from all models into a single timeline
                (let* ((all-messages '()))
                  ;; Gather messages from all models
                  (maphash
                   (lambda (model history)
                     (dolist (msg history)
                       ;; Add model information to each message for display
                       (let ((msg-with-model (copy-alist msg)))
                         (setq msg-with-model (cons (cons 'model model) msg-with-model))
                         ;; Add timestamp if available, or use a counter for ordering
                         (unless (assoc 'timestamp msg-with-model)
                           (setq msg-with-model (cons (cons 'timestamp 0) msg-with-model)))
                         ;; (push msg-with-model all-messages)
                         (setq all-messages (append all-messages (list msg-with-model)))
                         )))
                   ollama-buddy--conversation-history-by-model)
                  
                  ;; (setq all-messages (nreverse all-messages))
                  
                  ;; Display all messages in order
                  (dolist (msg all-messages)
                    (let* ((role (alist-get 'role msg))
                           (model (alist-get 'model msg))
                           (content (alist-get 'content msg))
                           (color (ollama-buddy--get-model-color model)))
                      
                      (when (string= role "user")
                        (let ((start (point)))
                          (insert (format "\n\n* %s %s %s"
                                          model
                                          ">> PROMPT: "
                                          content))
                          (let ((overlay (make-overlay start (+ start 4 (length model)))))
                            (overlay-put overlay 'face `(:foreground ,color :weight bold)))))
                      
                      (when (string= role "assistant")
                        (let ((start-pos (point-max)))
                          (insert (format "\n\n%s\n\n%s"
                                          (concat "** [" model ": RESPONSE]")
                                          content))
                          ;; Now convert from markdown to org if enabled
                          (when ollama-buddy-convert-markdown-to-org
                            (ollama-buddy--md-to-org-convert-region start-pos (point-max))))))))
                ;; Show a prompt at the end
                (ollama-buddy--prepare-prompt-area)))))
        ;; Update status
        (ollama-buddy--update-status (format "Session '%s' loaded" chosen-name))
        (message "Loaded session: %s" chosen-name)))))

(defun ollama-buddy-sessions-list ()
  "Display a list of saved sessions."
  (interactive)
  (ollama-buddy--ensure-sessions-directory)
  
  (let* ((session-files (directory-files 
                         ollama-buddy-sessions-directory nil 
                         "^ollama-buddy-session--.*\\.el$"))
         (buf (get-buffer-create "*Ollama Buddy Sessions*")))
    
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Ollama Buddy Saved Sessions:\n\n")
        
        (if (null session-files)
            (insert "No saved sessions found.\n")
          (dolist (file session-files)
            (when (string-match "ollama-buddy-session--\\(.*\\)\\.el$" file)
              (let* ((session-name (match-string 1 file))
                     (file-path (expand-file-name file ollama-buddy-sessions-directory))
                     (attrs (file-attributes file-path))
                     (mod-time (format-time-string 
                                "%Y-%m-%d %H:%M:%S"
                                (file-attribute-modification-time attrs)))
                     (size (file-attribute-size attrs))
                     (models '()))
                
                ;; Attempt to extract model information by reading the file
                (with-temp-buffer
                  (insert-file-contents file-path)
                  (goto-char (point-min))
                  (while (re-search-forward "puthash\\s-+\"\\([^\"]+\\)\"" nil t)
                    (push (match-string 1) models)))
                
                ;; Display session info
                (insert (propertize (format "- %s%s\n" 
                                            session-name
                                            (if (string= session-name ollama-buddy--current-session)
                                                " (current)" ""))
                                    'face '(:weight bold)))
                (insert (format "  Modified: %s\n" mod-time))
                (insert (format "  Size: %d bytes\n" size))
                (when models
                  (insert "  Models: ")
                  (insert (mapconcat 
                           (lambda (model)
                             (propertize model 
                                         'face `(:foreground ,(ollama-buddy--get-model-color model))))
                           (delete-dups models)
                           ", "))
                  (insert "\n"))
                (insert "\n")))))
        
        (insert "\nUse M-x ollama-buddy-sessions-load to load a session")
        (insert "\nUse M-x ollama-buddy-sessions-save to save the current session")
        (insert "\nUse M-x ollama-buddy-sessions-delete to delete a session")
        (view-mode 1)))
    
    (display-buffer buf)))

(defun ollama-buddy-sessions-delete (&optional session-name)
  "Delete a saved session.
If SESSION-NAME is not provided, prompt for a name."
  (interactive)
  (ollama-buddy--ensure-sessions-directory)
  
  (let* ((session-files (directory-files 
                         ollama-buddy-sessions-directory nil 
                         "^ollama-buddy-session--.*\\.el$"))
         (session-names (mapcar 
                         (lambda (file)
                           (when (string-match "ollama-buddy-session--\\(.*\\)\\.el$" file)
                             (match-string 1 file)))
                         session-files)))
    
    ;; Check if we have any sessions
    (if (null session-names)
        (message "No saved sessions found in %s" ollama-buddy-sessions-directory)
      
      (let* ((chosen-name (or session-name
                              (completing-read 
                               "Delete session: " 
                               session-names nil t)))
             (session-file (expand-file-name 
                            (format "ollama-buddy-session--%s.el" chosen-name) 
                            ollama-buddy-sessions-directory)))
        
        ;; Check if file exists
        (if (not (file-exists-p session-file))
            (message "Session file not found: %s" session-file)
          
          ;; Confirm deletion
          (when (yes-or-no-p (format "Really delete session '%s'? " chosen-name))
            (delete-file session-file)
            
            ;; Reset current session if we just deleted it
            (when (string= chosen-name ollama-buddy--current-session)
              (setq ollama-buddy--current-session nil))
            
            (message "Deleted session: %s" chosen-name)))))))

(defun ollama-buddy-sessions-new ()
  "Start a new session by clearing history and buffer."
  (interactive)

  (when (y-or-n-p "Are you sure? ")
    ;; Confirm if we have a current session
    (when (and ollama-buddy--current-session
               (not (yes-or-no-p 
                     (format "Start new session? Current session '%s' will be discarded unless saved. " 
                             ollama-buddy--current-session))))
      (user-error "Operation cancelled"))
    
    ;; Clear current session
    (setq ollama-buddy--current-session nil)
    
    ;; Clear all model histories
    (clrhash ollama-buddy--conversation-history-by-model)
    (setq ollama-buddy--conversation-history nil)
    
    ;; Clear the chat buffer
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (let ((inhibit-read-only t))
        (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
        (erase-buffer)
        (ollama-buddy-mode 1)
        (insert (ollama-buddy--create-intro-message))
        (ollama-buddy--prepare-prompt-area)))
    
    ;; Update status
    (ollama-buddy--update-status "New session started")
    (message "Started new session")))

(defun ollama-buddy-sessions-open-directory ()
  "Open the ollama-buddy sessions directory in Dired."
  (interactive)
  (ollama-buddy--ensure-sessions-directory)
  (dired ollama-buddy-sessions-directory))

;; Auto-save session functionality
(defcustom ollama-buddy-auto-save-session nil
  "Whether to automatically save session on exit."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-auto-save-session-name "autosave"
  "Name to use for auto-saved sessions."
  :type 'string
  :group 'ollama-buddy)

(defun ollama-buddy-sessions-maybe-auto-save ()
  "Auto-save session if enabled and there's content to save."
  (when (and ollama-buddy-auto-save-session
             (> (hash-table-count ollama-buddy--conversation-history-by-model) 0))
    (ollama-buddy-sessions-save ollama-buddy-auto-save-session-name)))

(defun ollama-buddy--add-to-history (role content)
  "Add message with ROLE and CONTENT to conversation history for current model."
  (when ollama-buddy-history-enabled
    (let* ((model ollama-buddy--current-model)
           (history (gethash model ollama-buddy--conversation-history-by-model nil)))
      
      ;; Create new history entry for this model if it doesn't exist
      (unless history
        (setq history nil))
      
      ;; Add the new message to this model's history
      ;; and put it at the end
      (setq history
            (append history
                    (list `((role . ,role)
                            (content . ,content)))))
      
      ;; Truncate history if needed
      (when (> (length history) (* 2 ollama-buddy-max-history-length))
        (setq history (seq-take history (* 2 ollama-buddy-max-history-length))))
      
      ;; Update the hash table with the modified history
      (puthash model history ollama-buddy--conversation-history-by-model)
      
      ;; Update the current history variable for backward compatibility
      (setq ollama-buddy--conversation-history history))))

(defun ollama-buddy--get-history-for-request ()
  "Get history for the current request."
  (if ollama-buddy-history-enabled
      (let* ((model ollama-buddy--current-model)
             (history (gethash model ollama-buddy--conversation-history-by-model nil)))
        history)
    nil))

(defun ollama-buddy-clear-history (&optional all-models)
  "Clear the conversation history.
With prefix argument ALL-MODELS, clear history for all models."
  (interactive "P")
  (if all-models
      (progn
        (clrhash ollama-buddy--conversation-history-by-model)
        (setq ollama-buddy--conversation-history nil)
        (ollama-buddy--update-status "All models' history cleared")
        (message "Ollama conversation history cleared for all models"))
    (let ((model ollama-buddy--current-model))
      (remhash model ollama-buddy--conversation-history-by-model)
      (setq ollama-buddy--conversation-history nil)
      (ollama-buddy--update-status (format "History cleared for %s" model))
      (message "Ollama conversation history cleared for %s" model))))

(defun ollama-buddy-toggle-history ()
  "Toggle conversation history on/off."
  (interactive)
  (setq ollama-buddy-history-enabled (not ollama-buddy-history-enabled))
  (ollama-buddy--update-status 
   (if ollama-buddy-history-enabled "History enabled" "History disabled"))
  (message "Ollama conversation history %s" 
           (if ollama-buddy-history-enabled "enabled" "disabled")))

(defun ollama-buddy--display-history (&optional all-models)
  "Display the conversation history in a buffer.
With prefix argument ALL-MODELS, show history for all models."
  (interactive "P")
  (let ((buf (get-buffer-create "*Ollama Conversation History*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (visual-line-mode 1)
        (erase-buffer)
        (insert "Ollama Conversation History:\n\n")
        
        (if all-models
            ;; Display history for all models
            (progn
              (if (= (hash-table-count ollama-buddy--conversation-history-by-model) 0)
                  (insert "No conversation history available for any model.")
                (maphash
                 (lambda (model history)
                   (let ((history-count (/ (length history) 2)))
                     (insert (format "== Model: %s (%d message pairs) ==\n\n" 
                                     model history-count))
                     (dolist (msg history)
                       (let* ((role (alist-get 'role msg))
                              (content (alist-get 'content msg))
                              (role-face (if (string= role "user") 
                                             '(:inherit bold) 
                                           '(:inherit bold))))
                         (insert (propertize (format "[%s]: " (upcase role)) 'face role-face))
                         (insert (format "%s\n\n" content))))
                     (insert "\n")))
                 ollama-buddy--conversation-history-by-model)))
          
          ;; Display history for current model only
          (let* ((model ollama-buddy--current-model)
                 (history (gethash model ollama-buddy--conversation-history-by-model nil)))
            (if (null history)
                (insert (format "No conversation history available for model %s." model))
              (let ((history-count (/ (length history) 2)))
                (insert (format "Current history for %s: %d message pairs\n\n" 
                                model history-count))
                
                ;; Display the history in chronological order
                (dolist (msg history)
                  (let* ((role (alist-get 'role msg))
                         (content (alist-get 'content msg))
                         (role-face (if (string= role "user") 
                                        '(:inherit bold :foreground "green") 
                                      '(:inherit bold :foreground "blue"))))
                    (insert (propertize (format "[%s]: " (upcase role)) 'face role-face))
                    (insert (format "%s\n\n" content))))))))

        (insert "\n==================================================")
        (insert "\nM-x ollama-buddy-toggle-history to toggle history")
        (insert "\nM-x ollama-buddy-clear-history to clear history for current model")
        (insert "\nC-u M-x ollama-buddy-clear-history to clear history for all models")
        (view-mode 1)))
    (display-buffer buf)))

(defun ollama-buddy--update-token-rate-display ()
  "Update the token rate display in real-time."
  (when (and ollama-buddy--current-token-start-time
             (> ollama-buddy--current-token-count 0))
    (let* ((current-time (float-time))
           (total-rate (if (> (- current-time ollama-buddy--current-token-start-time) 0)
                           (/ ollama-buddy--current-token-count 
                              (- current-time ollama-buddy--current-token-start-time))
                         0)))
      
      ;; Update status with token information
      (ollama-buddy--update-status 
       (format "Processing... [%d tokens, %.1f t/s]" 
               ollama-buddy--current-token-count total-rate))
      
      ;; Update tracking variables
      (setq ollama-buddy--last-token-count ollama-buddy--current-token-count
            ollama-buddy--last-update-time current-time))))

(defun ollama-buddy-display-token-stats ()
  "Display token usage statistics."
  (interactive)
  (let ((buf (get-buffer-create "*Ollama Token Stats*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Ollama Token Usage Statistics:\n\n")
        
        (if (null ollama-buddy--token-usage-history)
            (insert "No token usage data available yet.")
          (let* ((total-tokens (apply #'+ (mapcar (lambda (info) (plist-get info :tokens)) 
                                                  ollama-buddy--token-usage-history)))
                 (avg-rate (/ (apply #'+ (mapcar (lambda (info) (plist-get info :rate)) 
                                                 ollama-buddy--token-usage-history))
                              (float (length ollama-buddy--token-usage-history)))))
            
            ;; Summary stats
            (insert (format "Total tokens generated: %d\n" total-tokens))
            (insert (format "Average token rate: %.2f tokens/sec\n\n" avg-rate))
            
            ;; Model breakdown
            (insert "Tokens by model:\n")
            (let ((model-stats (make-hash-table :test 'equal)))
              (dolist (info ollama-buddy--token-usage-history)
                (let* ((model (plist-get info :model))
                       (tokens (plist-get info :tokens))
                       (current (gethash model model-stats '(0 0))))
                  (puthash model 
                           (list (+ (car current) tokens) (1+ (cadr current)))
                           model-stats)))
              
              (maphash (lambda (model stats)
                         (let ((model-color (ollama-buddy--get-model-color model)))
                           (insert "  ")
                           (insert (propertize model 'face `(:foreground ,model-color)))
                           (insert (format ": %d tokens in %d responses\n" 
                                           (car stats) (cadr stats)))))
                       model-stats))
            
            ;; Recent history (last 10)
            (insert "\nRecent interactions:\n")
            (let ((recent (seq-take ollama-buddy--token-usage-history 10)))
              (dolist (info recent)
                (let ((model (plist-get info :model))
                      (tokens (plist-get info :tokens))
                      (rate (plist-get info :rate))
                      (time (format-time-string "%Y-%m-%d %H:%M:%S" 
                                                (plist-get info :timestamp))))
                  (insert (format "  %s: %d tokens (%.2f t/s) at %s\n" 
                                  model tokens rate time))))))
          
          (insert "\n\nUse M-x ollama-buddy-toggle-token-display to toggle display of token stats in responses")
          (view-mode 1))))
    (display-buffer buf)))

(defun ollama-buddy-toggle-token-display ()
  "Toggle display of token statistics after each response."
  (interactive)
  (setq ollama-buddy-display-token-stats (not ollama-buddy-display-token-stats))
  (message "Ollama token statistics display: %s"
           (if ollama-buddy-display-token-stats "enabled" "disabled")))

;; Function to update and retrieve model colors
(defun ollama-buddy--update-model-colors ()
  "Update the model colors hash table and return it."
  (let ((models-with-colors (ollama-buddy--get-models-with-colors)))
    (dolist (pair models-with-colors)
      (puthash (car pair) (cdr pair) ollama-buddy--model-colors))
    ollama-buddy--model-colors))

;; Get color for a specific model
(defun ollama-buddy--get-model-color (model)
  "Get the color associated with MODEL."
  (if ollama-buddy-enable-model-colors
      (or (gethash model ollama-buddy--model-colors)
          (ollama-buddy--hash-string-to-color model))
    (face-foreground 'default)))  ;; Returns the default foreground color

(defun ollama-buddy-toggle-model-colors ()
  "Toggle the use of model-specific colors in ollama-buddy."
  (interactive)
  (setq ollama-buddy-enable-model-colors (not ollama-buddy-enable-model-colors))
  (message "Ollama Buddy Model Colors: %s"
           (if ollama-buddy-enable-model-colors "Enabled" "Disabled")))

(defun ollama-buddy--color-contrast (color1 color2)
  "Calculate contrast ratio between COLOR1 and COLOR2.
Returns a value between 1 and 21, where higher values indicate better contrast.
Based on WCAG 2.0 contrast algorithm."
  (let* ((rgb1 (color-name-to-rgb color1))
         (rgb2 (color-name-to-rgb color2))
         ;; Calculate relative luminance for each color
         (l1 (ollama-buddy--relative-luminance rgb1))
         (l2 (ollama-buddy--relative-luminance rgb2))
         ;; Ensure lighter color is l1
         (light (max l1 l2))
         (dark (min l1 l2)))
    ;; Contrast ratio formula
    (/ (+ light 0.05) (+ dark 0.05))))

(defun ollama-buddy--relative-luminance (rgb)
  "Calculate the relative luminance of RGB.
RGB should be a list of (r g b) values between 0 and 1."
  (let* ((r (nth 0 rgb))
         (g (nth 1 rgb))
         (b (nth 2 rgb))
         ;; Convert RGB to linear values (gamma correction)
         (r-linear (if (<= r 0.03928)
                       (/ r 12.92)
                     (expt (/ (+ r 0.055) 1.055) 2.4)))
         (g-linear (if (<= g 0.03928)
                       (/ g 12.92)
                     (expt (/ (+ g 0.055) 1.055) 2.4)))
         (b-linear (if (<= b 0.03928)
                       (/ b 12.92)
                     (expt (/ (+ b 0.055) 1.055) 2.4))))
    ;; Calculate luminance with RGB coefficients
    (+ (* 0.2126 r-linear)
       (* 0.7152 g-linear)
       (* 0.0722 b-linear))))

(defun ollama-buddy--hash-string-to-color (str)
  "Generate a consistent color based on the hash of STR with good contrast.
Adapts the color to the current theme (light or dark) for better visibility."
  (let* ((hash (abs (sxhash str)))
         ;; Generate HSL values - keeping saturation high for readability
         (hue (mod hash 360))
         (saturation 85)
         ;; Determine if background is light or dark
         (is-dark-background (eq (frame-parameter nil 'background-mode) 'dark))
         ;; Adjust lightness based on background (darker for light bg, lighter for dark bg)
         (base-lightness (if is-dark-background 65 45))
         ;; Avoid problematic hue ranges for visibility (e.g., yellows on white background)
         ;; Adjust lightness for problematic hues
         (lightness (cond
                     ;; Yellows (40-70) - make darker on light backgrounds
                     ((and (>= hue 40) (<= hue 70) (not is-dark-background))
                      (max 20 (- base-lightness 20)))
                     ;; Blues (180-240) - make lighter on dark backgrounds
                     ((and (>= hue 180) (<= hue 240) is-dark-background)
                      (min 80 (+ base-lightness 15)))
                     ;; Default lightness
                     (t base-lightness)))
         ;; Convert HSL to RGB
         (rgb-values (color-hsl-to-rgb (/ hue 360.0) (/ saturation 100.0) (/ lightness 100.0)))
         ;; Convert RGB to hex color
         (color (apply #'color-rgb-to-hex rgb-values))
         ;; Get foreground/background colors for contrast check
         (bg-color (face-background 'default))
         (target-color color))
    
    ;; Adjust saturation for better contrast if needed (fallback approach)
    (when (and bg-color
               (< (ollama-buddy--color-contrast bg-color target-color) 4.5))
      (let* ((adjusted-saturation (min 100 (+ saturation 10)))
             (adjusted-lightness (if is-dark-background
                                     (min 85 (+ lightness 10))
                                   (max 15 (- lightness 10))))
             (adjusted-rgb (color-hsl-to-rgb (/ hue 360.0) 
                                             (/ adjusted-saturation 100.0) 
                                             (/ adjusted-lightness 100.0))))
        (setq target-color (apply #'color-rgb-to-hex adjusted-rgb))))
    
    target-color))

;; Modify the model retrieval function to include colors
(defun ollama-buddy--get-models-with-colors ()
  "Get available Ollama models with their associated colors."
  (when-let ((response (ollama-buddy--make-request "/api/tags" "GET")))
    (mapcar (lambda (m)
              (let ((name (alist-get 'name m)))
                (cons name (ollama-buddy--hash-string-to-color name))))
            (alist-get 'models response))))

(defun ollama-buddy--assign-model-letters ()
  "Assign letters to available models and update the intro message."
  (setq ollama-buddy--model-letters
        (cl-loop for model in (ollama-buddy--get-models)
                 for letter across "abcdefghijklmnopqrstuvwxyz"
                 collect (cons letter model))))

(defun ollama-buddy--format-models-with-letters ()
  "Format models with letter assignments for display."
  (when-let* ((models-alist ollama-buddy--model-letters)
              (total (length models-alist))
              (rows (ceiling (/ total 2.0))))
    (let* ((formatted-pairs
            (cl-loop for row below rows
                     collect
                     (cl-loop for col below 2
                              for idx = (+ (* col rows) row)
                              when (< idx total)
                              collect (nth idx models-alist))))
           (max-width (apply #'max
                             (mapcar (lambda (pair)
                                       (length (cdr pair)))
                                     models-alist)))
           (format-str (format "  (%%c) %%-%ds  %%s" max-width)))
      (concat (mapconcat
               (lambda (row)
                 (format format-str
                         (caar row)
                         (if (cdar row)
                             (let ((color (ollama-buddy--get-model-color (cdar row))))
                               (propertize (cdar row) 'face `(:foreground ,color)))
                           "")
                         (if (cdr row)
                             (let ((color (ollama-buddy--get-model-color (cdadr row))))
                               (format "(%c) %s" (caadr row)
                                       (propertize (cdadr row) 'face `(:foreground ,color))))
                           "")))
               formatted-pairs
               "\n")
              "\n\n"))))

(defun ollama-buddy--check-status ()
  "Check Ollama status with caching for better performance."
  (let ((current-time (float-time)))
    (when (or (null ollama-buddy--last-status-check)
              (> (- current-time ollama-buddy--last-status-check)
                 ollama-buddy--status-cache-ttl))
      (setq ollama-buddy--status-cache (ollama-buddy--ollama-running)
            ollama-buddy--last-status-check current-time))
    ollama-buddy--status-cache))

(defvar ollama-buddy-roles--current-role "default"
  "The currently active ollama-buddy role.")

(defvar ollama-buddy-role-creator--command-template
  '((key . nil)
    (description . nil)
    (model . nil)
    (prompt . nil))
  "Template for a new command definition.")

(defun ollama-buddy-roles--get-available-roles ()
  "Scan the preset directory and extract role names from filenames."
  (if (not (file-directory-p ollama-buddy-roles-directory))
      (progn
        (message "Error: Ollama Buddy roles directory does not exist: %s" 
                 ollama-buddy-roles-directory)
        nil)
    (let ((files (directory-files ollama-buddy-roles-directory nil "^ollama-buddy--preset__.*\\.el$"))
          roles)
      (if (null files)
          (progn
            (message "No role preset files found in directory: %s" 
                     ollama-buddy-roles-directory)
            nil)
        (dolist (file files)
          (when (string-match "ollama-buddy--preset__\\(.*\\)\\.el$" file)
            (push (match-string 1 file) roles)))
        (sort roles #'string<)))))

(defun ollama-buddy-roles--load-role-preset (role)
  "Load the preset file for ROLE."
  (let ((preset-file (expand-file-name 
                      (format "ollama-buddy--preset__%s.el" role) 
                      ollama-buddy-roles-directory)))
    (if (file-exists-p preset-file)
        (progn
          (load-file preset-file)
          (setq ollama-buddy-roles--current-role role)
          (message "Loaded Ollama Buddy role: %s" role))
      (message "Role preset file not found: %s" preset-file))))


(defun ollama-buddy-roles-switch-role ()
  "Switch to a different ollama-buddy role."
  (interactive)
  (let ((roles (ollama-buddy-roles--get-available-roles)))
    (if (null roles)
        (message "No role presets available. Create some files in %s first." 
                 ollama-buddy-roles-directory)
      (let ((role (completing-read 
                   (format "Select role (current: %s): " ollama-buddy-roles--current-role)
                   roles nil t)))
        (ollama-buddy-roles--load-role-preset role)))))

(defun ollama-buddy-role-creator--create-command ()
  "Create a new command interactively."
  (let* ((command-name (read-string "Command name (e.g., my-command): "))
         (key (read-char "Press key for menu shortcut: "))
         (description (read-string "Description: "))
         (use-model (y-or-n-p "Use specific model? "))
         (model (if use-model
                    (completing-read "Model: " (ollama-buddy--get-models) nil t)
                  nil))
         (use-prompt (y-or-n-p "Add a system prompt? "))
         (prompt (if use-prompt
                     (read-string "System prompt: ")
                   nil))
         (symbol (intern command-name)))
    ;; Generate the command definition
    (list symbol
          :key key
          :description description
          :model model
          :prompt prompt
          :action `(lambda () 
                     (ollama-buddy--send-with-command ',symbol)))))

(defun ollama-buddy-role-creator-generate-role-file (role-name commands)
  "Generate a role file for ROLE-NAME with COMMANDS."
  (let ((file-path (expand-file-name 
                    (format "ollama-buddy--preset__%s.el" role-name)
                    ollama-buddy-roles-directory)))
    ;; Create directory if it doesn't exist
    (unless (file-directory-p ollama-buddy-roles-directory)
      (make-directory ollama-buddy-roles-directory t))
    ;; Generate the file content
    (with-temp-file file-path
      (insert (format ";; ollama-buddy preset for role: %s\n" role-name))
      (insert ";; Generated by ollama-buddy-role-creator\n\n")
      (insert "(require 'ollama-buddy)\n\n")
      (insert "(setq ollama-buddy-command-definitions\n")
      (insert "  '(\n")
      ;; Insert the standard commands first
      (insert "    ;; Standard commands\n")
      (dolist (cmd '(open-chat show-models switch-role create-role open-roles-directory swap-model help send-region))
        (when-let ((cmd-def (ollama-buddy--get-command-def cmd)))
          (insert (format "    %S\n" cmd-def))))
      ;; Insert custom commands
      (insert "\n    ;; Custom commands for this role\n")
      (dolist (cmd commands)
        (insert (format "    %S\n" cmd)))
      ;; Close the list and provide call
      (insert "    ))\n\n"))
    ;; Return the file path
    file-path))

;;;###autoload
(defun ollama-buddy-role-creator-create-new-role ()
  "Create a new role interactively."
  (interactive)
  ;; Ensure the directory exists
  (ollama-buddy-roles-create-directory)
  (let ((role-name (read-string "Role name: "))
        (commands '())
        (continue t))
    ;; Main command creation loop
    (while continue
      (message "Adding command %d..." (1+ (length commands)))
      (push (ollama-buddy-role-creator--create-command) commands)
      (setq continue (y-or-n-p "Add another command? ")))
    ;; Generate the role file
    (let ((file-path (ollama-buddy-role-creator-generate-role-file role-name commands)))
      (message "Role saved to %s" file-path)
      ;; Ask to load the new role
      (when (y-or-n-p "Load this role now? ")
        (ollama-buddy-roles--load-role-preset role-name)))))

;; Initialize with default role if it exists
(defun ollama-buddy-roles-initialize ()
  "Initialize the roles system and load the default role if available."
  (let ((roles (ollama-buddy-roles--get-available-roles)))
    (when (and roles (member "default" roles))
      (ollama-buddy-roles--load-role-preset "default"))))

;; Helper function to create the roles directory
(defun ollama-buddy-roles-create-directory ()
  "Create the ollama-buddy roles directory if it doesn't exist."
  (interactive)
  (if (file-exists-p ollama-buddy-roles-directory)
      (message "Ollama Buddy roles directory already exists: %s" 
               ollama-buddy-roles-directory)
    (if (yes-or-no-p 
         (format "Create Ollama Buddy roles directory at %s? " 
                 ollama-buddy-roles-directory))
        (progn
          (make-directory ollama-buddy-roles-directory t)
          (message "Created Ollama Buddy roles directory: %s" 
                   ollama-buddy-roles-directory))
      (message "Directory creation cancelled."))))

;; Function to open the roles directory in dired
(defun ollama-buddy-roles-open-directory ()
  "Open the ollama-buddy roles directory in Dired."
  (interactive)
  (if (not (file-directory-p ollama-buddy-roles-directory))
      (if (yes-or-no-p 
           (format "Roles directory doesn't exist. Create it at %s? " 
                   ollama-buddy-roles-directory))
          (progn
            (make-directory ollama-buddy-roles-directory t)
            (dired ollama-buddy-roles-directory))
        (message "Directory not created."))
    (dired ollama-buddy-roles-directory)))

(defun ollama-buddy--validate-model (model)
  "Validate MODEL availability."
  (when (and model (ollama-buddy--ollama-running))
    (when (member model (ollama-buddy--get-models))
      model)))

(defun ollama-buddy--get-valid-model (specified-model)
  "Get valid model from SPECIFIED-MODEL with fallback handling."
  (let* ((valid-model (or (ollama-buddy--validate-model specified-model)
                          (ollama-buddy--validate-model ollama-buddy-default-model))))
    (if valid-model
        (cons valid-model specified-model)
      (let ((models (ollama-buddy--get-models)))
        (if models
            (let ((selected (completing-read
                             (format "%s not available. Select model: "
                                     (or specified-model ""))
                             models nil t)))
              (setq ollama-buddy--current-model selected)
              (cons selected specified-model))
          (error "No Ollama models available"))))))

(defun ollama-buddy--monitor-connection ()
  "Monitor Ollama connection status and update UI accordingly."
  (unless (ollama-buddy--ollama-running)
    (when (and ollama-buddy--active-process
               (process-live-p ollama-buddy--active-process))
      (delete-process ollama-buddy--active-process)
      (setq ollama-buddy--active-process nil)
      (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "\n\n[Connection Lost - Request Interrupted]")))))
  (ollama-buddy--update-status ollama-buddy--status))

(defun ollama-buddy--update-status (status &optional original-model actual-model)
  "Update the Ollama status and refresh the display.
STATUS is the current operation status.
ORIGINAL-MODEL is the model that was requested.
ACTUAL-MODEL is the model being used instead."
  (setq ollama-buddy--status status)
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (let* ((model (or ollama-buddy--current-model
                      ollama-buddy-default-model
                      "No Model"))
           (history (when (and ollama-buddy-show-history-indicator 
                               ollama-buddy-history-enabled)
                      (let ((history-count (/ (length 
                                               (gethash model 
                                                        ollama-buddy--conversation-history-by-model 
                                                        nil)) 
                                              2)))
                        (format " [H:%d]" history-count))))
           (temp-info (format " [T:%.1f]" ollama-buddy--current-temperature)))
      (setq header-line-format
            (concat
             (if ollama-buddy-convert-markdown-to-org " ORG" " Markdown")
             (format (if (string-empty-p (ollama-buddy--update-multishot-status))
                         " %s%s %s %s%s%s"
                       " %s %s %s %s%s%s")
                     (ollama-buddy--update-multishot-status)
                     (propertize (if (ollama-buddy--check-status) "RUNNING" "OFFLINE")
                                 'face '(:weight bold))
                     (propertize model 'face `(:weight bold :box (:line-width 4 :style pressed-button)))
                     (propertize status 'face '(:weight bold))
                     (or history "")
                     temp-info)
             (when (and original-model actual-model (not (string= original-model actual-model)))
               (propertize (format " [Using %s instead of %s]" actual-model original-model)
                           'face '(:foreground "orange" :weight bold))))))))

(defun ollama-buddy--ensure-running ()
  "Ensure Ollama is running and update status accordingly."
  (unless (ollama-buddy--check-status)
    (user-error "Ollama is not running.  Please start Ollama server")))

(defun ollama-buddy--initialize-chat-buffer ()
  "Initialize the chat buffer and check Ollama status."
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (when (= (buffer-size) 0)
      (org-mode)
      (visual-line-mode 1)
      (ollama-buddy-mode 1)
      (ollama-buddy--check-status)
      ;; now set up default model if none exist
      (when (not ollama-buddy-default-model)
        ;; just get the first model
        (let ((model (car (ollama-buddy--get-models))))
          (setq ollama-buddy--current-model model)
          (setq ollama-buddy-default-model model)
          (insert "NO DEFAULT MODEL : Using best guess : " model)))
      (insert (ollama-buddy--create-intro-message))
      (ollama-buddy--prepare-prompt-area)
      (put 'ollama-buddy--cycle-prompt-history 'history-position -1))
    (ollama-buddy--update-status "Idle")))

(defun ollama-buddy--stream-filter (_proc output)
  "Process stream OUTPUT while preserving cursor position."
  (when-let* ((json-str (replace-regexp-in-string "^[^\{]*" "" output))
              (json-data (and (> (length json-str) 0) (json-read-from-string json-str)))
              (text (alist-get 'content (alist-get 'message json-data))))
    
    ;; Set start time if this is the first token and start the update timer
    (unless ollama-buddy--current-token-start-time
      (setq ollama-buddy--current-token-start-time (float-time)
            ollama-buddy--last-token-count 0
            ollama-buddy--last-update-time nil
            
            ;; Add a marker for the response start position
            ollama-buddy--response-start-position (with-current-buffer ollama-buddy--chat-buffer (point-max)))
      
      ;; Start the real-time update timer
      (when ollama-buddy--token-update-timer
        (cancel-timer ollama-buddy--token-update-timer))
      (setq ollama-buddy--token-update-timer
            (run-with-timer 0 ollama-buddy--token-update-interval
                            #'ollama-buddy--update-token-rate-display)))
    
    ;; Increment token count when text is received
    (when (not (string-empty-p text))
      (setq ollama-buddy--current-token-count (1+ ollama-buddy--current-token-count)))
    
    (with-current-buffer ollama-buddy--chat-buffer
      (let* ((inhibit-read-only t)
             (window (get-buffer-window ollama-buddy--chat-buffer t))
             (old-point (and window (window-point window)))
             (at-end (and window (>= old-point (point-max))))
             (old-window-start (and window (window-start window))))
        (save-excursion
          (goto-char (point-max))
          ;; Insert the text directly - we'll convert it at the end of the response
          (insert text)

         ;; Track the complete response for history
          (when (boundp 'ollama-buddy--current-response)
            (setq ollama-buddy--current-response 
                  (concat (or ollama-buddy--current-response "") text)))
          (unless (boundp 'ollama-buddy--current-response)
            (setq ollama-buddy--current-response text))

          ;; lets push to a register if multishot is enabled
          (when ollama-buddy--multishot-sequence
            (let* ((reg-char
                    (aref ollama-buddy--multishot-sequence
                          ollama-buddy--multishot-progress))
                   (current (get-register reg-char))
                   (new-content (concat (if (stringp current) current "") text)))
              (set-register reg-char new-content)))
          
          ;; Check if this response is complete
          (when (eq (alist-get 'done json-data) t)
            ;; Convert the response from markdown to org format if enabled
            (when ollama-buddy-convert-markdown-to-org
              (let ((response-end (point-max)))
                (when (and (boundp 'ollama-buddy--response-start-position)
                           ollama-buddy--response-start-position)
                  (ollama-buddy--md-to-org-convert-region 
                   ollama-buddy--response-start-position 
                   response-end)
                  ;; Reset the marker after conversion
                  (makunbound 'ollama-buddy--response-start-position))))
            
            ;; Add the user message to history
            (ollama-buddy--add-to-history "user" ollama-buddy--current-prompt)
            ;; Add the complete response to history
            (ollama-buddy--add-to-history "assistant" ollama-buddy--current-response)
            (makunbound 'ollama-buddy--current-response)
            
            ;; Cancel the update timer
            (when ollama-buddy--token-update-timer
              (cancel-timer ollama-buddy--token-update-timer)
              (setq ollama-buddy--token-update-timer nil))
            
            ;; Calculate final statistics
            (let* ((elapsed-time (- (float-time) ollama-buddy--current-token-start-time))
                   (token-rate (if (> elapsed-time 0)
                                   (/ ollama-buddy--current-token-count elapsed-time)
                                 0))
                   (token-info (list :model ollama-buddy--current-model
                                     :tokens ollama-buddy--current-token-count
                                     :elapsed elapsed-time
                                     :rate token-rate
                                     :timestamp (current-time))))
              
              ;; Add to history
              (push token-info ollama-buddy--token-usage-history)
              
              ;; Display token info if enabled
              (when ollama-buddy-display-token-stats
                (insert (format "\n\n*** Token Stats\n[%d tokens in %.1fs, %.1f tokens/sec]" 
                                ollama-buddy--current-token-count
                                elapsed-time
                                token-rate)))
              
              ;; Reset tracking variables
              (setq ollama-buddy--current-token-count 0
                    ollama-buddy--current-token-start-time nil
                    ollama-buddy--last-token-count 0
                    ollama-buddy--last-update-time nil))

            ;; reset the current model if from external
            (when ollama-buddy--current-request-temporary-model
              (setq ollama-buddy--current-model ollama-buddy--current-request-temporary-model)
              (setq ollama-buddy--current-request-temporary-model nil))
            
            (insert "\n\n*** FINISHED")
            
            ;; Handle multishot progression here
            (if ollama-buddy--multishot-sequence
                (progn
                  ;; Increment progress
                  (setq ollama-buddy--multishot-progress
                        (1+ ollama-buddy--multishot-progress))
                  ;; Check if there are more models to process
                  (if (< ollama-buddy--multishot-progress
                         (length ollama-buddy--multishot-sequence))
                      (progn
                        ;; Process next model after a short delay
                        (run-with-timer 0.5 nil #'ollama-buddy--send-next-in-sequence))
                    (progn
                      (ollama-buddy--update-status "Multi Finished")
                      (ollama-buddy--prepare-prompt-area))))
              ;; Not in multishot mode, just show the prompt
              (progn
                (ollama-buddy--prepare-prompt-area)
                (ollama-buddy--update-status (format "Finished [%d tokens, %.1f t/s]" 
                                                     (plist-get (car ollama-buddy--token-usage-history) :tokens)
                                                     (plist-get (car ollama-buddy--token-usage-history) :rate)))))))
        (when window
          (if at-end
              (set-window-point window (point-max))
            (set-window-point window old-point))
          (set-window-start window old-window-start t))))))

(defun ollama-buddy--stream-sentinel (_proc event)
  "Handle stream completion EVENT."
  (when-let* ((status (cond ((string-match-p "finished" event) "Completed")
                            ((string-match-p "\\(?:deleted\\|connection broken\\)" event) "Interrupted")))
              (msg (format "\n\n[Stream %s]" status)))
    ;; Clean up multishot variables
    (setq ollama-buddy--multishot-sequence nil
          ollama-buddy--multishot-prompt nil)
    
    ;; Clean up token tracking
    (when ollama-buddy--token-update-timer
      (cancel-timer ollama-buddy--token-update-timer)
      (setq ollama-buddy--token-update-timer nil))

    ;; reset the current model if from external
    (when ollama-buddy--current-request-temporary-model
      (setq ollama-buddy--current-model ollama-buddy--current-request-temporary-model)
      (setq ollama-buddy--current-request-temporary-model nil))
    
    (with-current-buffer ollama-buddy--chat-buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize msg 'face '(:weight bold)))
        (ollama-buddy--prepare-prompt-area)))
    
    ;; Only show token stats in status if we completed successfully
    (if (string= status "Completed")
        (let ((last-info (car ollama-buddy--token-usage-history)))
          (if last-info
              (ollama-buddy--update-status 
               (format "Stream %s [%d tokens, %.1f t/s]" 
                       status
                       (plist-get last-info :tokens)
                       (plist-get last-info :rate)))
            (ollama-buddy--update-status (concat "Stream " status))))
      (ollama-buddy--update-status (concat "Stream " status)))))

(defun ollama-buddy--swap-model ()
  "Swap ollama model."
  (interactive)
  (if (not (ollama-buddy--ollama-running))
      (error "!!WARNING!! ollama server not running")
    (let ((new-model
           (completing-read "Model: " (ollama-buddy--get-models) nil t)))
      (setq ollama-buddy-default-model new-model)
      (setq ollama-buddy--current-model new-model)
      (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
      (ollama-buddy--prepare-prompt-area)
      (goto-char (point-max))
      (ollama-buddy--update-status "Idle"))))

;; Update buffer initialization to check status
(defun ollama-buddy--open-chat ()
  "Open chat buffer and initialize if needed."
  (interactive)
  (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
  (ollama-buddy--initialize-chat-buffer)
  (goto-char (point-max)))

(defun ollama-buddy--menu-help-assistant ()
  "Show the help assistant."
  (interactive)
  (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
  (goto-char (point-max))
  (when (re-search-backward ">> PROMPT:\\s-*" nil t)
    (beginning-of-line)
    (skip-chars-backward "\n")
    (delete-region (point) (point-max)))
  (insert (ollama-buddy--create-intro-message))
  (ollama-buddy--prepare-prompt-area))

(defun ollama-buddy--menu-custom-prompt ()
  "Show the custom prompt."
  (interactive)
  (when-let ((prompt (read-string "Enter prompt prefix on selection: " nil nil nil t)))
    (unless (use-region-p)
      (user-error "No region selected.  Select text to use with prompt"))
    (unless (not (string-empty-p prompt))
      (user-error "Input string is empty"))
    (let* ((prompt-with-selection (concat
                                   (when prompt (concat prompt "\n\n"))
                                   (buffer-substring-no-properties
                                    (region-beginning) (region-end)))))
      (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
        (pop-to-buffer (current-buffer))
        (ollama-buddy--prepare-prompt-area)
        (goto-char (point-max))
        (insert (string-trim prompt-with-selection)))
      (ollama-buddy--send (string-trim prompt-with-selection)))))

(defun ollama-buddy--menu-minibuffer-prompt ()
  "Show the custom minibuffer prompt."
  (interactive)
  (when-let ((prompt (read-string "Enter prompt: " nil nil nil t)))
    (unless (not (string-empty-p prompt))
      (user-error "Input string is empty"))
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (pop-to-buffer (current-buffer))
      (ollama-buddy--prepare-prompt-area)
      (goto-char (point-max))
      (insert (string-trim prompt)))
    (ollama-buddy--send (string-trim prompt))))

(defun ollama-buddy--get-command-def (command-name)
  "Get command definition for COMMAND-NAME."
  (assoc command-name ollama-buddy-command-definitions))

(defun ollama-buddy--get-command-prop (command-name prop)
  "Get property PROP from command COMMAND-NAME."
  (plist-get (cdr (ollama-buddy--get-command-def command-name)) prop))

(defun ollama-buddy--text-after-prompt ()
  "Get the text after the prompt:"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward ">> PROMPT:\\s-*" nil t)
        (progn
          (search-forward ":")
          (string-trim (buffer-substring-no-properties
                        (point) (point-max))))
      "")))

(defun ollama-buddy--send-with-command (command-name)
  "Send request using configuration from COMMAND-NAME."
  (let* ((prompt-text (ollama-buddy--get-command-prop command-name :prompt))
         (selected-text (when (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning) (region-end))))
         (model (ollama-buddy--get-command-prop command-name :model)))
    
    ;; Verify requirements
    (when (and prompt-text (not selected-text))
      (user-error "This command requires selected text"))
    
    ;; Prepare and send the prompt
    (let ((full-prompt (ollama-buddy--prepare-command-prompt command-name selected-text)))
      (ollama-buddy--send (string-trim full-prompt) model))))

(defun ollama-buddy--send (&optional prompt specified-model)
  "Send PROMPT with optional SYSTEM-PROMPT and SPECIFIED-MODEL."
  ;; Check status and update UI if offline
  (unless (ollama-buddy--check-status)
    (ollama-buddy--update-status "OFFLINE")
    (user-error "Ensure Ollama is running"))

  (unless (> (length prompt) 0)
    (user-error "Ensure prompt is defined"))

  (let* ((model-info (ollama-buddy--get-valid-model specified-model))
         (model (car model-info))
         (original-model (cdr model-info))
         (messages (ollama-buddy--get-history-for-request))
         ;; Add the current prompt to the messages
         (messages (append messages `(((role . "user")
                                       (content . ,prompt)))))
         ;; Include temperature in the payload
         (payload (json-encode
                   `((model . ,model)
                     (messages . ,(vconcat [] messages))
                     (stream . t)
                     (options . ((temperature . ,ollama-buddy--current-temperature)))))))
    
    (setq ollama-buddy--current-model model)
    (setq ollama-buddy--current-prompt prompt)
    
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (pop-to-buffer (current-buffer))
      (goto-char (point-max))
      (unless (> (buffer-size) 0)
        (insert (ollama-buddy--create-intro-message)))
      
      ;; Add model info to response header
      (insert (propertize (format "\n\n** [%s: RESPONSE]" model) 'face 
                          `(:inherit bold :foreground ,(ollama-buddy--get-model-color model))) "\n\n")
      
      (when (and original-model model (not (string= original-model model)))
        (insert (propertize (format "[Using %s instead of %s]" model original-model)
                            'face '(:inherit error :weight bold)) "\n\n"))
      
      ;; Enable visual-line-mode for better text wrapping
      (visual-line-mode 1))

    (ollama-buddy--update-status "Sending request..." original-model model)

    (when (and ollama-buddy--active-process
               (process-live-p ollama-buddy--active-process))
      (set-process-sentinel ollama-buddy--active-process nil)
      (delete-process ollama-buddy--active-process)
      (setq ollama-buddy--active-process nil))
    
    ;; Add error handling for network process creation
    (condition-case err
        (setq ollama-buddy--active-process
              (make-network-process
               :name "ollama-chat-stream"
               :buffer nil
               :host ollama-buddy-host
               :service ollama-buddy-port
               :coding 'utf-8
               :filter #'ollama-buddy--stream-filter
               :sentinel #'ollama-buddy--stream-sentinel))
      (error
       (ollama-buddy--update-status "OFFLINE - Connection failed")
       (error "Failed to connect to Ollama: %s" (error-message-string err))))
    
    (condition-case err
        (process-send-string
         ollama-buddy--active-process
         (concat "POST /api/chat HTTP/1.1\r\n"
                 (format "Host: %s:%d\r\n" ollama-buddy-host ollama-buddy-port)
                 "Content-Type: application/json\r\n"
                 (format "Content-Length: %d\r\n\r\n" (string-bytes payload))
                 payload))
      (error
       (ollama-buddy--update-status "OFFLINE - Send failed")
       (when (and ollama-buddy--active-process
                  (process-live-p ollama-buddy--active-process))
         (delete-process ollama-buddy--active-process))
       (error "Failed to send request to Ollama: %s" (error-message-string err))))))

(defun ollama-buddy--make-request (endpoint method &optional payload)
  "Generic request function for ENDPOINT with METHOD and optional PAYLOAD."
  (when (ollama-buddy--ollama-running)
    (let ((url-request-method method)
          (url-request-extra-headers '(("Content-Type" . "application/json")
                                       ("Connection" . "close")))
          (url (format "http://%s:%d%s"
                       ollama-buddy-host ollama-buddy-port endpoint)))
      (with-temp-buffer
        (if payload
            (let ((url-request-data (encode-coding-string payload 'utf-8)))
              (url-insert-file-contents url))
          (url-insert-file-contents url))
        (json-read-from-string (buffer-string))))))

(defun ollama-buddy--get-models ()
  "Get available Ollama models."
  (when-let ((response (ollama-buddy--make-request "/api/tags" "GET")))
    (mapcar #'car (ollama-buddy--get-models-with-colors))))

(defun ollama-buddy--ollama-running ()
  "Check if Ollama server is running."
  (condition-case nil
      (let ((proc (make-network-process
                   :name "ollama-test"
                   :buffer nil
                   :host ollama-buddy-host
                   :service ollama-buddy-port
                   :nowait nil)))
        (delete-process proc)
        t)
    (error nil)))

(defun ollama-buddy--create-intro-message ()
  "Create welcome message with lettered model assignments in org format."
  (ollama-buddy--assign-model-letters)
  (let* ((models-section
          (when (ollama-buddy--ollama-running)
            (ollama-buddy--format-models-with-letters)))
         (message-text
          (concat
           (when (= (buffer-size) 0)
             (concat "#+TITLE: Ollama Buddy Chat\n"
                     "#+STARTUP: content"))
           "\n\n* Welcome to OLLAMA BUDDY\n"
           " ___ _ _      n _ n      ___       _   _ _ _\n"
           "|   | | |__._|o(Y)o|__._| . |_ _ _| |_| | | |\n"
           "| | | | | .  |     | .  | . | | | . | . |__ |\n"
           "|___|_|_|__/_|_|_|_|__/_|___|___|___|___|___|\n\n"
           "** Available Models\n\n"
           models-section
           "** Quick Tips\n\n"
           "- Ask me anything!          C-c C-c\n"
           "- Change your model?        C-c m\n"
           "- Change your mind?         C-c k\n"
           "- Adjust temperature?       C-c t/T/0\n"
           "    - 0.0 (precise) to 1.0+ (creative)\n" 
           "- Prompt history?           M-p/M-n\n"
           "- In another buffer?        M-x ollama-buddy-menu")))
    (add-face-text-property 0 (length message-text) '(:inherit bold) nil message-text)
    message-text))

(defun ollama-buddy--update-multishot-status ()
  "Update status line to show multishot progress."
  (if ollama-buddy--multishot-sequence
      (let* ((completed (upcase (substring ollama-buddy--multishot-sequence
                                           0 ollama-buddy--multishot-progress)))
             (remaining (substring ollama-buddy--multishot-sequence
                                   ollama-buddy--multishot-progress)))
        (concat (propertize "Multishot: " 'face '(:weight bold))
                (propertize completed 'face '(:weight bold))
                (propertize remaining 'face '(:weight normal))))
    ""))

(defun ollama-buddy--multishot-send (prompt sequence)
  "Send PROMPT to multiple models specified by SEQUENCE of letters."
  ;; Store sequence and prompt for use across multiple calls
  (setq ollama-buddy--multishot-sequence sequence
        ollama-buddy--multishot-prompt prompt
        ollama-buddy--multishot-progress 0)
  ;; reset registers
  (mapc (lambda (ch)
          (set-register ch ""))
        sequence)
  (setq ollama-buddy--current-request-temporary-model ollama-buddy--current-model)
  (ollama-buddy--send-next-in-sequence))

(defun ollama-buddy--send-next-in-sequence ()
  "Send prompt to next model in the multishot sequence."
  (when (and ollama-buddy--multishot-sequence
             ollama-buddy--multishot-prompt
             (< ollama-buddy--multishot-progress
                (length ollama-buddy--multishot-sequence)))
    (let* ((current-letter (aref ollama-buddy--multishot-sequence
                                 ollama-buddy--multishot-progress))
           (model (cdr (assoc current-letter ollama-buddy--model-letters))))
      (when model
        (setq ollama-buddy--current-model model)
        (if (eq ollama-buddy--multishot-progress 0)
            (ollama-buddy--prepare-prompt-area t t)
          (progn
            (ollama-buddy--prepare-prompt-area)
            (insert ollama-buddy--multishot-prompt)))
        (ollama-buddy--send ollama-buddy--multishot-prompt model)))))

(defun ollama-buddy--multishot-prepare ()
  "Prepare for a multishot sequence and return the prompt text."
  (interactive)
  (let* ((prompt-data (ollama-buddy--get-prompt-content))
         (prompt-text (car prompt-data)))
    
    ;; Ensure we have content
    (when (string-empty-p prompt-text)
      (user-error "Please enter a prompt before starting multishot"))
    
    prompt-text))

(defun ollama-buddy--multishot-prompt ()
  "Prompt for and execute multishot sequence."
  (interactive)
  (let* ((prompt-text (ollama-buddy--multishot-prepare))
         (available-letters (mapcar #'car ollama-buddy--model-letters))
         (prompt (concat "Enter model sequence - available ["
                        (apply #'string available-letters) "]: "))
         (sequence (read-string prompt))
         (valid-sequence (cl-remove-if-not 
                         (lambda (c) (memq c available-letters))
                         (string-to-list sequence))))
    
    (when valid-sequence
      (let ((sequence-str (apply #'string valid-sequence)))
        (message "Running multishot with %d models: %s" 
                (length valid-sequence) sequence-str)
        (ollama-buddy--multishot-send prompt-text sequence-str)))))

(defun ollama-buddy--cycle-prompt-history (direction)
  "Cycle through prompt history in DIRECTION (1=forward, -1=backward)."
  (interactive)
  (when ollama-buddy--prompt-history
    (let* ((prompt-data (ollama-buddy--get-prompt-content))
           (prompt-point (cdr prompt-data))
           (current-pos (or (get 'ollama-buddy--cycle-prompt-history 'history-position) 0))
           (new-pos (+ current-pos direction))
           (new-pos (if (< new-pos 0) 
                        0 
                      (min new-pos (1- (length ollama-buddy--prompt-history)))))
           (new-content (nth new-pos ollama-buddy--prompt-history)))
      
      ;; Store position for next cycle
      (put 'ollama-buddy--cycle-prompt-history 'history-position new-pos)
      
      (when prompt-point
        (save-excursion
          (goto-char prompt-point)
          (search-forward ":")
          (delete-region (point) (point-max))
          (insert " " new-content))))))

(defun ollama-buddy-previous-history ()
  "Navigate to previous item in prompt history."
  (interactive)
  (ollama-buddy--cycle-prompt-history 1))

(defun ollama-buddy-next-history ()
  "Navigate to next item in prompt history."
  (interactive)
  (ollama-buddy--cycle-prompt-history -1))

;;;###autoload
(defun ollama-buddy-menu ()
  "Display Ollama Buddy menu."
  (interactive)
  (let ((ollama-status (ollama-buddy--check-status)))  ; Store the status check result
    (ollama-buddy--update-status
     (if ollama-status "Menu opened - Ready" "Menu opened - Ollama offline"))
    (when-let* ((items (mapcar (lambda (cmd-def)
                                 (let* ((key (plist-get (cdr cmd-def) :key))
                                        (desc (plist-get (cdr cmd-def) :description))
                                        (model (plist-get (cdr cmd-def) :model))
                                        (action (plist-get (cdr cmd-def) :action))
                                        ;; Add model indicator if a specific model is used
                                        (desc-with-model
                                         (if model
                                             (let ((color (ollama-buddy--get-model-color model)))
                                               (concat desc " "
                                                       (propertize (concat "[" model "]") 
                                                                   'face `(:inherit bold :foreground ,color))))
                                           desc)))
                                   (cons key (list desc-with-model action))))
                               ollama-buddy-command-definitions))
                (formatted-items
                 (mapcar (lambda (item)
                           (format "[%c] %s" (car item) (cadr item)))
                         items))
                (total (length formatted-items))
                (rows (ceiling (/ total (float ollama-buddy-menu-columns))))
                (padded-items (append formatted-items
                                      (make-list (- (* rows
                                                       ollama-buddy-menu-columns)
                                                    total)
                                                 "")))
                (format-string
                 (mapconcat
                  (lambda (width) (format "%%-%ds" (+ width 2)))
                  (butlast
                   (cl-loop for col below ollama-buddy-menu-columns collect
                            (cl-loop for row below rows
                                     for idx = (+ (* col rows) row)
                                     when (< idx total)
                                     maximize (length (nth idx padded-items)))))
                  ""))
                
                (model (or ollama-buddy--current-model
                           ollama-buddy-default-model "NONE"))
                
                (available-models-text
                 (if (ollama-buddy--ollama-running)
                     (mapconcat (lambda (model)
                                  (propertize model 'face
                                              `(:inherit bold :foreground
                                                         ,(ollama-buddy--get-model-color model))))
                                (ollama-buddy--get-models) " ")
                   "No models available"))
                
                (colored-current-model
                 (propertize model 'face `(:foreground
                                           ,(ollama-buddy--get-model-color 
                                             model)
                                           :weight bold)))
                (prompt
                 (format "%s %s%s\nAvailable: %s\n%s"
                         (if ollama-status "RUNNING" "NOT RUNNING")
                         colored-current-model
                         (if (use-region-p) "" " (NO SELECTION)")
                         available-models-text
                         (mapconcat
                          (lambda (row)
                            (if format-string
                                (apply #'format (concat format-string "%s") row)
                              (car row)))
                          (cl-loop for row below rows collect
                                   (cl-loop for col below ollama-buddy-menu-columns
                                            for idx = (+ (* col rows) row)
                                            when (< idx (length padded-items))
                                            collect (nth idx padded-items)))
                          "\n")))
                (key (read-key prompt))
                (cmd (assoc key items)))
      (funcall (caddr cmd)))))

(defun ollama-buddy-show-model-status ()
  "Display status of models referenced in command definitions with color coding."
  (interactive)
  (let* ((used-models (delete-dups
                       (delq nil
                             (mapcar (lambda (cmd)
                                       (plist-get (cdr cmd) :model))
                                     ollama-buddy-command-definitions))))
         (available-models (ollama-buddy--get-models))
         (buf (get-buffer-create "*Ollama Model Status*")))
    ;; Update model colors
    (when (ollama-buddy--ollama-running)
      (ollama-buddy--update-model-colors))
    
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Ollama Model Status:\n\n")
        
        ;; Display current model with color
        (when ollama-buddy--current-model
          (let ((color (ollama-buddy--get-model-color ollama-buddy--current-model)))
            (insert "Current Model: ")
            (insert (propertize ollama-buddy--current-model 'face `(:foreground ,color :weight bold)))
            (insert "\n")))
        
        ;; Display default model with color
        (when ollama-buddy-default-model
          (let ((color (ollama-buddy--get-model-color ollama-buddy-default-model)))
            (insert "Default Model: ")
            (insert (propertize ollama-buddy-default-model 'face `(:foreground ,color :weight bold)))
            (insert "\n\n")))
        
        ;; Display models used in commands with colors
        (insert "Models used in commands:\n")
        (dolist (model used-models)
          (when model
            (let ((color (ollama-buddy--get-model-color model)))
              (insert "  ")
              (insert (propertize model 'face `(:foreground ,color)))
              (insert (format ": %s\n"
                              (if (member model available-models)
                                  "Available ✓"
                                "Not Available ✗"))))))
        
        ;; List available models with colors
        (insert "\nAvailable Models:\n")
        (dolist (model available-models)
          (let ((color (ollama-buddy--get-model-color model)))
            (insert "  ")
            (insert (propertize model 'face `(:foreground ,color)))
            (insert "\n")))))
    (display-buffer buf)))

;;;###autoload
(defun ollama-buddy-enable-monitor ()
  "Enable background connection monitoring."
  (interactive)
  (setq ollama-buddy-enable-background-monitor t)
  (unless ollama-buddy--connection-timer
    (setq ollama-buddy--connection-timer
          (run-with-timer 0 ollama-buddy-connection-check-interval
                          #'ollama-buddy--monitor-connection))))

;;;###autoload
(defun ollama-buddy-disable-monitor ()
  "Disable background connection monitoring."
  (interactive)
  (setq ollama-buddy-enable-background-monitor nil)
  (when ollama-buddy--connection-timer
    (cancel-timer ollama-buddy--connection-timer)
    (setq ollama-buddy--connection-timer nil)))

(defun ollama-buddy--send-prompt ()
  "Send the current prompt to a LLM."
  (interactive)
  (let* ((prompt-data (ollama-buddy--get-prompt-content))
         (prompt-text (car prompt-data))
         (model (or ollama-buddy--current-model
                   ollama-buddy-default-model
                   "Default:latest")))
    
    ;; Add to history if non-empty
    (when (and prompt-text (not (string-empty-p prompt-text)))
      (put 'ollama-buddy--cycle-prompt-history 'history-position -1)
      (add-to-history 'ollama-buddy--prompt-history prompt-text))
    
    ;; Reset multishot variables
    (setq ollama-buddy--multishot-sequence nil
          ollama-buddy--multishot-prompt nil)
    
    (ollama-buddy--send prompt-text model)))

(defun ollama-buddy--cancel-request ()
  "Cancel the current request and clean up resources."
  (interactive)
  (when ollama-buddy--active-process
    (delete-process ollama-buddy--active-process)
    (setq ollama-buddy--active-process nil))
  
  ;; Clean up token tracking
  (when ollama-buddy--token-update-timer
    (cancel-timer ollama-buddy--token-update-timer)
    (setq ollama-buddy--token-update-timer nil))

  ;; Reset token tracking variables
  (setq ollama-buddy--current-token-count 0
        ollama-buddy--current-token-start-time nil
        ollama-buddy--last-token-count 0
        ollama-buddy--last-update-time nil)
  
  ;; Update status to show cancelled
  (ollama-buddy--update-status "Cancelled"))

(defvar ollama-buddy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ollama-buddy--send-prompt)
    (define-key map (kbd "C-c l") #'ollama-buddy--multishot-prompt)
    (define-key map (kbd "C-c k") #'ollama-buddy--cancel-request)
    (define-key map (kbd "C-c m") #'ollama-buddy--swap-model)
    ;; Improved history navigation
    (define-key map (kbd "M-p") #'ollama-buddy-previous-history)
    (define-key map (kbd "M-n") #'ollama-buddy-next-history)
    ;; Temperature controls
    (define-key map (kbd "C-c t") #'ollama-buddy-set-temperature)
    (define-key map (kbd "C-c T") #'ollama-buddy-temperature-preset)
    (define-key map (kbd "C-c 0") #'ollama-buddy-reset-temperature)
    (define-key map (kbd "C-c C-o") #'ollama-buddy-toggle-markdown-conversion)
    map)
  "Keymap for ollama-buddy mode.")

(define-minor-mode ollama-buddy-mode
  "Minor mode for ollama-buddy keybindings."
  :lighter " OB"
  :keymap ollama-buddy-mode-map)

(push 'ollama-buddy--prompt-history savehist-additional-variables)

(provide 'ollama-buddy)
;;; ollama-buddy.el ends here
