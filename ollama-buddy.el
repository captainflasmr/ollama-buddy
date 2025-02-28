;;; ollama-buddy.el --- Ollama Buddy: Your Friendly AI Assistant -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.2.4
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

(defgroup ollama-buddy nil
  "Customization group for Ollama Buddy."
  :group 'applications
  :prefix "ollama-buddy-")

(defcustom ollama-buddy-enable-model-colors nil
  "Whether to show model colors. EXPERIMNTAL."
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

(defcustom ollama-buddy-menu-columns 4
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

(defconst ollama-buddy--separators
  '((header . "------------------ o( Y )o ------------------")
    (response . "------------------ @( Y )@ ------------------"))
  "Separators for chat display.")

(defcustom ollama-buddy-connection-check-interval 5
  "Interval in seconds to check Ollama connection status."
  :type 'integer
  :group 'ollama-buddy)

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

;; Function to update and retrieve model colors
(defun ollama-buddy--update-model-colors ()
  "Update the model colors hash table and return it."
  (let ((models-with-colors (ollama-buddy--get-models-with-colors)))
    (dolist (pair models-with-colors)
      (puthash (car pair) (cdr pair) ollama-buddy--model-colors))
    ollama-buddy--model-colors))

;; Get color for a specific model
(defun ollama-buddy--get-model-color (model)
  "Get the color associated with MODEL, or return the default foreground color if disabled."
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

(defun ollama-buddy--hash-string-to-color (str)
  "Generate a consistent color based on the hash of STR."
  (let* ((hash (abs (sxhash str)))
         ;; Generate HSL values - keeping saturation and lightness fixed for readability
         (hue (mod hash 360))
         (saturation 70)
         (lightness 60))
    ;; Convert HSL to hex color
    (apply #'color-rgb-to-hex
           (color-hsl-to-rgb (/ hue 360.0) (/ saturation 100.0) (/ lightness 100.0)))))

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
      (concat "Models available:\n\n"
              (mapconcat
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
    (let ((model (or ollama-buddy--current-model
                     ollama-buddy-default-model
                     "No Model")))
      (setq header-line-format
            (concat
             (format (if (string-empty-p (ollama-buddy--update-multishot-status))
                         " [%s%s %s: %s]"
                       " [%s %s %s: %s]")
                     (ollama-buddy--update-multishot-status)
                     (propertize (if (ollama-buddy--check-status) "RUNNING" "OFFLINE")
                                 'face '(:weight bold))
                     (propertize model 'face
                                 `(:weight bold
                                           :foreground ,(ollama-buddy--get-model-color
                                                         model)))
                     (propertize status 'face '(:weight bold)))
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
      (ollama-buddy-mode 1)
      (ollama-buddy--check-status)
      (insert (ollama-buddy--create-intro-message))
      (ollama-buddy--show-prompt))
    (ollama-buddy--update-status "Idle")))

(defun ollama-buddy--stream-filter (_proc output)
  "Process stream OUTPUT while preserving cursor position."
  (when-let* ((json-str (replace-regexp-in-string "^[^\{]*" "" output))
              (json-data (and (> (length json-str) 0) (json-read-from-string json-str)))
              (text (alist-get 'content (alist-get 'message json-data))))
    
    (if (not (string-empty-p text))
        (ollama-buddy--update-status "Processing..."))
    
    (with-current-buffer ollama-buddy--chat-buffer
      (let* ((inhibit-read-only t)
             (window (get-buffer-window ollama-buddy--chat-buffer t))
             (old-point (and window (window-point window)))
             (at-end (and window (>= old-point (point-max))))
             (old-window-start (and window (window-start window))))
        (save-excursion
          (goto-char (point-max))
          (insert text)

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
            (insert "\n\n")
            (insert (propertize "[" 'face '(:inherit bold)))
            (insert (propertize ollama-buddy--current-model 'face `(:inherit bold)))
            (insert (propertize ": FINISHED]" 'face '(:inherit bold)))
            
            ;; Handle multishot progression here
            (if ollama-buddy--multishot-sequence
                (progn
                  ;; Increment progress
                  (setq ollama-buddy--multishot-progress
                        (1+ ollama-buddy--multishot-progress))
                  ;; Check if there are more models to process
                  (if (< ollama-buddy--multishot-progress
                         (length ollama-buddy--multishot-sequence))
                      ;; Process next model after a short delay
                      (run-with-timer 0.5 nil
                                      (lambda ()
                                        (let* ((current-letter
                                                (aref ollama-buddy--multishot-sequence
                                                      ollama-buddy--multishot-progress))
                                               (next-model
                                                (cdr (assoc current-letter
                                                            ollama-buddy--model-letters))))
                                          (when next-model
                                            (ollama-buddy--send
                                             ollama-buddy--multishot-prompt
                                             next-model)))))
                    ;; End of sequence
                    (progn
                      (ollama-buddy--update-status "Multi Finished")
                      (ollama-buddy--show-prompt))))
              ;; Not in multishot mode, just show the prompt
              (progn
                (ollama-buddy--show-prompt)
                (ollama-buddy--update-status "Finished")))))
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
    (setq ollama-buddy--multishot-sequence nil
          ollama-buddy--multishot-prompt nil)
    (with-current-buffer ollama-buddy--chat-buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize msg 'face '(:weight bold)))
        (ollama-buddy--show-prompt)))
    (ollama-buddy--update-status (concat "Stream " status))))

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
      (ollama-buddy--show-prompt)
      (goto-char (point-max))
      (ollama-buddy--update-status "Idle"))))

;; Update buffer initialization to check status
(defun ollama-buddy--open-chat ()
  "Open chat buffer and initialize if needed."
  (interactive)
  (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
  (ollama-buddy--initialize-chat-buffer)
  (goto-char (point-max)))

(defcustom ollama-buddy-command-definitions
  '((open-chat
     :key ?o
     :description "Open chat buffer"
     :action ollama-buddy--open-chat)
    
    (show-models
     :key ?v
     :description "View model status"
     :action ollama-buddy-show-model-status)

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
    
    (swap-model
     :key ?m
     :description "Swap model"
     :action ollama-buddy--swap-model)
    
    (help
     :key ?h
     :description "Help assistant"
     :action (lambda ()
               (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
               (goto-char (point-max))
               (insert (ollama-buddy--create-intro-message))
               (ollama-buddy--show-prompt)))
    
    (send-region
     :key ?l
     :description "Send region"
     :action (lambda () (ollama-buddy--send-with-command 'send-region)))
    
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
    
    (custom-prompt
     :key ?e
     :description "Custom prompt"
     :action (lambda ()
               (when-let ((prefix (read-string "Enter prompt prefix: " nil nil nil t)))
                 (unless (use-region-p)
                   (user-error "No region selected.  Select text to use with prompt"))
                 (unless (not (string-empty-p prefix))
                   (user-error "Input string is empty"))
                 (ollama-buddy--send
                  (concat prefix "\n\n"
                          (buffer-substring-no-properties
                           (region-beginning) (region-end)))))))
    
    (minibuffer-prompt
     :key ?i
     :description "Minibuffer Prompt"
     :action (lambda ()
               (when-let ((prefix (read-string "Enter prompt: " nil nil nil t)))
                 (unless (not (string-empty-p prefix))
                   (user-error "Input string is empty"))
                 (ollama-buddy--send prefix))))
    
    (save-chat
     :key ?s
     :description "Save chat"
     :action (lambda ()
               (with-current-buffer ollama-buddy--chat-buffer
                 (write-region (point-min) (point-max)
                               (read-file-name "Save conversation to: ")
                               'append-to-file
                               nil))))
    (kill-request
     :key ?x
     :description "Kill request"
     :action (lambda ()
               (delete-process ollama-buddy--active-process)))

    (toggle-colors
     :key ?C
     :description "Toggle Colors"
     :action ollama-buddy-toggle-model-colors)
    
    (quit
     :key ?q
     :description "Quit"
     :action (lambda () (message "Quit Ollama Shell menu."))))
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

(defun ollama-buddy--get-command-def (command-name)
  "Get command definition for COMMAND-NAME."
  (assoc command-name ollama-buddy-command-definitions))

(defun ollama-buddy--get-command-prop (command-name prop)
  "Get property PROP from command COMMAND-NAME."
  (plist-get (cdr (ollama-buddy--get-command-def command-name)) prop))

(defun ollama-buddy--get-prompt-history-element ()
  "Through the minibuffer, bring up the prompt history."
  (interactive)
  (when ollama-buddy--prompt-history
    (let* ((bounds (save-excursion
                     (search-backward ">> PROMPT:")
                     (search-forward ": ")
                     (point)))
           (current-input (buffer-substring-no-properties bounds (point)))
           (input
            (read-from-minibuffer
             "Ollama Buddy: " nil nil nil 'ollama-buddy--prompt-history)))
      (when input
        (delete-region bounds (point))
        (insert input)))))

(defun ollama-buddy--show-prompt ()
  "Show the prompt with optionally a MODEL."
  (interactive)
  (let* ((model (or ollama-buddy--current-model
                    ollama-buddy-default-model
                    "Default:latest"))
         (color (ollama-buddy--get-model-color model)))
    (insert (format "\n\n%s\n\n%s %s"
                    (propertize (alist-get 'header ollama-buddy--separators) 'face '(:inherit bold))
                    (propertize model 'face `(:foreground ,color :weight bold))
                    (propertize ">> PROMPT: " 'face '(:inherit bold))))))

(defun ollama-buddy--send-with-command (command-name)
  "Send request using configuration from COMMAND-NAME."
  (let* ((prompt (or (ollama-buddy--get-command-prop command-name :prompt))))
    (when (and prompt (not (use-region-p)))
      (user-error "No region selected.  Select text to use with prompt"))
    (let* ((prompt-with-selection (concat
                                   (when prompt (concat prompt "\n\n"))
                                   (if (use-region-p)
                                       (buffer-substring-no-properties
                                        (region-beginning) (region-end))
                                     "")))
           (model (ollama-buddy--get-command-prop command-name :model)))
      (ollama-buddy--send (string-trim prompt-with-selection) model))))

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
         (payload (json-encode
                   `((model . ,model)
                     (messages . [((role . "user")
                                   (content . ,prompt))])
                     (stream . t)))))
    (setq ollama-buddy--current-model model)
    
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (pop-to-buffer (current-buffer))
      (goto-char (point-max))
      (unless (> (buffer-size) 0)
        (insert (ollama-buddy--create-intro-message)))
      (insert (format "\n\n%s\n\n%s %s\n\n%s\n\n"
                      (propertize (alist-get 'header ollama-buddy--separators) 'face '(:inherit bold))
                      (propertize "[User: PROMPT]" 'face '(:inherit bold))
                      prompt
                      (propertize (concat "[" model ": RESPONSE]") 'face
                                  `(:inherit bold :foreground ,(ollama-buddy--get-model-color 
                                                                model)))))
      (when (and original-model model (not (string= original-model model)))
        (insert (propertize (format "[Using %s instead of %s]" model original-model)
                            'face '(:inherit error :weight bold)) "\n\n"))
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
  "Create welcome message with lettered model assignments."
  (ollama-buddy--assign-model-letters)
  (let* ((models-section
          (when (ollama-buddy--ollama-running)
            (ollama-buddy--format-models-with-letters)))
         (message-text
          (concat
           "\n\n"
           (alist-get 'header ollama-buddy--separators)
           "\n"
           " ___ _ _      n _ n      ___       _   _ _ _\n"
           "|   | | |__._|o(Y)o|__._| . |_ _ _| |_| | | |\n"
           "| | | | | .  |     | .  | . | | | . | . |__ |\n"
           "|___|_|_|__/_|_|_|_|__/_|___|___|___|___|___|\n\n"
           "Hi there! and welcome to OLLAMA BUDDY!\n\n"
           models-section
           "Quick Tips:\n\n"
           "- Ask me anything!   C-c C-c to send\n"
           "- Multi-model shot?  C-c C-l\n"
           "- Change your mind?  C-c C-k to cancel\n"
           "- Change your model? C-c C-m\n"
           "- Prompt history?    M-p, minibuffer M-p/M-n\n"
           "- In another buffer? M-x ollama-buddy-menu")))
    (add-face-text-property 0 (length message-text) '(:inherit bold) nil message-text)
    message-text))

(defun ollama-buddy--update-multishot-status ()
  "Update status line to show multishot progress."
  (if ollama-buddy--multishot-sequence
    (let* ((completed (substring ollama-buddy--multishot-sequence
                                 0 ollama-buddy--multishot-progress))
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
  ;; Start with the first model in sequence
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
        (ollama-buddy--update-status "Multi Start")
        (ollama-buddy--send ollama-buddy--multishot-prompt model)))))

(defun ollama-buddy--multishot-prompt ()
  "Prompt for and execute multishot sequence."
  (interactive)
  (let* ((available-letters (mapcar #'car ollama-buddy--model-letters))
         (prompt (concat
                  "Enter model sequence - available ["
                  available-letters "]"))
         (input-chars nil)
         char)
    (while (progn
             (setq char (read-key prompt))
             (and char
                  (not (eq char ?\r))
                  (not (eq char ?\n))
                  (memq char available-letters)))
      (push char input-chars)
      (setq prompt (concat "Enter model sequence: "
                           (concat (reverse input-chars)))))
    (when input-chars
      (let* ((sequence (concat (reverse input-chars)))
             (bounds (save-excursion
                       (search-backward ">> PROMPT:")
                       (forward-char 10)
                       (point)))
             (query-text (string-trim
                          (buffer-substring-no-properties bounds (point)))))
        (ollama-buddy--multishot-send query-text sequence)))))

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

(defun ollama-buddy-pull-model ()
  "Pull a model from Ollama interactively."
  (interactive)
  (let ((model (read-string "Enter model name to pull (e.g., llama2:latest): ")))
    (ollama-buddy--make-request "/api/pull" "POST"
                                (json-encode `((name . ,model))))
    (message "Pulling model %s... Check server logs for progress." model)))

(defun ollama-buddy--send-prompt ()
  "Send the current prompt to a LLM.."
  (interactive)
  (let* ((bounds (save-excursion
                   (search-backward ">> PROMPT:")
                   (search-forward ":")
                   (point)))
         (model (or ollama-buddy--current-model
                    ollama-buddy-default-model
                    "Default:latest"))
         (query-text (string-trim (buffer-substring-no-properties bounds (point)))))
    
    ;; Add to history if non-empty
    (when (and query-text (not (string-empty-p query-text)))
      (add-to-history 'ollama-buddy--prompt-history query-text))
    
    (setq ollama-buddy--multishot-sequence nil
          ollama-buddy--multishot-prompt nil)
    (ollama-buddy--send query-text model)))

(defun ollama-buddy--cancel-request ()
  "Cancel the current request."
  (interactive)
  (delete-process ollama-buddy--active-process))

(defvar ollama-buddy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ollama-buddy--send-prompt)
    (define-key map (kbd "C-c C-l") #'ollama-buddy--multishot-prompt)
    (define-key map (kbd "C-c C-k") #'ollama-buddy--cancel-request)
    (define-key map (kbd "C-c C-m") #'ollama-buddy--swap-model)
    (define-key map (kbd "M-p") #'ollama-buddy--get-prompt-history-element)
    map)
  "Keymap for ollama-buddy mode.")

(define-minor-mode ollama-buddy-mode
  "Minor mode for ollama-buddy keybindings."
  :lighter " OB"
  :keymap ollama-buddy-mode-map)

(provide 'ollama-buddy)
;;; ollama-buddy.el ends here
