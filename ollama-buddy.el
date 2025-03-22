;;; ollama-buddy.el --- Ollama Buddy: Your Friendly AI Assistant -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.9.10
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
;; Ollama Buddy is an Emacs package that provides a friendly AI assistant
;; for various tasks such as code refactoring, generating commit messages,
;; dictionary lookups, and more.  It interacts with the Ollama server to
;; perform these tasks.
;;
;;; Quick Start
;;
;; (use-package ollama-buddy
;;    :ensure t
;;    :bind ("C-c o" . ollama-buddy-menu))
;;
;; OR (to select the default model)
;;
;; (use-package ollama-buddy
;;    :ensure t
;;    :bind ("C-c o" . ollama-buddy-menu)
;;    :custom ollama-buddy-default-model "llama:latest")
;;
;; OR (use-package local)
;;
;; (use-package ollama-buddy
;;    :load-path "path/to/ollama-buddy"
;;    :bind ("C-c o" . ollama-buddy-menu)
;;    :custom ollama-buddy-default-model "llama:latest")
;;
;; OR (the old way)
;;
;; (add-to-list 'load-path "path/to/ollama-buddy")
;; (require 'ollama-buddy)
;; (global-set-key (kbd "C-c o") #'ollama-buddy-menu)
;; (setq ollama-buddy-default-model "llama:latest")
;;
;;; Usage
;;
;; M-x ollama-buddy-menu / C-c o
;;
;; and the chat assistant buffer is presented and off you go!
;;
;;; Code:

(require 'json)
(require 'subr-x)
(require 'url)
(require 'cl-lib)
(require 'dired)
(require 'org)
(require 'savehist)
(require 'color)
(require 'ollama-buddy-core)
(require 'ollama-buddy-transient)

(defun ollama-buddy-display-token-graph ()
  "Display a visual graph of token usage statistics."
  (interactive)
  (let ((buf (get-buffer-create "*Ollama Token Graph*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Token Usage Graph\n\n")
        
        (if (null ollama-buddy--token-usage-history)
            (insert "No token usage data available yet.")
          
          ;; Group by model
          (let ((model-data (make-hash-table :test 'equal)))
            (dolist (info ollama-buddy--token-usage-history)
              (let* ((model (plist-get info :model))
                     (tokens (plist-get info :tokens))
                     (rate (plist-get info :rate))
                     (model-stats (gethash model model-data nil)))
                (unless model-stats
                  (setq model-stats (list :tokens 0 :count 0 :rates nil)))
                (plist-put model-stats :tokens (+ (plist-get model-stats :tokens) tokens))
                (plist-put model-stats :count (1+ (plist-get model-stats :count)))
                (plist-put model-stats :rates
                           (cons rate (plist-get model-stats :rates)))
                (puthash model model-stats model-data)))
            
            ;; Display token count graph
            (insert "=== Token Count by Model ===\n\n")
            (let* ((models nil)
                   (max-tokens 0)
                   (max-model-len 0))
              
              ;; Gather data
              (maphash (lambda (model stats)
                         (push model models)
                         (setq max-tokens (max max-tokens (plist-get stats :tokens)))
                         (setq max-model-len (max max-model-len (length model))))
                       model-data)
              
              ;; Sort models by token count
              (setq models (sort models (lambda (a b)
                                          (> (plist-get (gethash a model-data) :tokens)
                                             (plist-get (gethash b model-data) :tokens)))))
              
              ;; Display bar chart
              (dolist (model models)
                (let* ((stats (gethash model model-data))
                       (tokens (plist-get stats :tokens))
                       (count (plist-get stats :count))
                       (color (ollama-buddy--get-model-color model))
                       (bar-width (round (* 50 (/ (float tokens) max-tokens))))
                       (bar (make-string bar-width ?█)))
                  (insert (format (format "%%-%ds │ %%s %%d tokens (%%d responses)\n"
                                          max-model-len)
                                  model
                                  (propertize bar 'face `(:foreground ,color))
                                  tokens count))))
              
              ;; Display token rate graph
              (insert "\n=== Average Token Rate by Model ===\n\n")
              (let ((max-rate 0))
                ;; Find max rate
                (maphash (lambda (_model stats)
                           (let ((rates (plist-get stats :rates)))
                             (when rates
                               (let ((avg (/ (apply #'+ rates) (float (length rates)))))
                                 (setq max-rate (max max-rate avg))))))
                         model-data)
                
                ;; Display bar chart
                (dolist (model models)
                  (let* ((stats (gethash model model-data))
                         (rates (plist-get stats :rates))
                         (avg-rate (if rates (/ (apply #'+ rates) (float (length rates))) 0))
                         (color (ollama-buddy--get-model-color model))
                         (bar-width (round (* 50 (/ avg-rate max-rate))))
                         (bar (make-string bar-width ?█)))
                    (insert (format (format "%%-%ds │ %%s %%.1f tokens/sec\n"
                                            max-model-len)
                                    model
                                    (propertize bar 'face `(:foreground ,color))
                                    avg-rate)))))))))
      (goto-char (point-min))
      (view-mode 1))
    (display-buffer buf)))

(defun ollama-buddy-history-edit ()
  "Edit the conversation history in a buffer."
  (interactive)
  (cond
   ((= (prefix-numeric-value current-prefix-arg) 4)
    (call-interactively 'ollama-buddy-history-edit-model))
   (t
    (let ((buf (get-buffer-create ollama-buddy--history-edit-buffer)))
      (with-current-buffer buf
        (erase-buffer)
        (emacs-lisp-mode)
        (visual-line-mode 1)
        
        (when (= (hash-table-count ollama-buddy--conversation-history-by-model) 0)
          (insert "No conversation history available for any model. "))
        
        ;; Convert the hashtable to an alist for easier editing
        (let ((history-alist nil))
          (maphash (lambda (k v)
                   (push (cons k v) history-alist))
                 ollama-buddy--conversation-history-by-model)
        
        ;; Insert the pretty-printed history
        (let ((print-level nil)
              (print-length nil))
          (pp (nreverse history-alist) (current-buffer))))
      
      ;; Set up local keys for saving or canceling
      (use-local-map (copy-keymap emacs-lisp-mode-map))
      (local-set-key (kbd "C-c C-c") 'ollama-buddy-history-save)
      (local-set-key (kbd "C-c C-k") 'ollama-buddy-history-cancel)
      
      ;; Set buffer-local variables to identify this as a history edit buffer
      (setq-local ollama-buddy-editing-history t)
      (setq header-line-format "Edit history and press C-c C-c to save, C-c C-k to cancel"))
    
    ;; Display the buffer
    (pop-to-buffer buf)
    (goto-char (point-min))
    (message "Edit history and press C-c C-c to save, C-c C-k to cancel")))))

(defun ollama-buddy-history-save ()
  "Save the edited history back to `ollama-buddy--conversation-history-by-model'."
  (interactive)
  (unless (and (boundp 'ollama-buddy-editing-history)
               ollama-buddy-editing-history)
    (user-error "Not in an Ollama history edit buffer"))
  
  (condition-case err
      (let ((edited-history (read (buffer-string))))
        ;; Validate the edited history
        (unless (listp edited-history)
          (user-error "Invalid history format: must be an alist"))
        
        ;; Clear existing history
        (clrhash ollama-buddy--conversation-history-by-model)
        
        ;; Add the new entries
        (dolist (entry edited-history)
          (let ((model (car entry))
                (msgs (cdr entry)))
            (puthash model msgs ollama-buddy--conversation-history-by-model)))
        
        ;; Update current conversation history for backward compatibility
        (when ollama-buddy--current-model
          (setq ollama-buddy--conversation-history
                (gethash ollama-buddy--current-model
                         ollama-buddy--conversation-history-by-model nil)))
        
        ;; Provide feedback and clean up
        (message "History saved successfully")
        (kill-buffer)
        (ollama-buddy--update-status "History Updated"))
    (error
     (message "Error saving history: %s" (error-message-string err)))))

(defun ollama-buddy-history-cancel ()
  "Cancel editing the history."
  (interactive)
  (when (y-or-n-p "Cancel editing? Changes will be lost.  ?")
    (kill-buffer)
    (message "History editing cancelled")))

(defun ollama-buddy-history-edit-model (model)
  "Edit the conversation history for a specific MODEL."
  (interactive
   (list (completing-read "Edit history for model: "
                          (let ((models nil))
                            (maphash (lambda (k _v) (push k models))
                                     ollama-buddy--conversation-history-by-model)
                            models))))
  
  (let ((buf (get-buffer-create ollama-buddy--history-edit-buffer)))
    (with-current-buffer buf
      (erase-buffer)
      (emacs-lisp-mode)
      (visual-line-mode 1)
      (insert (format ";; Ollama Buddy Conversation History Editor for model: %s\n" model))

      (when (= (hash-table-count ollama-buddy--conversation-history-by-model) 0)
        (insert "No conversation history available for any model. "))
              
      ;; Get just this model's history
      (let ((model-history (gethash model ollama-buddy--conversation-history-by-model nil)))
        ;; Insert the pretty-printed history
        (let ((print-level nil)
              (print-length nil))
          (pp model-history (current-buffer))))
      
      ;; Set up local keys for saving or canceling
      (use-local-map (copy-keymap emacs-lisp-mode-map))
      (local-set-key (kbd "C-c C-c")
                     (lambda () (interactive)
                       (ollama-buddy-history-save-model model)))
      (local-set-key (kbd "C-c C-k") 'ollama-buddy-history-cancel)
      
      ;; Set buffer-local variables to identify this as a history edit buffer
      (setq-local ollama-buddy-editing-history t)
      (setq-local ollama-buddy-editing-model model)
      (setq header-line-format
            (format "Edit history for %s and press C-c C-c to save, C-c C-k to cancel" model)))
    
    ;; Display the buffer
    (pop-to-buffer buf)
    (goto-char (point-min))
    (message "Edit history and press C-c C-c to save, C-c C-k to cancel")))

(defun ollama-buddy-history-save-model (model)
  "Save the edited history for MODEL back to variable."
  (interactive)
  (unless (and (boundp 'ollama-buddy-editing-history)
               ollama-buddy-editing-history
               (boundp 'ollama-buddy-editing-model)
               (string= ollama-buddy-editing-model model))
    (user-error "Not editing history for model %s" model))
  
  (condition-case err
      (let ((edited-history (read (buffer-string))))
        ;; Validate the edited history
        (unless (listp edited-history)
          (user-error "Invalid history format: must be a list of message alists"))
        
        ;; Update the specific model's history
        (puthash model edited-history ollama-buddy--conversation-history-by-model)
        
        ;; Update current conversation history for backward compatibility
        (when (and ollama-buddy--current-model
                   (string= ollama-buddy--current-model model))
          (setq ollama-buddy--conversation-history edited-history))
        
        ;; Provide feedback and clean up
        (message "History for %s saved successfully" model)
        (kill-buffer)
        (ollama-buddy--update-status (format "History for %s Updated" model)))
    (error
     (message "Error saving history: %s" (error-message-string err)))))

(defun ollama-buddy-toggle-interface-level ()
  "Toggle between basic and advanced interface levels."
  (interactive)
  (setq ollama-buddy-interface-level
        (if (eq ollama-buddy-interface-level 'basic) 'advanced 'basic))
  (message "Ollama Buddy interface level set to %s"
          (if (eq ollama-buddy-interface-level 'basic) "basic" "advanced"))
  (ollama-buddy--menu-help-assistant))

;;;###autoload
(defun ollama-buddy-update-command-with-params (entry-name &rest props-and-params)
  "Update command ENTRY-NAME with properties and parameters.
PROPS-AND-PARAMS should be property-value pairs,
with an optional :parameters property followed by parameter-value pairs."
  (when-let ((entry (assq entry-name ollama-buddy-command-definitions)))
    (let ((current-plist (cdr entry))
          properties
          parameters)
      
      ;; Split into properties and parameters
      (let ((params-pos (cl-position :parameters props-and-params)))
        (if params-pos
            (progn
              (setq properties (cl-subseq props-and-params 0 params-pos))
              (when (< (+ params-pos 1) (length props-and-params))
                (setq parameters (nth (+ params-pos 1) props-and-params))))
          (setq properties props-and-params)))
      
      ;; Update properties
      (while properties
        (let ((prop (car properties))
              (value (cadr properties)))
          (setq current-plist (plist-put current-plist prop value))
          (setq properties (cddr properties))))
      
      ;; Update parameters if provided
      (when parameters
        (setq current-plist (plist-put current-plist :parameters parameters)))
      
      ;; Update the command definition
      (setf (cdr entry) current-plist)))
  ollama-buddy-command-definitions)

(defun ollama-buddy-add-parameters-to-command (entry-name &rest parameters)
  "Add specific parameters to ENTRY-NAME command.
PARAMETERS should be a plist with parameter names and values."
  (when-let ((entry (assq entry-name ollama-buddy-command-definitions)))
    (let* ((current-plist (cdr entry))
           (current-params (plist-get current-plist :parameters))
           (new-params (if current-params
                          current-params
                        (list))))
      
      ;; Process parameter pairs and add to list
      (cl-loop for (param value) on parameters by #'cddr do
               (push (cons param value) new-params))
      
      ;; Update the command definition
      (setf (cdr entry) (plist-put current-plist :parameters new-params))))
  ollama-buddy-command-definitions)

;;;###autoload
(defun ollama-buddy-update-menu-entry (entry-name &rest props)
  "Update menu entry ENTRY-NAME with property-value pairs in PROPS.
PROPS should be a sequence of property-value pairs."
  (when-let ((entry (assq entry-name ollama-buddy-command-definitions)))
    (let ((current-plist (cdr entry)))
      (while props
        (let ((prop (car props))
              (value (cadr props)))
          (setq current-plist (plist-put current-plist prop value))
          (setq props (cddr props))))
      (setf (cdr entry) current-plist)))
  ollama-buddy-command-definitions)

(defun ollama-buddy-params-reset ()
  "Reset all parameters to default values and clear modification tracking."
  (interactive)
  (setq ollama-buddy-params-active (copy-tree ollama-buddy-params-defaults)
        ollama-buddy-params-modified nil)
  (ollama-buddy--update-status "Params Reset")
  (message "Ollama parameters reset to defaults"))

(defun ollama-buddy-params-edit (param)
  "Edit a specific parameter PARAM interactively."
  (interactive
   (list (intern (completing-read "Select parameter to edit: "
                                  (mapcar (lambda (pair) (symbol-name (car pair)))
                                          ollama-buddy-params-active)
                                  nil t))))
  (let* ((current-value (alist-get param ollama-buddy-params-active))
         (default-value (alist-get param ollama-buddy-params-defaults))
         (param-type (type-of current-value))
         (prompt (format "Set %s (%s, default: %s): " param param-type default-value))
         (new-value
          (cond
           ((eq param-type 'integer)
            (read-number prompt current-value))
           ((eq param-type 'float)
            (read-number prompt current-value))
           ((eq param-type 'boolean)
            (y-or-n-p (format "Enable %s? " param)))
           ((vectorp current-value)
            (let ((items (split-string
                          (read-string
                           (format "Enter stop sequences (comma-separated): %s"
                                   (mapconcat #'identity current-value ","))
                           nil nil (mapconcat #'identity current-value ","))
                          "," t "\\s-*")))
              (vconcat [] items)))
           (t (read-string prompt (format "%s" current-value))))))
    
    ;; Track whether this parameter is being modified or reset to default
    (if (equal new-value default-value)
        (setq ollama-buddy-params-modified
              (delete param ollama-buddy-params-modified))
      (add-to-list 'ollama-buddy-params-modified param))
    
    ;; Update the parameter value
    (setf (alist-get param ollama-buddy-params-active) new-value)
    (ollama-buddy--update-status "Params changed")
    (message "Updated %s to %s%s"
             param
             new-value
             (if (equal new-value default-value)
                 " (default value)"
               ""))))

(defun ollama-buddy-params-display ()
  "Display the current Ollama parameter settings."
  (interactive)
  (let ((buf (get-buffer-create "*Ollama Parameters*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Ollama API Parameters:\n\n")
        
        ;; Group parameters into categories for better organization
        (let ((generation-params '(temperature top_k top_p min_p typical_p
                                               repeat_last_n repeat_penalty presence_penalty
                                               frequency_penalty mirostat mirostat_tau mirostat_eta
                                               penalize_newline stop))
              (resource-params '(num_keep seed num_predict numa num_ctx num_batch
                                          num_gpu main_gpu low_vram vocab_only use_mmap
                                          use_mlock num_thread)))
          
          ;; Display generation parameters
          (insert "Generation Parameters:\n")
          (insert "=====================\n")
          (dolist (param generation-params)
            (when-let ((value (alist-get param ollama-buddy-params-active))
                       (default-value (alist-get param ollama-buddy-params-defaults)))
              (let ((modified-marker (if (memq param ollama-buddy-params-modified)
                                         (propertize "* " 'face '(:foreground "red"))
                                       "  "))
                    (value-display (cond
                                    ((vectorp value) (format "[%s]"
                                                             (mapconcat #'identity value ", ")))
                                    (t value))))
                (insert (format "%s%-20s: %s%s\n"
                                modified-marker
                                param
                                value-display
                                (if (equal value default-value)
                                    ""
                                  (format " (default: %s)" default-value)))))))
          
          ;; Display resource parameters
          (insert "\nResource Parameters:\n")
          (insert "===================\n")
          (dolist (param resource-params)
            (when-let ((value (alist-get param ollama-buddy-params-active))
                       (default-value (alist-get param ollama-buddy-params-defaults)))
              (let ((modified-marker (if (memq param ollama-buddy-params-modified)
                                         (propertize "* " 'face '(:foreground "red"))
                                       "  ")))
                (insert (format "%s%-20s: %s%s\n"
                                modified-marker
                                param
                                value
                                (if (equal value default-value)
                                    ""
                                  (format " (default: %s)" default-value)))))))))
      
      ;; Display modified parameter count
      (insert (format "\n%d parameters will be sent to Ollama%s\n"
                      (length ollama-buddy-params-modified)
                      (if (zerop (length ollama-buddy-params-modified))
                          " (all default values)"
                        (format ": %s" (mapcar #'symbol-name ollama-buddy-params-modified))))))
    (display-buffer buf)))

(defun ollama-buddy-toggle-params-in-header ()
  "Toggle display of modified parameters in the header line."
  (interactive)
  (setq ollama-buddy-show-params-in-header
        (not ollama-buddy-show-params-in-header))
  (ollama-buddy--update-status ollama-buddy--status)
  (message "Parameters in header: %s"
           (if ollama-buddy-show-params-in-header "enabled" "disabled")))

(defun ollama-buddy-set-suffix ()
  "Set the current prompt as a suffix."
  (interactive)
  (let* ((prompt-data (ollama-buddy--get-prompt-content))
         (prompt-text (car prompt-data)))
    
    ;; Add to history if non-empty
    (when (and prompt-text (not (string-empty-p prompt-text)))
      (put 'ollama-buddy--cycle-prompt-history 'history-position -1)
      (add-to-history 'ollama-buddy--prompt-history prompt-text))
    
    ;; Set as suffix
    (setq ollama-buddy--current-suffix prompt-text)
    
    ;; Update the UI to reflect the change
    (ollama-buddy--prepare-prompt-area t t nil t)
    (ollama-buddy--prepare-prompt-area nil nil)
    
    ;; Update status to show suffix is set
    (ollama-buddy--update-status "Suffix set")
    (message "Suffix set: %s"
             (if (> (length prompt-text) 50)
                 (concat (substring prompt-text 0 47) "...")
               prompt-text))))

(defun ollama-buddy-reset-suffix ()
  "Reset the suffix to default (none)."
  (interactive)
  (setq ollama-buddy--current-suffix nil)
  
  ;; Update the UI to reflect the change
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (ollama-buddy--prepare-prompt-area t))
  
  ;; Update status
  (ollama-buddy--update-status "Suffix reset")
  (message "Suffix has been reset"))

(defun ollama-buddy-reset-all-prompts ()
  "Reset both system prompt and suffix to default (none)."
  (interactive)
  (setq ollama-buddy--current-system-prompt nil
        ollama-buddy--current-suffix nil)
  
  ;; Update the UI to reflect the change
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (ollama-buddy--prepare-prompt-area t))
  
  ;; Update status
  (ollama-buddy--update-status "System prompt reset")
  (message "System prompt has been reset"))

(defun ollama-buddy-show-raw-model-info (&optional model)
  "Retrieve and display raw JSON information about the current default MODEL."
  (interactive)
  (let* ((model (or model
                    ollama-buddy--current-model
                    ollama-buddy-default-model
                    (error "No default model set")))
         (endpoint "/api/show")
         (payload (json-encode `((model . ,model)))))
    
    ;; Make API request to get model info
    (condition-case err
        (let* ((response (ollama-buddy--make-request endpoint "POST" payload)))
          
          ;; Open and prepare chat buffer
          (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
            (pop-to-buffer (current-buffer))
            (goto-char (point-max))
            
            ;; Insert model info header with color
            (insert (format "[MODEL INFO REQUEST]\n\n** [MODEL INFO: %s]\n\n" model))

            ;; Pretty print the JSON response
            (insert "#+begin_src json\n")
            (let ((json-start (point)))
              ;; Convert Elisp object to JSON string and insert
              (insert (json-encode response))
              ;; Pretty print the inserted JSON
              (json-pretty-print json-start (point)))
            (insert "\n#+end_src")
            
            ;; Add a prompt area after the information
            (ollama-buddy--prepare-prompt-area)
            (ollama-buddy--update-status "Model info displayed")))
      
      (error
       (message "Failed to retrieve model info: %s" (error-message-string err))
       (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
         (pop-to-buffer (current-buffer))
         (goto-char (point-max))
         (insert (format "\n\n** [ERROR] Failed to retrieve info for model: %s\n\n" model))
         (insert (format "Error: %s\n\n" (error-message-string err)))
         (ollama-buddy--prepare-prompt-area)
         (ollama-buddy--update-status "Error retrieving model info"))))))

(defun ollama-buddy-toggle-debug-mode ()
  "Toggle display of raw JSON messages in a debug buffer."
  (interactive)
  (setq ollama-buddy-debug-mode (not ollama-buddy-debug-mode))
  (if ollama-buddy-debug-mode
      (progn
        (with-current-buffer (get-buffer-create ollama-buddy--debug-buffer)
          (erase-buffer)
          (insert "=== Ollama Buddy Debug Mode ===\n")
          (insert "Raw JSON messages will appear here.\n\n")
          (special-mode))
        (display-buffer ollama-buddy--debug-buffer)
        (message "Debug mode enabled - raw JSON will be shown"))
    (when (get-buffer ollama-buddy--debug-buffer)
      (kill-buffer ollama-buddy--debug-buffer))
    (message "Debug mode disabled")))

(defun ollama-buddy--md-to-org-convert-region (start end)
  "Convert the region from START to END from Markdown to Org-mode format."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      
      ;; First, handle code blocks by temporarily protecting their content
      (goto-char (point-min))
      (let ((code-blocks nil)
            (counter 0)
            block-start block-end lang content placeholder)
        
        ;; IMPORTANT: Add save-match-data here
        (save-match-data
          ;; Find and replace code blocks with placeholders
          (while (re-search-forward "```\\(.*?\\)\\(?:\n\\|\\s-\\)\\(\\(?:.\\|\n\\)*?\\)```" nil t)
            (setq lang (match-string 1)
                  content (match-string 2)
                  block-start (match-beginning 0)
                  block-end (match-end 0)
                  placeholder (format "CODE_BLOCK_PLACEHOLDER_%d" counter))
            
            ;; Store the code block information for later restoration
            (push (list placeholder lang content) code-blocks)
            
            ;; Replace with placeholder
            (delete-region block-start block-end)
            (goto-char block-start)
            (insert placeholder)
            (setq counter (1+ counter))))
        
        ;; Apply regular Markdown to Org transformations - in individual save-match-data blocks
        ;; Lists: Translate `-`, `*`, or `+` lists to Org-mode lists
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "^\\([ \t]*\\)[*-+] \\(.*\\)$" nil t)
            (replace-match (concat (match-string 1) "- \\2"))))
        
        ;; Bold: `**bold**` -> `*bold*` only if directly adjacent
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "\\*\\*\\([^ ]\\(.*?\\)[^ ]\\)\\*\\*" nil t)
            (replace-match "*\\1*")))
        
        ;; Italics: `_italic_` -> `/italic/`
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "\\([ \n]\\)_\\([^ ].*?[^ ]\\)_\\([ \n]\\)" nil t)
            (replace-match "\\1/\\2/\\3")))
        
        ;; Links: `[text](url)` -> `[[url][text]]`
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
            (replace-match "[[\\2][\\1]]")))
        
        ;; Inline code: `code` -> =code=
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "`\\(.*?\\)`" nil t)
            (replace-match "=\\1=")))
        
        ;; Horizontal rules: `---` or `***` -> `-----`
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "^\\(-{3,}\\|\\*{3,}\\)$" nil t)
            (replace-match "-----")))
        
        ;; Images: `![alt text](url)` -> `[[url]]`
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "!\\[.*?\\](\\(.*?\\))" nil t)
            (replace-match "[[\\1]]")))
        
        ;; Headers: Adjust '#'
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "^\\(#+\\) " nil t)
            (replace-match (make-string (length (match-string 1)) ?*) nil nil nil 1)))
        
        ;; Any extra characters
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "—" nil t)
            (replace-match ", ")))
        
        ;; Restore code blocks with proper Org syntax
        (save-match-data
          (dolist (block (nreverse code-blocks))
            (let ((placeholder (nth 0 block))
                  (lang (nth 1 block))
                  (content (nth 2 block)))
              (goto-char (point-min))
              (when (search-forward placeholder nil t)
                (replace-match (format "#+begin_src %s\n%s#+end_src" lang content) t t)))))))))

(defun ollama-buddy-set-system-prompt ()
  "Set the current prompt as a system prompt."
  (interactive)
  (let* ((prompt-data (ollama-buddy--get-prompt-content))
         (prompt-text (car prompt-data)))
    
    ;; Add to history if non-empty
    (when (and prompt-text (not (string-empty-p prompt-text)))
      (put 'ollama-buddy--cycle-prompt-history 'history-position -1)
      (add-to-history 'ollama-buddy--prompt-history prompt-text))
    
    ;; Set as system prompt
    (setq ollama-buddy--current-system-prompt prompt-text)
    
    ;; Update the UI to reflect the change
    (ollama-buddy--prepare-prompt-area t t t)
    (ollama-buddy--prepare-prompt-area nil nil)
    
    ;; Update status to show system prompt is set
    (ollama-buddy--update-status "System prompt set")
    (message "System prompt set: %s"
             (if (> (length prompt-text) 50)
                 (concat (substring prompt-text 0 47) "...")
               prompt-text))))

(defun ollama-buddy-reset-system-prompt ()
  "Reset the system prompt to default (none)."
  (interactive)
  (setq ollama-buddy--current-system-prompt nil)
  
  ;; Update the UI to reflect the change
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (ollama-buddy--prepare-prompt-area t))
  
  ;; Update status
  (ollama-buddy--update-status "System prompt reset")
  (message "System prompt has been reset"))

(defun ollama-buddy--get-prompt-content ()
  "Extract the current prompt content from the buffer.
Returns a cons cell (TEXT . POINT) with the prompt text and point position."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward ">> \\(?:PROMPT\\|SYSTEM PROMPT\\):" nil t)
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
  "Toggle automatic conversion of markdown to `org-mode' format."
  (interactive)
  (setq ollama-buddy-convert-markdown-to-org
        (not ollama-buddy-convert-markdown-to-org))
  (ollama-buddy--update-status
   (if ollama-buddy-convert-markdown-to-org "Markdown conversion enabled" "Markdown conversion disabled"))
  (message "Markdown to Org conversion: %s"
           (if ollama-buddy-convert-markdown-to-org "enabled" "disabled")))

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
                         (setq all-messages (append all-messages (list msg-with-model))))))
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
                     (format "Start new session? Current session '%s' will be discarded unless saved.  ?"
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
        (ollama-buddy--apply-model-colors-to-buffer)
        (ollama-buddy--prepare-prompt-area)))
    
    ;; Update status
    (ollama-buddy--update-status "New session started")
    (message "Started new session")))

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

(defun ollama-buddy-display-history ()
  "Display the conversation history in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Ollama Conversation History*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (visual-line-mode 1)
        (erase-buffer)
        (cond
         ((= (prefix-numeric-value current-prefix-arg) 4)
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
                                        '(:inherit bold)
                                      '(:inherit bold))))
                    (insert (propertize (format "[%s]: " (upcase role)) 'face role-face))
                    (insert (format "%s\n\n" content))))))))
         (t
          (insert "Ollama Conversation History:\n\n")
          ;; Display history for all models
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
             ollama-buddy--conversation-history-by-model))))
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
       (format "Typing... [%d tokens, %.1f t/s]"
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
  (ollama-buddy--update-status (concat "Stats in chat " (if ollama-buddy-display-token-stats "enabled" "disabled")))
  (message "Ollama token statistics display: %s"
           (if ollama-buddy-display-token-stats "enabled" "disabled")))

(defun ollama-buddy-toggle-model-colors ()
  "Toggle the use of model-specific colors in ollama-buddy."
  (interactive)
  (setq ollama-buddy-enable-model-colors (not ollama-buddy-enable-model-colors))
  (message "Ollama Buddy Model Colors: %s"
           (if ollama-buddy-enable-model-colors "Enabled" "Disabled")))

(defun ollama-buddy--assign-model-letters ()
  "Assign letters to available models and update the intro message."
  (setq ollama-buddy--model-letters
        (cl-loop for model in (ollama-buddy--get-models)
                 for letter across "abcdefghijklmnopqrstuvwxyz"
                 collect (cons letter model))))

(defun ollama-buddy--get-model-letter (model-name)
  "Return the letter assigned to MODEL-NAME from `ollama-buddy--model-letters`."
  (car (rassoc model-name ollama-buddy--model-letters)))

(defun ollama-buddy--format-models-with-letters-plain ()
  "Format models with letter assignments for display without color properties."
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
                         (if (cdar row) (cdar row) "")
                         (if (cdr row)
                             (format "(%c) %s"
                                     (caadr row)
                                     (if (cdadr row) (cdadr row) ""))
                           "")))
               formatted-pairs
               "\n")
              "\n\n"))))

(defun ollama-buddy--apply-model-colors-to-buffer ()
  "Apply color overlays to models in the buffer after text insertion."
  (save-excursion
    ;; First, find the Available Models section
    (goto-char (point-max))
    (when (search-backward "** Local Available Models" nil t)
      (forward-line 2) ;; Skip the header and empty line
      
      ;; Now we're at the start of the model list
      (let ((models-end (save-excursion
                          (if (search-forward "** Quick Tips" nil t)
                              (progn (forward-line -1) (point))
                            (point-max)))))
        
        ;; Iterate through the model letters section
        (while (< (point) models-end)
          ;; Look for a pattern that captures both columns in one regex
          ;; Format: (a) model1  (b) model2
          (when (looking-at "\\s-*(\\([a-z]\\)) \\([^ \t\n]+\\)\\(.*?(\\([a-z]\\)) \\([^ \t\n]+\\)\\)?")
            ;; First column
            (let* ((model1 (match-string 2))
                   (model1-start (match-beginning 2))
                   (model1-end (match-end 2))
                   (color1 (ollama-buddy--get-model-color model1)))
              
              ;; Create overlay for first model
              (let ((overlay (make-overlay model1-start model1-end)))
                (overlay-put overlay 'face `(:foreground ,color1 :weight bold))))
            
            ;; Second column (if it exists)
            (when (match-string 3) ;; Check if we have a second column
              (let* ((model2 (match-string 5))
                     (model2-start (match-beginning 5))
                     (model2-end (match-end 5))
                     (color2 (ollama-buddy--get-model-color model2)))
                
                ;; Create overlay for second model
                (let ((overlay (make-overlay model2-start model2-end)))
                  (overlay-put overlay 'face `(:foreground ,color2 :weight bold))))))
          
          (forward-line 1))))))

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
         (use-prompt (y-or-n-p "Add a user prompt prefix? "))
         (prompt (if use-prompt
                     (read-string "User prompt prefix: ")
                   nil))
         (use-system (y-or-n-p "Add a system prompt/message? "))
         (system (if use-system
                     (read-string "System prompt/message: ")
                   nil))
         (symbol (intern command-name)))
    ;; Generate the command definition
    (list symbol
          :key key
          :description description
          :model model
          :prompt prompt
          :system system
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
           (format "Roles directory doesn't exist.  Create it at %s? "
                   ollama-buddy-roles-directory))
          (progn
            (make-directory ollama-buddy-roles-directory t)
            (dired ollama-buddy-roles-directory))
        (message "Directory not created."))
    (dired ollama-buddy-roles-directory)))

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

(defun ollama-buddy-show-system-prompt ()
  "Display the current system prompt in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Ollama System Prompt*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (visual-line-mode 1)
        (if ollama-buddy--current-system-prompt
            (progn
              (insert "Current System Prompt:\n\n")
              (insert ollama-buddy--current-system-prompt))
          (insert "No system prompt is currently set.")))
      (view-mode 1))
    (display-buffer buf)))

(defun ollama-buddy--initialize-chat-buffer ()
  "Initialize the chat buffer and check Ollama status."
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (when (= (buffer-size) 0)
      (org-mode)
      (visual-line-mode 1)
      (ollama-buddy-mode 1)
      (ollama-buddy--check-status)
      (insert (ollama-buddy--create-intro-message))
      ;; Apply color overlays to the inserted text
      (ollama-buddy--apply-model-colors-to-buffer)
      ;; now set up default model if none exist
      (when (not ollama-buddy-default-model)
        ;; just get the first model
        (let ((model (car (ollama-buddy--get-models))))
          (setq ollama-buddy--current-model model)
          (setq ollama-buddy-default-model model)
          (insert (format "\n\n* NO DEFAULT MODEL : Using best guess : %s" model))))
      (ollama-buddy--prepare-prompt-area)
      (put 'ollama-buddy--cycle-prompt-history 'history-position -1))
    (ollama-buddy--update-status "Idle")))

(defun ollama-buddy--stream-filter (_proc output)
  "Process stream OUTPUT while preserving cursor position."

  ;; Log raw output to debug buffer if enabled
  (when ollama-buddy-debug-mode
    (with-current-buffer (get-buffer-create ollama-buddy--debug-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t)
            (start-point (point)))
        (insert (format "\n=== MESSAGE %s ===\n"
                        (format-time-string "%H:%M:%S.%3N")))
        (insert output "\n")
        (save-excursion
          (goto-char start-point)
          (when (search-forward "{" nil t)
            (goto-char (match-beginning 0))
            (let ((json-start (point)))
              (when (ignore-errors (forward-sexp) t)
                (let ((json-end (point)))
                  (json-pretty-print json-start json-end)))))))))

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
      (progn
        (when ollama-buddy-convert-markdown-to-org
          (save-excursion
            (goto-char (point-max))
            (beginning-of-line)
            (let ((end (point)))
              (if (re-search-backward ": RESPONSE" nil t)
                  (progn
                    (search-forward "]")
                    (ollama-buddy--md-to-org-convert-region
                     (point) end))))))
        (ollama-buddy--update-status (concat "Stream " status))))))

(defun ollama-buddy--swap-model ()
  "Swap ollama model, including OpenAI models if available."
  (interactive)
  (if (and (not (ollama-buddy--ollama-running))
           (not (featurep 'ollama-buddy-openai)))
      (error "!!WARNING!! ollama server not running")
    (let* ((models (ollama-buddy--get-models-with-openai))
           (new-model (completing-read "Model: " models nil t)))
      
      ;; Check if it's an OpenAI model and set it accordingly
      (if (and (featurep 'ollama-buddy-openai)
               (ollama-buddy-openai--is-openai-model new-model))
          (progn
            (setq ollama-buddy-default-model new-model)
            (setq ollama-buddy--current-model new-model)
            (setq ollama-buddy-openai--current-model new-model)
            (message "Switched to OpenAI model: %s" new-model))
        (progn
          (setq ollama-buddy-default-model new-model)
          (setq ollama-buddy--current-model new-model)
          (message "Switched to Ollama model: %s" new-model)))
      
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
  ;; Apply color overlays to the inserted text
  (ollama-buddy--apply-model-colors-to-buffer)
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

(defun ollama-buddy--send-with-command (command-name)
  "Send request using configuration from COMMAND-NAME."
  (let* ((prompt-text (ollama-buddy--get-command-prop command-name :prompt))
         (system-text (ollama-buddy--get-command-prop command-name :system))
         (selected-text (when (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning) (region-end))))
         (model (ollama-buddy--get-command-prop command-name :model))
         (params-alist (ollama-buddy--get-command-prop command-name :parameters)))
    
    ;; Verify requirements
    (when (and prompt-text (not selected-text))
      (user-error "This command requires selected text"))
    
    ;; Apply command-specific parameters if provided
    (when params-alist
      (ollama-buddy--apply-command-parameters params-alist))
    
    ;; Display which system prompt will be used
    (when system-text
      (ollama-buddy--display-system-prompt system-text 3))
    
    ;; Prepare and send the prompt
    (let ((full-prompt (ollama-buddy--prepare-command-prompt command-name selected-text)))
      ;; Temporarily set system prompt if specified for this command
      (let ((old-system-prompt ollama-buddy--current-system-prompt))
        (when system-text
          (setq ollama-buddy--current-system-prompt system-text))
        
        ;; Send the request
        (ollama-buddy--send (string-trim full-prompt) model)
        
        ;; Restore the original system prompt if we changed it
        (when system-text
          (setq ollama-buddy--current-system-prompt old-system-prompt))
        
        ;; Restore default parameters if we changed them
        (when params-alist
          (ollama-buddy--restore-default-parameters))))))

(defun ollama-buddy--send (&optional prompt specified-model)
  "Send PROMPT with optional SYSTEM-PROMPT, SUFFIX and SPECIFIED-MODEL."
  ;; Check status and update UI if offline
  (unless (or (ollama-buddy--check-status)
              (and (featurep 'ollama-buddy-openai)
                   (ollama-buddy-openai--is-openai-model 
                    (or specified-model ollama-buddy--current-model))))
    (ollama-buddy--update-status "OFFLINE")
    (user-error "Ensure Ollama is running"))

  (unless (> (length prompt) 0)
    (user-error "Ensure prompt is defined"))

  ;; For OpenAI models, use the OpenAI send function
  (if (and (featurep 'ollama-buddy-openai)
           (ollama-buddy-openai--is-openai-model 
            (or specified-model ollama-buddy--current-model)))
      (ollama-buddy-openai--send prompt specified-model)
    
    ;; Original Ollama send code
    (let* ((model-info (ollama-buddy--get-valid-model specified-model))
           (model (car model-info))
           (original-model (cdr model-info))
           (messages (ollama-buddy--get-history-for-request))
           ;; If we have a system prompt, add it to the request
           (messages-with-system
            (if ollama-buddy--current-system-prompt
                (append `(((role . "system")
                           (content . ,ollama-buddy--current-system-prompt)))
                        messages)
              messages))
           ;; Add the current prompt to the messages
           (messages-all (append messages-with-system
                                 `(((role . "user")
                                    (content . ,prompt)))))
           ;; Get only the modified parameters
           (modified-options (ollama-buddy-params-get-for-request))
           ;; Build the base payload
           (base-payload `((model . ,model)
                           (messages . ,(vconcat [] messages-all))
                           (stream . t)))
           ;; Add system prompt if present
           (with-system (if ollama-buddy--current-system-prompt
                            (append base-payload `((system . ,ollama-buddy--current-system-prompt)))
                          base-payload))
           ;; Add suffix if present
           (with-suffix (if ollama-buddy--current-suffix
                            (append with-system `((suffix . ,ollama-buddy--current-suffix)))
                          with-system))
           ;; Add modified parameters if present
           (final-payload (if modified-options
                              (append with-suffix `((options . ,modified-options)))
                            with-suffix))
           (payload (json-encode final-payload)))
      
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
         (error "Failed to send request to Ollama: %s" (error-message-string err)))))))

(defun ollama-buddy--create-intro-message ()
  "Create welcome message with model management capabilities in org format."
  (ollama-buddy--assign-model-letters)
  (let* ((available-models (when (ollama-buddy--ollama-running)
                             (ollama-buddy--get-models)))
         ;; Get OpenAI models if available
         (openai-models (when (featurep 'ollama-buddy-openai)
                          (mapcar #'ollama-buddy-openai--get-full-model-name
                                 ollama-buddy-openai-models)))
         ;; Get models available for pull but not yet downloaded
         (models-to-pull (when (ollama-buddy--ollama-running)
                           (cl-set-difference
                            ollama-buddy-available-models
                            available-models
                            :test #'string=)))
         ;; Basic tips for beginners
         (basic-tips
          "- Ask me anything!                    C-c C-c
- Main transient menu                 C-c O
- Change model                        C-c m
- Cancel request                      C-c k
- Browse prompt history               M-p/M-n
- Advanced interface (show all tips)  C-c A")
         ;; Advanced tips for experienced users
         (advanced-tips
          "- Ask me anything!                    C-c C-c
- Main transient menu                 C-c O
- Manage models                       C-c W
- Show Help/Status/Token usage        C-c h/v/U
- Model Change/Info/Multishot         C-c m/i/M
- System Prompt Set/Show/Reset        C-c s/C-s/r
- Parameter Menu/Profiles             C-c P/p
- History Toggle/Clear/Show/Edit      C-c H/X/V/J
- Session New/Load/Save/List          C-c N/L/S/Q
- Role Switch/Create/Directory        C-c R/E/D
- Fabric Patterns Menu                C-c f
- Display Options (Colors/Markdown)   C-c c/C-o
- Browse prompt history               M-p/M-n
- Basic interface (simpler display)   C-c A")
         ;; Choose tips based on interface level
         (tips-section (if (eq ollama-buddy-interface-level 'basic)
                          basic-tips
                          advanced-tips))
         ;; Create model management section
         (models-management-section
          (when available-models
            (concat
             "** Local Available Models\n\n"
             (mapconcat
              (lambda (model)
                (let ((model-letter (ollama-buddy--get-model-letter model)))
                  (concat
                   (format
                    "(%c) %s [[elisp:(ollama-buddy-select-model \"%s\")][[Select]]] "
                    model-letter model model)
                   (format
                    "[[elisp:(ollama-buddy-show-raw-model-info \"%s\")][[Info]]] " model)
                   (format "[[elisp:(ollama-buddy-pull-model \"%s\")][[Pull]]] " model)
                   (format
                    "[[elisp:(ollama-buddy-delete-model-in-chat \"%s\"))][[Delete]]]"
                    model))))
              available-models
              "\n")
             "\n\n")))
         
         ;; Create section for models available to pull but not yet downloaded
         (models-to-pull-section
          (when (and models-to-pull (not (null models-to-pull)))
            (concat
             "** Recommended Models to Download\n\n"
             (mapconcat
              (lambda (model)
                (format "[[elisp:(ollama-buddy-pull-model \"%s\")][[%s]]]"
                        model model))
              models-to-pull
              " ")
             "\n\n")))

         ;; Create section for OpenAI models if available
         (openai-models-section
          (when (and openai-models (not (null openai-models)))
            (concat
             "** OpenAI Models\n\n"
             (mapconcat
              (lambda (model)
                (format "[[elisp:(ollama-buddy-select-model \"%s\")][[%s]]]"
                        model model))
              openai-models
              " ")
             "\n\n")))
         
         ;; Models with letters (the original section)
         (message-text
          (concat
           (when (= (buffer-size) 0)
             (concat "#+TITLE: Ollama Buddy Chat\n"
                     "#+STARTUP: content"))
           "\n\n* Welcome to OLLAMA BUDDY\n\n"
           "#+begin_example\n"
           " ___ _ _      n _ n      ___       _   _ _ _\n"
           "|   | | |__._|o(Y)o|__._| . |_ _ _| |_| | | |\n"
           "| | | | | .  |     | .  | . | | | . | . |__ |\n"
           "|___|_|_|__/_|_|_|_|__/_|___|___|___|___|___|\n"
           "#+end_example\n\n"
           models-management-section
           openai-models-section
           models-to-pull-section
           "** Actions\n\n"
           "[[elisp:(call-interactively #'ollama-buddy-import-gguf-file)][[Import GGUF File]]] "
           "[[elisp:(call-interactively #'ollama-buddy-pull-model)][[Pull Any Model]]] "
           (if (featurep 'ollama-buddy-openai)
               "[[elisp:(call-interactively #'ollama-buddy-openai-setup)][[Setup OpenAI]]]\n\n"
             "\n\n")
           "** Quick Tips\n\n"
           tips-section)))

    ;; Apply overlay colors to model names
    (with-temp-buffer
      (insert message-text)
      (goto-char (point-min))
      ;; Find model names in the format "=model-name=" and apply face
      (while (re-search-forward "=\\([^=]+\\)=" nil t)
        (let* ((model-name (match-string 1))
               (color (ollama-buddy--get-model-color model-name))
               (start (match-beginning 0))
               (end (match-end 0)))
          (add-text-properties start end `(face (:foreground ,color :weight bold)) (current-buffer))))
      (setq message-text (buffer-string)))
    
    ;; Add bold face to the entire message
    (add-face-text-property 0 (length message-text) '(:inherit bold) nil message-text)
    message-text))

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

;;;###autoload
(defun ollama-buddy-add-model-to-menu-entry (entry-name model-name)
  "Add :model property with MODEL-NAME to ENTRY-NAME in the menu variable.
Modifies the variable in place."
  (when-let ((entry (assq entry-name ollama-buddy-command-definitions)))
    (setf (cdr entry)
          (append (cdr entry) (list :model model-name))))
  ollama-buddy-command-definitions)

(defun ollama-buddy-show-model-status ()
  "Display status of models referenced in command definitions with color coding."
  (interactive)
  (let* ((used-models (delete-dups
                       (delq nil
                             (mapcar (lambda (cmd)
                                       (plist-get (cdr cmd) :model))
                                     ollama-buddy-command-definitions))))
         (available-models (ollama-buddy--get-models))
         (running-models (ollama-buddy--get-running-models))
         (ollama-version (ollama-buddy--make-request "/api/version" "GET"))
         (buf (get-buffer-create "*Ollama Model Status*")))
    ;; Update model colors
    (when (ollama-buddy--ollama-running)
      (ollama-buddy--update-model-colors))
    
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; ollama version
        (insert "ollama version : " (cdar ollama-version) "\n\n")
        
        ;; List running models with colors
        (if running-models
            (progn
              (insert "Currently Running Models:\n")
              (dolist (model running-models)
                (let ((color (ollama-buddy--get-model-color model)))
                  (insert "  ")
                  (insert (propertize model 'face `(:foreground ,color)))
                  (insert "\n"))))
          (insert "No models currently running.\n"))

        (insert "\n")
        
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
            (insert "\n")))
        
        ;; List available models with colors
        (insert "\nAvailable Models:\n")
        (dolist (model available-models)
          (let ((color (ollama-buddy--get-model-color model)))
            (insert "  ")
            (insert (propertize model 'face `(:foreground ,color)))
            (insert "\n")))

        (insert "\n")
        
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
                                "Not Available ✗"))))))))
    (display-buffer buf)))

(defun ollama-buddy--send-prompt ()
  "Send the current prompt to a LLM with support for system prompt and suffixes."
  (interactive)
  (let* ((current-prefix-arg-val (prefix-numeric-value current-prefix-arg))
         (prompt-data (ollama-buddy--get-prompt-content))
         (prompt-text (car prompt-data))
         (model (or ollama-buddy--current-model
                    ollama-buddy-default-model
                    "Default:latest")))
    
    ;; Handle prefix arguments
    (cond
     ;; C-u (4) - Set system prompt
     ((= current-prefix-arg-val 4)
      (ollama-buddy-set-system-prompt))

     ;; C-u C-u (16) - Reset both system prompt
     ((= current-prefix-arg-val 16)
      (ollama-buddy-reset-all-prompts))
     
     ;; No prefix - Regular prompt
     (t
      ;; Add to history if non-empty
      (when (and prompt-text (not (string-empty-p prompt-text)))
        (put 'ollama-buddy--cycle-prompt-history 'history-position -1)
        (add-to-history 'ollama-buddy--prompt-history prompt-text))
      
      ;; Reset multishot variables
      (setq ollama-buddy--multishot-sequence nil
            ollama-buddy--multishot-prompt nil)
      
      ;; Send with system prompt and suffix support
      (ollama-buddy--send prompt-text model)))))

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

(defun ollama-buddy-ensure-modelfile-directory ()
  "Create the ollama-buddy modelfile directory if it doesn't exist."
  (unless (file-directory-p ollama-buddy-modelfile-directory)
    (make-directory ollama-buddy-modelfile-directory t)))

(defun ollama-buddy-import-gguf-file (file-path)
  "Import a GGUF file at FILE-PATH into Ollama."
  (interactive "fSelect GGUF file: ")
  (unless (file-exists-p file-path)
    (user-error "File does not exist: %s" file-path))
  
  (unless (string-match-p "\\.gguf$" file-path)
    (user-error "File does not appear to be a GGUF file (missing .gguf extension): %s" file-path))
  
  ;; Ensure the modelfile directory exists
  (ollama-buddy-ensure-modelfile-directory)
  
  ;; Get the base name without extension for default model name
  (let* ((file-name (file-name-nondirectory file-path))
         (base-name (replace-regexp-in-string "\\.gguf$" "" file-name))
         ;; Prompt for model name, suggesting a sanitized version of the filename
         (model-name (read-string "Model name to create: "
                                 (replace-regexp-in-string "[^a-zA-Z0-9_-]" "-" base-name)))
         ;; Prompt for model parameters
         (parameters (read-string "Model parameters (optional): " ""))
         ;; Create a temporary Modelfile
         (modelfile-path (expand-file-name (format "Modelfile-%s" model-name)
                                          ollama-buddy-modelfile-directory))
         ;; Buffer for output
         (output-buffer (get-buffer-create "*Ollama Import*"))
         (default-directory (file-name-directory file-path)))
    
    ;; Generate Modelfile content
    (with-temp-file modelfile-path
      (insert (format "FROM %s\n" file-path))
      (when (not (string-empty-p parameters))
        (insert (format "PARAMETER %s\n" parameters))))
    
    ;; Show the buffer
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Creating Ollama model '%s' from file: %s\n\n" model-name file-path))
        (insert "Modelfile content:\n")
        (insert-file-contents modelfile-path)
        (insert "\n\nRunning ollama create command...\n\n")
        (display-buffer (current-buffer))))
    
    ;; Run the ollama create command
    (let ((process (start-process "ollama-create" output-buffer
                                 "ollama" "create" model-name "-f" modelfile-path)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (let ((status (string-trim event)))
           (with-current-buffer (process-buffer proc)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (cond
                ((string-prefix-p "finished" status)
                 (insert "\nSuccessfully created model: " model-name)
                 (ollama-buddy--assign-model-letters) ;; Update model list
                 ;; Ask if user wants to use this model now
                 (when (y-or-n-p (format "Model '%s' created.  Use it now? " model-name))
                   (setq ollama-buddy--current-model model-name)
                   (message "Switched to model: %s" model-name)))
                (t
                 (insert "\nError creating model: " status)))
               (insert "\n")))))))))

(defun ollama-buddy-dired-import-gguf ()
  "Import the GGUF file at point in Dired into Ollama."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (user-error "This command only works in Dired mode"))
  
  (let ((file (dired-get-filename nil t)))
    (if file
        (if (string-match-p "\\.gguf$" file)
            (ollama-buddy-import-gguf-file file)
          (user-error "File is not a GGUF file: %s" file))
      (user-error "No file at point"))))

(defun ollama-buddy-add-dired-integration ()
  "Add Ollama integration to Dired mode."
  (when (eq major-mode 'dired-mode)
    (local-set-key (kbd "C-c i") #'ollama-buddy-dired-import-gguf)))

;; Add hook to enable integration when entering dired mode
(add-hook 'dired-mode-hook #'ollama-buddy-add-dired-integration)

(defun ollama-buddy-manage-models ()
  "Display and manage Ollama models."
  (interactive)
  (let* ((available-models (ollama-buddy--get-models))
         (running-models (ollama-buddy--get-running-models))
         (buf (get-buffer-create "*Ollama Models Management*")))
    
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Ollama Models Management\n")
        (insert "=======================\n\n")
        
        ;; Display current and default models
        (when ollama-buddy--current-model
          (let ((color (ollama-buddy--get-model-color ollama-buddy--current-model)))
            (insert "Current Model: ")
            (insert (propertize ollama-buddy--current-model 'face `(:foreground ,color :weight bold)))
            (insert "\n")))
        
        (when ollama-buddy-default-model
          (let ((color (ollama-buddy--get-model-color ollama-buddy-default-model)))
            (insert "Default Model: ")
            (insert (propertize ollama-buddy-default-model 'face `(:foreground ,color :weight bold)))
            (insert "\n\n")))
        
        ;; List of models with status and actions
        (insert "Available Models:\n")
        
        (dolist (model available-models)
          (let* ((color (ollama-buddy--get-model-color model))
                (is-running (member model running-models)))
            
            (insert (format "  [%s] " (if is-running "✓" " ")))
            
            ;; Select button
            (insert-text-button
             (propertize model 'face `(:foreground ,color))
             'action `(lambda (_)
                        (ollama-buddy-select-model ,model))
             'help-echo "Select this model")
            
            (insert "  ")
            
            ;; Info button
            (insert-text-button
             "Info"
             'action `(lambda (_)
                        (ollama-buddy-show-raw-model-info ,model))
             'help-echo "Show model information")
            
            (insert "  ")
            
            ;; Pull/Stop button
            (if is-running
                (insert-text-button
                 "Stop"
                 'action `(lambda (_)
                            (ollama-buddy-stop-model ,model))
                 'help-echo "Stop this model")
              (insert-text-button
               "Pull"
               'action `(lambda (_)
                          (ollama-buddy-pull-model ,model))
               'help-echo "Pull/update this model"))
            
            (insert "  ")
            
            ;; Delete button with proper capture
            (insert-text-button
             "Delete"
             'action `(lambda (_)
                        (when (yes-or-no-p (format "Really delete model '%s'? " ,model))
                          (ollama-buddy-delete-model ,model)))
             'help-echo "Delete this model")
            
            (insert "\n")))
        
        ;; Actions at bottom
        (insert "\nActions:\n")
        (insert-text-button
         "[Import GGUF File]"
         'action (lambda (_) (call-interactively #'ollama-buddy-import-gguf-file))
         'help-echo "Import a GGUF file to create a new model")
        (insert "  ")
        (insert-text-button
         "[Refresh List]"
         'action (lambda (_) (ollama-buddy-manage-models))
         'help-echo "Refresh model list")
        (insert "  ")
        (insert-text-button
         "[Pull Model from Hub]"
         'action (lambda (_) (call-interactively #'ollama-buddy-pull-model))
         'help-echo "Pull a model from Ollama Hub")
        
        ;; Set up the mode
        (special-mode)
        (use-local-map (copy-keymap special-mode-map))
        (local-set-key (kbd "g") #'ollama-buddy-manage-models) ;; g to refresh
        (local-set-key (kbd "i") (lambda () (interactive)
                                   (call-interactively #'ollama-buddy-import-gguf-file)))
        (local-set-key (kbd "p") (lambda () (interactive)
                                   (call-interactively #'ollama-buddy-pull-model)))))
    
    (display-buffer buf)))

; Helper functions for actions
(defun ollama-buddy-select-model (model)
  "Set MODEL as the current model, handling both Ollama and OpenAI models."
  (setq ollama-buddy-default-model model)
  (setq ollama-buddy--current-model model)
  
  ;; If it's an OpenAI model, update the openai current model as well
  (when (and (featurep 'ollama-buddy-openai)
             (ollama-buddy-openai--is-openai-model model))
    (setq ollama-buddy-openai--current-model model))
  
  (message "Selected model: %s" model)
  (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
  (ollama-buddy--prepare-prompt-area)
  (goto-char (point-max))
  (ollama-buddy--update-status "Idle"))

(defun ollama-buddy-show-model-info (model)
  "Display detailed information about MODEL."
  (let ((endpoint "/api/show")
        (payload (json-encode `((name . ,model)))))
    (condition-case err
        (let* ((response (ollama-buddy--make-request endpoint "POST" payload))
               (buf (get-buffer-create "*Ollama Model Info*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "Ollama Model Information: %s\n\n" model))
              (insert "#+begin_src json\n")
              (let ((json-start (point)))
                (insert (json-encode response))
                (json-pretty-print json-start (point)))
              (insert "\n#+end_src")
              (view-mode 1)))
          (display-buffer buf))
      (error (message "Error fetching model info: %s" (error-message-string err))))))

(defun ollama-buddy-pull-model (model)
  "Pull or update MODEL from Ollama Hub."
  (interactive
   (list (completing-read "Pull model: " ollama-buddy-available-models)))
  (let ((buf (get-buffer-create "*Ollama Pull*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Pulling model: %s\n\n" model))
        (display-buffer (current-buffer))))
    
    (let ((process (start-process "ollama-pull" buf "ollama" "pull" model)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (let ((status (string-trim event)))
           (with-current-buffer (process-buffer proc)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert (format "\nPull %s: %s\n" model status))
               (when (string-prefix-p "finished" status)
                 (ollama-buddy--assign-model-letters) ;; Update model list
                 (insert "\nModel ready to use.")))))))
      (message "Pulling model %s in background..." model))))

(defun ollama-buddy-stop-model (model)
  "Stop a running MODEL."
  (let ((buf (get-buffer-create "*Ollama Command*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Stopping model: %s\n\n" model))
        (display-buffer (current-buffer))))
    
    (let ((process (start-process "ollama-rm-running" buf "ollama" "rm" "-r" model)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (let ((status (string-trim event)))
           (with-current-buffer (process-buffer proc)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert (format "\nStop %s: %s\n" model status))
               (when (string-prefix-p "finished" status)
                 (ollama-buddy-manage-models)))))))
      (message "Stopping model %s..." model))))

(defun ollama-buddy-delete-model-in-chat (model)
  "Delete MODEL from Ollama."
  (let ((buf (get-buffer-create ollama-buddy--chat-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (insert (format "Deleting model: %s\n\n" model))
        (display-buffer (current-buffer))))
    
    (let ((process (start-process "ollama-delete" buf "ollama" "rm" model)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (let ((status (string-trim event)))
           (with-current-buffer (process-buffer proc)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert (format "\nDelete %s: %s\n" model status))
               (when (string-prefix-p "finished" status)
                 (ollama-buddy--assign-model-letters) ;; Update model list
                 (insert (ollama-buddy--create-intro-message))
                 (ollama-buddy--apply-model-colors-to-buffer)
                 (ollama-buddy--prepare-prompt-area)))))))
      (message "Deleting model %s..." model))))

(defun ollama-buddy-delete-model (model)
  "Delete MODEL from Ollama."
  (let ((buf (get-buffer-create "*Ollama Command*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Deleting model: %s\n\n" model))
        (display-buffer (current-buffer))))
    
    (let ((process (start-process "ollama-delete" buf "ollama" "rm" model)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (let ((status (string-trim event)))
           (with-current-buffer (process-buffer proc)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert (format "\nDelete %s: %s\n" model status))
               (when (string-prefix-p "finished" status)
                 (ollama-buddy--assign-model-letters) ;; Update model list
                 (ollama-buddy-manage-models)))))))
      (message "Deleting model %s..." model))))

(defun ollama-buddy-params-help ()
  "Display help for Ollama parameters."
  (interactive)
  (let ((buf (get-buffer-create "*Ollama Parameters Help*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Ollama Parameters Help\n")
        (insert "====================\n\n")
        
        (insert "Generation Parameters:\n")
        (insert "---------------------\n")
        (insert "temperature: Controls randomness (0.0-1.0+), higher = more creative\n")
        (insert "top_k: Limits token selection to top K most probable tokens\n")
        (insert "top_p: Nucleus sampling threshold (0.0-1.0)\n")
        (insert "min_p: Minimum probability threshold for token selection\n")
        (insert "typical_p: Controls how 'typical' responses are\n")
        (insert "repeat_last_n: Number of tokens to consider for repetition penalties\n")
        (insert "repeat_penalty: Penalty for repeating tokens (higher = less repetition)\n")
        (insert "presence_penalty: Penalizes tokens already in the text\n")
        (insert "frequency_penalty: Penalizes frequent tokens\n")
        (insert "mirostat: Enable adaptive sampling (0=off, 1=Mirostat, 2=Mirostat 2.0)\n")
        (insert "mirostat_tau: Target entropy for Mirostat\n")
        (insert "mirostat_eta: Learning rate for Mirostat\n")
        (insert "penalize_newline: Whether to penalize newline tokens\n")
        (insert "stop: Sequences that will stop generation when produced\n\n")
        
        (insert "Resource Parameters:\n")
        (insert "------------------\n")
        (insert "num_keep: Number of tokens to keep from prompt\n")
        (insert "seed: Random seed for deterministic generation\n")
        (insert "num_predict: Maximum tokens to generate\n")
        (insert "numa: Enable Non-Uniform Memory Access optimization\n")
        (insert "num_ctx: Context window size in tokens\n")
        (insert "num_batch: Batch size for processing\n")
        (insert "num_gpu: Number of GPUs to use\n")
        (insert "main_gpu: Primary GPU for computation\n")
        (insert "low_vram: Optimize for systems with limited VRAM\n")
        (insert "vocab_only: Load only vocabulary, not weights\n")
        (insert "use_mmap: Use memory-mapped files\n")
        (insert "use_mlock: Lock model weights in memory\n")
        (insert "num_thread: Number of CPU threads to use\n")
        
        (view-mode 1)))
    (display-buffer buf)))

(defvar ollama-buddy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Primary Transient Menu access
    (define-key map (kbd "C-c O") #'ollama-buddy-transient-menu)
    (define-key map (kbd "C-c W") #'ollama-buddy-manage-models)
    
    ;; Chat section keybindings from transient
    (define-key map (kbd "C-c C-c") #'ollama-buddy--send-prompt)  ;; Keep existing binding for RET
    (define-key map (kbd "C-c h") #'ollama-buddy--menu-help-assistant)
    (define-key map (kbd "C-c k") #'ollama-buddy--cancel-request)
    
    ;; Prompts section keybindings
    (define-key map (kbd "C-c l") (lambda () (interactive) (ollama-buddy--send-with-command 'send-region)))
    (define-key map (kbd "C-c s") #'ollama-buddy-set-system-prompt)
    (define-key map (kbd "C-c C-s") #'ollama-buddy-show-system-prompt)
    (define-key map (kbd "C-c r") #'ollama-buddy-reset-system-prompt)
    (define-key map (kbd "C-c b") #'ollama-buddy-menu)
    
    ;; Model section keybindings
    (define-key map (kbd "C-c m") #'ollama-buddy--swap-model)
    (define-key map (kbd "C-c v") #'ollama-buddy-show-model-status)
    (define-key map (kbd "C-c i") #'ollama-buddy-show-raw-model-info)
    (define-key map (kbd "C-c M") #'ollama-buddy--multishot-prompt)
    
    ;; Roles & Patterns keybindings
    (define-key map (kbd "C-c R") #'ollama-buddy-roles-switch-role)
    (define-key map (kbd "C-c E") #'ollama-buddy-role-creator-create-new-role)
    (define-key map (kbd "C-c D") #'ollama-buddy-roles-open-directory)
    (define-key map (kbd "C-c f") #'ollama-buddy-transient-fabric-menu)
    
    ;; Display Options keybindings
    (define-key map (kbd "C-c A") #'ollama-buddy-toggle-interface-level)
    (define-key map (kbd "C-c B") #'ollama-buddy-toggle-debug-mode)
    (define-key map (kbd "C-c T") #'ollama-buddy-toggle-token-display)
    (define-key map (kbd "C-c U") #'ollama-buddy-display-token-stats)
    (define-key map (kbd "C-c C-o") #'ollama-buddy-toggle-markdown-conversion)
    (define-key map (kbd "C-c c") #'ollama-buddy-toggle-model-colors)
    (define-key map (kbd "C-c g") #'ollama-buddy-display-token-graph)
    
    ;; History keybindings
    (define-key map (kbd "C-c H") #'ollama-buddy-toggle-history)
    (define-key map (kbd "C-c X") #'ollama-buddy-clear-history)
    (define-key map (kbd "C-c V") #'ollama-buddy-display-history)
    (define-key map (kbd "C-c J") #'ollama-buddy-history-edit)
    (define-key map (kbd "M-p") #'ollama-buddy-previous-history)  ;; Keep these existing bindings
    (define-key map (kbd "M-n") #'ollama-buddy-next-history)
    
    ;; Session keybindings
    (define-key map (kbd "C-c N") #'ollama-buddy-sessions-new)
    (define-key map (kbd "C-c L") #'ollama-buddy-sessions-load)
    (define-key map (kbd "C-c S") #'ollama-buddy-sessions-save)
    (define-key map (kbd "C-c Q") #'ollama-buddy-sessions-list)
    (define-key map (kbd "C-c Z") #'ollama-buddy-sessions-delete)
    
    ;; Parameter keybindings
    (define-key map (kbd "C-c P") #'ollama-buddy-transient-parameter-menu)
    (define-key map (kbd "C-c G") #'ollama-buddy-params-display)
    (define-key map (kbd "C-c I") #'ollama-buddy-params-help)
    (define-key map (kbd "C-c K") #'ollama-buddy-params-reset)
    (define-key map (kbd "C-c F") #'ollama-buddy-toggle-params-in-header)
    (define-key map (kbd "C-c p") #'ollama-buddy-transient-profile-menu)
    map)
  "Keymap for ollama-buddy mode.")

(define-minor-mode ollama-buddy-mode
  "Minor mode for ollama-buddy keybindings."
  :lighter " OB"
  :keymap ollama-buddy-mode-map)

(push 'ollama-buddy--prompt-history savehist-additional-variables)

(setq org-return-follows-link t)

(provide 'ollama-buddy)
;;; ollama-buddy.el ends here
