;;; ollama-buddy.el --- Ollama Buddy: Your Friendly AI Assistant -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.4.0
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

(defcustom ollama-buddy-enable-model-colors t
  "Whether to show model colors. EXPERIMENTAL."
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

(defvar ollama-buddy--conversation-history nil
  "History of messages for the current conversation.")

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

(defun ollama-buddy--find-prompt-positions ()
  "Find all prompt positions in the current buffer.
Returns a list of positions where prompts start."
  (let ((positions '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[User: PROMPT\\]" nil t)
        (push (match-beginning 0) positions)))
    (nreverse positions)))

(defun ollama-buddy--find-response-positions ()
  "Find all response positions in the current buffer.
Returns a list of positions where responses start."
  (let ((positions '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[.*: RESPONSE\\]" nil t)
        (push (match-beginning 0) positions)))
    (nreverse positions)))

(defun ollama-buddy--find-all-conversation-markers ()
  "Find all prompt and response markers in chronological order.
Returns a list of cons cells with position and type ('prompt or 'response)."
  (let ((markers '()))
    (save-excursion
      ;; Find all prompts
      (goto-char (point-min))
      (while (re-search-forward "\\[User: PROMPT\\]" nil t)
        (push (cons (match-beginning 0) 'prompt) markers))
      
      ;; Find all responses
      (goto-char (point-min))
      (while (re-search-forward "\\[.*: RESPONSE\\]" nil t)
        (push (cons (match-beginning 0) 'response) markers)))
    
    ;; Sort by position and return
    (sort markers (lambda (a b) (< (car a) (car b))))))

(defun ollama-buddy-previous-prompt ()
  "Navigate to the previous prompt."
  (interactive)
  (let ((positions (ollama-buddy--find-prompt-positions)))
    (if (null positions)
        (message "No prompts found")
      (let ((prev-pos (car (last (seq-filter (lambda (pos) (< pos (point))) positions)))))
        (if prev-pos
            (progn
              (goto-char prev-pos)
              (recenter))
          (message "No previous prompt"))))))

(defun ollama-buddy-next-prompt ()
  "Navigate to the next prompt."
  (interactive)
  (let ((positions (ollama-buddy--find-prompt-positions)))
    (if (null positions)
        (message "No prompts found")
      (let ((next-pos (car (seq-filter (lambda (pos) (> pos (point))) positions))))
        (if next-pos
            (progn
              (goto-char next-pos)
              (recenter))
          (message "No next prompt"))))))

(defun ollama-buddy-previous-response ()
  "Navigate to the previous response."
  (interactive)
  (let ((positions (ollama-buddy--find-response-positions)))
    (if (null positions)
        (message "No responses found")
      (let ((prev-pos (car (last (seq-filter (lambda (pos) (< pos (point))) positions)))))
        (if prev-pos
            (progn
              (goto-char prev-pos)
              (recenter))
          (message "No previous response"))))))

(defun ollama-buddy-next-response ()
  "Navigate to the next response."
  (interactive)
  (let ((positions (ollama-buddy--find-response-positions)))
    (if (null positions)
        (message "No responses found")
      (let ((next-pos (car (seq-filter (lambda (pos) (> pos (point))) positions))))
        (if next-pos
            (progn
              (goto-char next-pos)
              (recenter))
          (message "No next response"))))))

(defun ollama-buddy-previous-conversation-item ()
  "Navigate to the previous item (prompt or response) in the conversation."
  (interactive)
  (let ((markers (ollama-buddy--find-all-conversation-markers)))
    (if (null markers)
        (message "No conversation items found")
      (let ((prev-marker (car (last (seq-filter (lambda (marker) 
                                                  (< (car marker) (point))) 
                                                markers)))))
        (if prev-marker
            (progn
              (goto-char (car prev-marker))
              (recenter)
              (message "Previous %s" (if (eq (cdr prev-marker) 'prompt) 
                                         "prompt" 
                                       "response")))
          (message "No previous conversation item"))))))

(defun ollama-buddy-next-conversation-item ()
  "Navigate to the next item (prompt or response) in the conversation."
  (interactive)
  (let ((markers (ollama-buddy--find-all-conversation-markers)))
    (if (null markers)
        (message "No conversation items found")
      (let ((next-marker (car (seq-filter (lambda (marker) 
                                            (> (car marker) (point))) 
                                          markers))))
        (if next-marker
            (progn
              (goto-char (car next-marker))
              (recenter)
              (message "Next %s" (if (eq (cdr next-marker) 'prompt) 
                                     "prompt" 
                                   "response")))
          (message "No next conversation item"))))))

(defun ollama-buddy--add-to-history (role content)
  "Add message with ROLE and CONTENT to conversation history."
  (when ollama-buddy-history-enabled
    (push `((role . ,role)
            (content . ,content))
          ollama-buddy--conversation-history)
    ;; Truncate history if needed
    (when (> (length ollama-buddy--conversation-history) 
             (* 2 ollama-buddy-max-history-length))
      (setq ollama-buddy--conversation-history 
            (seq-take ollama-buddy--conversation-history 
                      (* 2 ollama-buddy-max-history-length))))))

(defun ollama-buddy--get-history-for-request ()
  "Format conversation history for inclusion in an Ollama request."
  (if ollama-buddy-history-enabled
      (reverse ollama-buddy--conversation-history)
    nil))

(defun ollama-buddy-clear-history ()
  "Clear the current conversation history."
  (interactive)
  (setq ollama-buddy--conversation-history nil)
  (ollama-buddy--update-status "History cleared")
  (message "Ollama conversation history cleared"))

(defun ollama-buddy-toggle-history ()
  "Toggle conversation history on/off."
  (interactive)
  (setq ollama-buddy-history-enabled (not ollama-buddy-history-enabled))
  (ollama-buddy--update-status 
   (if ollama-buddy-history-enabled "History enabled" "History disabled"))
  (message "Ollama conversation history %s" 
           (if ollama-buddy-history-enabled "enabled" "disabled")))

(defun ollama-buddy--display-history ()
  "Display the current conversation history in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Ollama Conversation History*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Ollama Conversation History:\n\n")
        
        (if (null ollama-buddy--conversation-history)
            (insert "No conversation history available.")
          (let ((history-count (/ (length ollama-buddy--conversation-history) 2)))
            (insert (format "Current history: %d message pairs\n\n" history-count))
            
            ;; Display the history in chronological order
            (dolist (msg (reverse ollama-buddy--conversation-history))
              (let* ((role (alist-get 'role msg))
                     (content (alist-get 'content msg))
                     (role-face (if (string= role "user") 
                                    '(:inherit bold :foreground "green") 
                                  '(:inherit bold :foreground "blue"))))
                (insert (propertize (format "[%s]: " (upcase role)) 'face role-face))
                (insert (format "%s\n\n" content))))))
        
        (insert "\nUse M-x ollama-buddy-toggle-history to toggle history")
        (insert "\nUse M-x ollama-buddy-clear-history to clear history")
        (view-mode 1)))
    (display-buffer buf)))

(defun ollama-buddy--update-token-rate-display ()
  "Update the token rate display in real-time."
  (when (and ollama-buddy--current-token-start-time
             (> ollama-buddy--current-token-count 0))
    (let* ((current-time (float-time))
           (interval-tokens (- ollama-buddy--current-token-count 
                              ollama-buddy--last-token-count))
           (interval-time (- current-time 
                            (or ollama-buddy--last-update-time 
                                ollama-buddy--current-token-start-time)))
           (current-rate (if (> interval-time 0)
                             (/ interval-tokens interval-time)
                           0))
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
    (let* ((model (or ollama-buddy--current-model
                      ollama-buddy-default-model
                      "No Model"))
           (history-msg (when (and ollama-buddy-show-history-indicator 
                                   ollama-buddy-history-enabled)
                          (format " [Hist:%d]" 
                                  (/ (length ollama-buddy--conversation-history) 2)))))
      (setq header-line-format
            (concat
             (format (if (string-empty-p (ollama-buddy--update-multishot-status))
                         " [%s%s %s: %s%s]"
                       " [%s %s %s: %s%s]")
                     (ollama-buddy--update-multishot-status)
                     (propertize (if (ollama-buddy--check-status) "RUNNING" "OFFLINE")
                                 'face '(:weight bold))
                     (propertize model 'face
                                 `(:weight bold
                                           :foreground ,(ollama-buddy--get-model-color
                                                         model)))
                     (propertize status 'face '(:weight bold))
                     (or history-msg ""))
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
    
    ;; Set start time if this is the first token and start the update timer
    (unless ollama-buddy--current-token-start-time
      (setq ollama-buddy--current-token-start-time (float-time)
            ollama-buddy--last-token-count 0
            ollama-buddy--last-update-time nil)
      
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
                (insert (format "\n\n[%d tokens in %.1fs, %.1f tokens/sec]" 
                                ollama-buddy--current-token-count
                                elapsed-time
                                token-rate)))
              
              ;; Reset tracking variables
              (setq ollama-buddy--current-token-count 0
                    ollama-buddy--current-token-start-time nil
                    ollama-buddy--last-token-count 0
                    ollama-buddy--last-update-time nil))
            
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
    
    (with-current-buffer ollama-buddy--chat-buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize msg 'face '(:weight bold)))
        (ollama-buddy--show-prompt)))
    
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
  '(    
    (open-chat
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

    (token-stats
     :key ?t
     :description "Token Usage Stats"
     :action ollama-buddy-display-token-stats)
    
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
             "Ollama Buddy: " (nth 0 ollama-buddy--prompt-history)
             nil nil '(ollama-buddy--prompt-history . 1))))
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
         (messages (ollama-buddy--get-history-for-request))
         ;; Add the current prompt to the messages
         (messages (append messages `(((role . "user")
                                       (content . ,prompt)))))
         (payload (json-encode
                   `((model . ,model)
                     (messages . ,(vconcat [] messages))
                     (stream . t)))))
    (setq ollama-buddy--current-model model)
    
    ;; Add the user message to history
    (ollama-buddy--add-to-history "user" prompt)
    
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
           "- Ask me anything!      C-c C-c\n"
           "- Multi-model shot?     C-c l\n"
           "- Change your mind?     C-c k\n"
           "- Change your model?    C-c m\n"
           "- Prompt history?       M-p/M-n\n"
           "- Jump to User prompts? C-c C-p/C-n\n"
           "- In another buffer?    M-x ollama-buddy-menu")))
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
    (define-key map (kbd "C-c p") #'ollama-buddy-previous-conversation-item)
    (define-key map (kbd "C-c n") #'ollama-buddy-next-conversation-item)
    (define-key map (kbd "C-c C-p") #'ollama-buddy-previous-prompt)
    (define-key map (kbd "C-c C-n") #'ollama-buddy-next-prompt)
    (define-key map (kbd "C-c M-p") #'ollama-buddy-previous-response)
    (define-key map (kbd "C-c M-n") #'ollama-buddy-next-response)
    (define-key map (kbd "M-p") #'ollama-buddy--get-prompt-history-element)
    map)
  "Keymap for ollama-buddy mode.")

(define-minor-mode ollama-buddy-mode
  "Minor mode for ollama-buddy keybindings."
  :lighter " OB"
  :keymap ollama-buddy-mode-map)

;; Add to command definitions
(add-to-list 'ollama-buddy-command-definitions
             '(toggle-history
               :key ?H
               :description "Toggle conversation history"
               :action ollama-buddy-toggle-history))

(add-to-list 'ollama-buddy-command-definitions
             '(clear-history
               :key ?X
               :description "Clear conversation history"
               :action ollama-buddy-clear-history))

(add-to-list 'ollama-buddy-command-definitions
             '(show-history
               :key ?V
               :description "View conversation history"
               :action ollama-buddy--display-history))

(add-to-list 'ollama-buddy-command-definitions
             '(previous-item
               :key ?P
               :description "Go to previous prompt/response"
               :action ollama-buddy-previous-conversation-item))

(add-to-list 'ollama-buddy-command-definitions
             '(next-item
               :key ?N
               :description "Go to next prompt/response"
               :action ollama-buddy-next-conversation-item))

(provide 'ollama-buddy)
;;; ollama-buddy.el ends here
