;; ollama-buddy-transient.el --- Transient menus for Ollama Buddy -*- lexical-binding: t; -*-

;;; Commentary:
;; This extension provides a transient-based menu system for ollama-buddy.
;; It organizes the commands into logical groups with descriptive prefixes.
;; Now includes Fabric pattern integration.

;;; Code:

(require 'transient)
(require 'ollama-buddy-core)  ;; Use core instead of main package
(require 'ollama-buddy-fabric)
(require 'ollama-buddy-awesome)
(require 'ollama-buddy-user-prompts)
(require 'ollama-buddy-rag)

;; Forward declarations for functions defined in ollama-buddy.el
(declare-function ollama-buddy-history-edit-model "ollama-buddy")
(declare-function ollama-buddy-set-max-history-length "ollama-buddy")
(declare-function ollama-buddy-set-model-context-size "ollama-buddy")
(declare-function ollama-buddy-toggle-context-percentage "ollama-buddy")
(declare-function ollama-buddy--open-chat "ollama-buddy")
(declare-function ollama-buddy--send-with-command "ollama-buddy")
(declare-function ollama-buddy-params-edit "ollama-buddy")
(declare-function ollama-buddy--send-prompt "ollama-buddy")
(declare-function ollama-buddy--menu-help-assistant "ollama-buddy")
(declare-function ollama-buddy--cancel-request "ollama-buddy")
(declare-function ollama-buddy-set-system-prompt "ollama-buddy")
(declare-function ollama-buddy-show-system-prompt "ollama-buddy")
(declare-function ollama-buddy-reset-system-prompt "ollama-buddy")
(declare-function ollama-buddy-menu "ollama-buddy")
(declare-function ollama-buddy-manage-models "ollama-buddy")
(declare-function ollama-buddy--swap-model "ollama-buddy")
(declare-function ollama-buddy--swap-model-cloud "ollama-buddy")
(declare-function ollama-buddy-cloud-signin "ollama-buddy")
(declare-function ollama-buddy-cloud-signout "ollama-buddy")
(declare-function ollama-buddy-cloud-status "ollama-buddy")
(declare-function ollama-buddy-show-raw-model-info "ollama-buddy")
(declare-function ollama-buddy--multishot-prompt "ollama-buddy")
(declare-function ollama-buddy-roles-switch-role "ollama-buddy")
(declare-function ollama-buddy-role-creator-create-new-role "ollama-buddy")
(declare-function ollama-buddy-roles-open-directory "ollama-buddy")
(declare-function ollama-buddy-toggle-debug-mode "ollama-buddy")
(declare-function ollama-buddy-toggle-token-display "ollama-buddy")
(declare-function ollama-buddy-toggle-show-history-indicator "ollama-buddy")
(declare-function ollama-buddy-display-token-stats "ollama-buddy")
(declare-function ollama-buddy-toggle-markdown-conversion "ollama-buddy")
(declare-function ollama-buddy-toggle-history "ollama-buddy")
(declare-function ollama-buddy-clear-history "ollama-buddy")
(declare-function ollama-buddy-history-edit "ollama-buddy")
(declare-function ollama-buddy-sessions-new "ollama-buddy")
(declare-function ollama-buddy-sessions-load "ollama-buddy")
(declare-function ollama-buddy-sessions-save "ollama-buddy")
(declare-function ollama-buddy-sessions-directory "ollama-buddy")
(declare-function ollama-buddy-params-display "ollama-buddy")
(declare-function ollama-buddy-params-help "ollama-buddy")
(declare-function ollama-buddy-params-reset "ollama-buddy")
(declare-function ollama-buddy-toggle-params-in-header "ollama-buddy")
(declare-function ollama-buddy-fabric-send "ollama-buddy-fabric")
(declare-function ollama-buddy-fabric-set-system-prompt "ollama-buddy-fabric")
(declare-function ollama-buddy-fabric-list-patterns "ollama-buddy-fabric")
(declare-function ollama-buddy-fabric-show-pattern "ollama-buddy-fabric")
(declare-function ollama-buddy-fabric-sync-patterns "ollama-buddy-fabric")
(declare-function ollama-buddy-fabric-populate-patterns "ollama-buddy-fabric")
(declare-function ollama-buddy-fabric-setup "ollama-buddy-fabric")
(declare-function ollama-buddy-copilot-login "ollama-buddy-copilot")
(declare-function ollama-buddy-copilot-logout "ollama-buddy-copilot")
(declare-function ollama-buddy-copilot-status "ollama-buddy-copilot")
(declare-function ollama-buddy-web-search "ollama-buddy-web-search")
(declare-function ollama-buddy-web-search-attach "ollama-buddy-web-search")
(declare-function ollama-buddy-web-search-count "ollama-buddy-web-search")
(declare-function ollama-buddy-tools-toggle "ollama-buddy-tools")
(declare-function ollama-buddy-tools-info "ollama-buddy-tools")
(declare-function ollama-buddy-rag-index-directory "ollama-buddy-rag")
(declare-function ollama-buddy-rag-search "ollama-buddy-rag")
(declare-function ollama-buddy-rag-attach "ollama-buddy-rag")
(declare-function ollama-buddy-rag-list-indexes "ollama-buddy-rag")
(declare-function ollama-buddy-rag-delete-index "ollama-buddy-rag")
(declare-function ollama-buddy-rag-clear-attached "ollama-buddy-rag")
(declare-function ollama-buddy-rag-count "ollama-buddy-rag")

(transient-define-prefix ollama-buddy-transient-menu ()
  "Ollama Buddy main menu."
  :info-manual "(ollama-buddy)Top"
  [
   "|o(Y)o| Ollama Buddy"
     
   ["Chat"
    ("o" "Open Chat" ollama-buddy--open-chat)
    ("A" "Attachments" ollama-buddy-transient-attachment-menu)
    ("/" "Web Search" ollama-buddy-transient-web-search-menu)
    ("r" "RAG" ollama-buddy-transient-rag-menu)
    ("a" "Authentication" ollama-buddy-transient-auth-menu)
    ("b" "Role Menu" ollama-buddy-role-transient-menu)
    ]

   ["Actions"
    ("h" "Help/Menu" ollama-buddy--menu-help-assistant)
    ("l" "Send Region" (lambda () (interactive) (ollama-buddy--send-with-command 'send-region)))
    ("k" "Cancel" ollama-buddy--cancel-request)
    ("x" "Toggle Stream  " ollama-buddy-toggle-streaming)
    ("W" "Toggle Tools" ollama-buddy-tools-toggle)
    ("Q" "List Tools" ollama-buddy-tools-info)
    ]

   ["System Prompts"
    ("s" "User Defined" ollama-buddy-transient-user-prompts-menu)
    ("f" "Fabric" ollama-buddy-transient-fabric-menu)
    ("w" "Awesome" ollama-buddy-transient-awesome-menu)
    ("C-s" "Show" ollama-buddy-show-system-prompt-info)
    ("C-r" "Reset" ollama-buddy-reset-system-prompt)
    ]

   ["Model"
    ("M" "Manage" ollama-buddy-manage-models)
    ("m" "Switch" ollama-buddy--swap-model)
    ("c" "Cloud" ollama-buddy--swap-model-cloud)
    ("i" "Info" ollama-buddy-show-raw-model-info)
    ("U" "Multishot" ollama-buddy--multishot-prompt)
    ]

   ["Roles"
    ("R" "Switch" ollama-buddy-roles-switch-role)
    ("E" "New" ollama-buddy-role-creator-create-new-role)
    ("D" "Directory" ollama-buddy-roles-open-directory)
    ]
   ]
  
  [
   ["Buffer"
    ("B" "Toggle Debug" ollama-buddy-toggle-debug-mode)
    ("#" "Token Stats" ollama-buddy-display-token-stats)
    ("C" "Context Info" ollama-buddy-show-context-info)
    ("C-o" "Toggle ORG/MD" ollama-buddy-toggle-markdown-conversion)
    ]
   
   ["Display Toggle"
    ("%" "Context Display" ollama-buddy-toggle-context-percentage)
    ("&" "Context Type" ollama-buddy-toggle-context-display-type)
    (">" "History Display" ollama-buddy-toggle-show-history-indicator)
    ("T" "Token Display" ollama-buddy-toggle-token-display)
    ("V" "Reasoning" ollama-buddy-toggle-reasoning-visibility)
    ("<" "Global Prompt" ollama-buddy-toggle-global-system-prompt)
    ("~" "Tone" ollama-buddy-set-tone)
    ]
   
   ["History"
    ("H" "Toggle" ollama-buddy-toggle-history)
    ("X" "Clear" ollama-buddy-clear-history)
    ("J" "Edit" ollama-buddy-history-edit-model)
    ("Y" "Edit Max" ollama-buddy-set-max-history-length)
    ("$" "Context Size" ollama-buddy-set-model-context-size)
    ]
   
   ["Sessions"
    ("N" "New" ollama-buddy-sessions-new)
    ("L" "Load" ollama-buddy-sessions-load)
    ("S" "Save" ollama-buddy-sessions-save)
    ("Z" "Directory" ollama-buddy-sessions-directory)
    ]
   
   ["Parameters"
    ("P" "Edit" ollama-buddy-transient-parameter-menu)
    ("G" "Display" ollama-buddy-params-display)
    ("I" "Help" ollama-buddy-params-help)
    ("K" "Reset" ollama-buddy-params-reset)
    ("F" "Toggle" ollama-buddy-toggle-params-in-header)
    ]
   ]
  )

(transient-define-prefix ollama-buddy-transient-fabric-menu ()
  "Fabric patterns menu for Ollama Buddy."
  [["Fabric Prompts"
    ("s" "Send with Prompt" ollama-buddy-fabric-send)
    ("L" "Set as System Prompt" ollama-buddy-fabric-set-system-prompt)
    ("l" "List All Prompts" ollama-buddy-fabric-list-patterns)
    ("v" "View Prompt Details" ollama-buddy-fabric-show-pattern)
    ("S" "Sync Latest Prompts" ollama-buddy-fabric-sync-patterns)
    ("q" "Quit" ollama-buddy-transient-menu)]]
    (interactive)
  (unless ollama-buddy-fabric--patterns
    (message "Loading Fabric patterns...")
    (ollama-buddy-fabric-populate-patterns))
  (transient-setup 'ollama-buddy-transient-fabric-menu))

(transient-define-prefix ollama-buddy-transient-profile-menu ()
  "Parameter profiles menu for Ollama Buddy."
  ["Parameter Profiles"
   [:description
    (lambda ()
      (format "Current modified parameters: %s"
              (mapconcat #'symbol-name ollama-buddy-params-modified ", ")))]
   
   ["Available Profiles"
    ("d" "Default" (lambda () (interactive)
                     (ollama-buddy-apply-param-profile "Default")
                     (message "Applied Default profile")))
    ("c" "Creative" (lambda () (interactive)
                      (ollama-buddy-apply-param-profile "Creative")
                      (message "Applied Creative profile")))
    ("p" "Precise" (lambda () (interactive)
                     (ollama-buddy-apply-param-profile "Precise")
                     (message "Applied Precise profile")))]
   
   ["Actions"
    ("q" "Quit" ollama-buddy-transient-menu)]]
  (interactive)
  (transient-setup 'ollama-buddy-transient-profile-menu))

(transient-define-prefix ollama-buddy-transient-parameter-menu ()
  "Parameter menu for Ollama Buddy."
  ["Parameters"
   ["Generation"
    ("t" "Temperature" (lambda () (interactive) (ollama-buddy-params-edit 'temperature)))
    ("k" "Top K" (lambda () (interactive) (ollama-buddy-params-edit 'top_k)))
    ("p" "Top P" (lambda () (interactive) (ollama-buddy-params-edit 'top_p)))
    ("m" "Min P" (lambda () (interactive) (ollama-buddy-params-edit 'min_p)))
    ("y" "Typical P" (lambda () (interactive) (ollama-buddy-params-edit 'typical_p)))
    ("r" "Repeat Penalty" (lambda () (interactive) (ollama-buddy-params-edit 'repeat_penalty)))]
   
   ["More Generation"
    ("f" "Frequency Penalty" (lambda () (interactive) (ollama-buddy-params-edit 'frequency_penalty)))
    ("s" "Presence Penalty" (lambda () (interactive) (ollama-buddy-params-edit 'presence_penalty)))
    ("n" "Repeat Last N" (lambda () (interactive) (ollama-buddy-params-edit 'repeat_last_n)))
    ("x" "Stop Sequences" (lambda () (interactive) (ollama-buddy-params-edit 'stop)))
    ("l" "Penalize Newline" (lambda () (interactive) (ollama-buddy-params-edit 'penalize_newline)))]
   
   ["Mirostat"
    ("M" "Mirostat Mode" (lambda () (interactive) (ollama-buddy-params-edit 'mirostat)))
    ("T" "Mirostat Tau" (lambda () (interactive) (ollama-buddy-params-edit 'mirostat_tau)))
    ("E" "Mirostat Eta" (lambda () (interactive) (ollama-buddy-params-edit 'mirostat_eta)))]
   
   ["Resource"
    ("c" "Num Ctx" (lambda () (interactive) (ollama-buddy-params-edit 'num_ctx)))
    ("b" "Num Batch" (lambda () (interactive) (ollama-buddy-params-edit 'num_batch)))
    ("g" "Num GPU" (lambda () (interactive) (ollama-buddy-params-edit 'num_gpu)))
    ("G" "Main GPU" (lambda () (interactive) (ollama-buddy-params-edit 'main_gpu)))
    ("K" "Num Keep" (lambda () (interactive) (ollama-buddy-params-edit 'num_keep)))]
   ]
  
  [["More Resource"
    ("P" "Num Predict" (lambda () (interactive) (ollama-buddy-params-edit 'num_predict)))
    ("S" "Seed" (lambda () (interactive) (ollama-buddy-params-edit 'seed)))
    ("N" "NUMA" (lambda () (interactive) (ollama-buddy-params-edit 'numa)))
    ("V" "Low VRAM" (lambda () (interactive) (ollama-buddy-params-edit 'low_vram)))
    ("o" "Vocab Only" (lambda () (interactive) (ollama-buddy-params-edit 'vocab_only)))]
   
   ["Memory"
    ("m" "Use MMAP" (lambda () (interactive) (ollama-buddy-params-edit 'use_mmap)))
    ("L" "Use MLOCK" (lambda () (interactive) (ollama-buddy-params-edit 'use_mlock)))
    ("C" "Num Thread" (lambda () (interactive) (ollama-buddy-params-edit 'num_thread)))]

   ["Profiles"
    ("d" "Default Profile" (lambda () (interactive)
                             (ollama-buddy-apply-param-profile "Default")
                             (message "Applied Default profile")))
    ("a" "Creative Profile" (lambda () (interactive)
                              (ollama-buddy-apply-param-profile "Creative")
                              (message "Applied Creative profile")))
    ("e" "Precise Profile" (lambda () (interactive)
                             (ollama-buddy-apply-param-profile "Precise")
                             (message "Applied Precise profile")))
    ("A" "All Profiles" ollama-buddy-transient-profile-menu)]
   
   ["Actions"
    ("D" "Display All" ollama-buddy-params-display)
    ("R" "Reset All" ollama-buddy-params-reset)
    ("H" "Help" ollama-buddy-params-help)
    ("F" "Toggle Display in Header" ollama-buddy-toggle-params-in-header)
    ("q" "Quit" ollama-buddy-transient-menu)]
   ])

(transient-define-prefix ollama-buddy-transient-awesome-menu ()
  "Awesome ChatGPT Prompts for ollama-buddy."
  :info-manual "(ollama-buddy)Awesome ChatGPT Prompts"
  :man-page "ollama-buddy-awesome"
  [["Awesome Prompts"
    ("s" "Send with Prompt" ollama-buddy-awesome-send)
    ("L" "Set as System Prompt" ollama-buddy-awesome-set-system-prompt)
    ("l" "List All Prompts" ollama-buddy-awesome-list-prompts)
    ("S" "Sync Latest Prompts" ollama-buddy-awesome-sync-prompts)
    ("q" "Quit" transient-quit-one)]])

(defun ollama-buddy--auth-cloud-description ()
  "Return description for Ollama Cloud auth with status indicator."
  (format "Ollama Cloud %s" (ollama-buddy--cloud-auth-status-indicator)))

(defun ollama-buddy--auth-copilot-description ()
  "Return description for GitHub Copilot auth with status indicator."
  (format "GitHub Copilot %s"
          (if (ollama-buddy--copilot-auth-status-p) "[✓]" "[✗]")))

(transient-define-prefix ollama-buddy-transient-auth-menu ()
  "Authentication menu for browser-based providers."
  [:description
   (lambda () (concat "Authentication - "
                      (or (ollama-buddy--format-auth-status) "No providers")))
   ["Ollama Cloud"
    ("c" "Sign In" ollama-buddy-cloud-signin)
    ("x" "Sign Out" ollama-buddy-cloud-signout)
    ("s" "Status" ollama-buddy-cloud-status)]
   [:if (lambda () (featurep 'ollama-buddy-copilot))
    "GitHub Copilot"
    ("p" "Login" ollama-buddy-copilot-login)
    ("l" "Logout" ollama-buddy-copilot-logout)
    ("t" "Status" ollama-buddy-copilot-status)]
   ["Navigation"
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ollama-buddy-transient-attachment-menu ()
  "File attachment menu."
  ["File Attachments"
   ("a" "Attach file" ollama-buddy-attach-file)
   ("w" "Show attachments" ollama-buddy-show-attachments)
   ("d" "Detach file" ollama-buddy-detach-file)
   ("0" "Clear all attachments" ollama-buddy-clear-attachments)
   ("q" "Quit" transient-quit-one)])

(defun ollama-buddy--web-search-status ()
  "Return web search status for transient display."
  (if (and (featurep 'ollama-buddy-web-search)
           (fboundp 'ollama-buddy-web-search-count))
      (let ((count (ollama-buddy-web-search-count)))
        (if (> count 0)
            (format "♁ %d attached" count)
          "No searches attached"))
    "Module not loaded"))

(transient-define-prefix ollama-buddy-transient-web-search-menu ()
  "Web search menu for Ollama Buddy."
  [:description
   (lambda () (concat "Web Search - " (ollama-buddy--web-search-status)))
   ["Search"
    ("s" "Search & Display" ollama-buddy-web-search)
    ("a" "Search & Attach" ollama-buddy-web-search-attach)]
   ["Manage"
    ("w" "Show Attachments" ollama-buddy-show-attachments)
    ("0" "Clear All" ollama-buddy-clear-attachments)]
   ["Info"
    :description
    (lambda ()
      "Use @search(query) inline in prompts for automatic search")]
   ["Navigation"
    ("q" "Quit" transient-quit-one)]])

(defun ollama-buddy--rag-status ()
  "Return RAG status string for transient menu."
  (let ((count (ollama-buddy-rag-count)))
    (if (> count 0)
        (format "%d attached" count)
      "No context attached")))

(transient-define-prefix ollama-buddy-transient-rag-menu ()
  "RAG (Retrieval-Augmented Generation) menu for Ollama Buddy."
  [:description
   (lambda () (concat "RAG - " (ollama-buddy--rag-status)))
   ["Index"
    ("i" "Index Directory" ollama-buddy-rag-index-directory)
    ("l" "List Indexes" ollama-buddy-rag-list-indexes)
    ("d" "Delete Index" ollama-buddy-rag-delete-index)]
   ["Search"
    ("s" "Search & Display" ollama-buddy-rag-search)
    ("a" "Search & Attach" ollama-buddy-rag-attach)]
   ["Manage"
    ("w" "Show Attachments" ollama-buddy-show-attachments)
    ("0" "Clear RAG Context" ollama-buddy-rag-clear-attached)]
   ["Navigation"
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix ollama-buddy-transient-user-prompts-menu ()
  "Transient menu for user system prompts."
  ["User System Prompts"
   [("S" "Save Current" ollama-buddy-user-prompts-save)
    ("L" "Set as System Prompt" ollama-buddy-user-prompts-load)
    ("N" "Create New" ollama-buddy-user-prompts-create-new)
    ("l" "List All Prompts" ollama-buddy-user-prompts-list)]
   [("e" "Edit Prompt" ollama-buddy-user-prompts-edit)
    ("s" "Set with Current Prompt" ollama-buddy-set-system-prompt)
    ("d" "Delete Prompt" ollama-buddy-user-prompts-delete)
    ("r" "Reset Prompt" ollama-buddy-reset-system-prompt)]
   [("q" "Quit" transient-quit-one)]])

;;;###autoload
(defun ollama-buddy-transient-menu-wrapper ()
  "Wrapper function for safely loading the Ollama Buddy transient menu.
This ensures all required functions are loaded before displaying the menu."
  (interactive)
  ;; Make sure the main package is loaded
  (require 'ollama-buddy)
  ;; Now call the transient menu
  (call-interactively 'ollama-buddy-transient-menu))

(defun ollama-buddy--role-menu-ensure-command (cmd-def)
  "Create or update an interactive command for CMD-DEF.
CMD-DEF is a single entry from `ollama-buddy-command-definitions':
  (NAME :key ?x :description \"...\" :action FN ...)
Returns the interned command symbol."
  (let* ((name (car cmd-def))
         (plist (cdr cmd-def))
         (action (plist-get plist :action))
         (sym (intern (format "ollama-buddy-role-cmd--%s" name))))
    (defalias sym
      (if (and (symbolp action) (commandp action))
          action
        (lambda ()
          (interactive)
          (funcall action))))
    (put sym 'function-documentation
         (or (plist-get plist :description) (symbol-name name)))
    sym))

(defun ollama-buddy--role-menu-build-groups ()
  "Build transient group vectors from `ollama-buddy-command-definitions'."
  (let ((groups (make-hash-table :test 'equal))
        (group-order nil)
        (seen-keys (make-hash-table :test 'equal)))
    (dolist (cmd-def ollama-buddy-command-definitions)
      (let* ((name (car cmd-def))
             (plist (cdr cmd-def))
             (key (plist-get plist :key))
             (key-str (when key (char-to-string key))))
        ;; Skip quit and entries without keys, deduplicate keys
        (when (and key-str
                   (not (eq name 'quit))
                   (not (gethash key-str seen-keys)))
          (puthash key-str t seen-keys)
          (let* ((group-name (or (plist-get plist :group) "Commands"))
                 (desc (or (plist-get plist :description) (symbol-name name)))
                 (model (plist-get plist :model))
                 (full-desc (if model (format "%s [%s]" desc model) desc))
                 (sym (ollama-buddy--role-menu-ensure-command cmd-def))
                 (spec (list key-str full-desc sym)))
            (unless (gethash group-name groups)
              (push group-name group-order))
            (puthash group-name
                     (append (gethash group-name groups) (list spec))
                     groups)))))
    (mapcar (lambda (gname)
              (apply #'vector gname (gethash gname groups)))
            (nreverse group-order))))

(defun ollama-buddy--selection-status ()
  "Return a string describing the current selection status."
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (chars (- end beg))
             (lines (count-lines beg end)))
        (format "Selection: %d chars, %d lines" chars lines))
    "No selection"))

(defun ollama-buddy-role-transient-menu ()
  "Dynamic role-specific command menu.
Rebuilds the transient prefix each invocation to reflect the
current role's `ollama-buddy-command-definitions'."
  (interactive)
  (let ((group-vectors (ollama-buddy--role-menu-build-groups))
        (selection-status (ollama-buddy--selection-status)))
    (eval
     `(transient-define-prefix ollama-buddy--role-transient-menu-impl ()
        "Dynamic role-specific command menu."
        [:description
         (lambda () ,selection-status)
         ,@group-vectors]))
    (transient-setup 'ollama-buddy--role-transient-menu-impl)))

(provide 'ollama-buddy-transient)
;;; ollama-buddy-transient.el ends here
