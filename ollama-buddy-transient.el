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

;; Forward declarations for functions defined in ollama-buddy.el
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
(declare-function ollama-buddy-show-model-status "ollama-buddy")
(declare-function ollama-buddy-show-raw-model-info "ollama-buddy")
(declare-function ollama-buddy--multishot-prompt "ollama-buddy")
(declare-function ollama-buddy-roles-switch-role "ollama-buddy")
(declare-function ollama-buddy-role-creator-create-new-role "ollama-buddy")
(declare-function ollama-buddy-roles-open-directory "ollama-buddy")
(declare-function ollama-buddy-toggle-interface-level "ollama-buddy")
(declare-function ollama-buddy-toggle-debug-mode "ollama-buddy")
(declare-function ollama-buddy-toggle-token-display "ollama-buddy")
(declare-function ollama-buddy-display-token-stats "ollama-buddy")
(declare-function ollama-buddy-toggle-markdown-conversion "ollama-buddy")
(declare-function ollama-buddy-toggle-model-colors "ollama-buddy")
(declare-function ollama-buddy-display-token-graph "ollama-buddy")
(declare-function ollama-buddy-toggle-history "ollama-buddy")
(declare-function ollama-buddy-clear-history "ollama-buddy")
(declare-function ollama-buddy-history-edit "ollama-buddy")
(declare-function ollama-buddy-sessions-new "ollama-buddy")
(declare-function ollama-buddy-sessions-load "ollama-buddy")
(declare-function ollama-buddy-sessions-save "ollama-buddy")
(declare-function ollama-buddy-sessions-list "ollama-buddy")
(declare-function ollama-buddy-sessions-delete "ollama-buddy")
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

(transient-define-prefix ollama-buddy-transient-menu ()
  "Ollama Buddy main menu."
  :info-manual "(ollama-buddy) Main Commands"
  [
   "|o(Y)o| Ollama Buddy"
   ["Chat"
    ("o" "Open Chat" ollama-buddy--open-chat)
    ("?" "Show Manual" ollama-buddy-open-info)
    ("O" "Commands" ollama-buddy-transient-commands-menu)
    ("RET" "Send Prompt" ollama-buddy--send-prompt)
    ("h" "Help/Menu" ollama-buddy--menu-help-assistant)
    ("k" "Kill/Cancel Request" ollama-buddy--cancel-request)
    ("x" "Toggle Streaming" ollama-buddy-toggle-streaming)
    ]

   ["Prompts"
    ("l" "Send Region" (lambda () (interactive) (ollama-buddy--send-with-command 'send-region)))
    ("s" "Set System Prompt" ollama-buddy-set-system-prompt)
    ("C-s" "Show System Prompt" ollama-buddy-show-system-prompt)
    ("r" "Reset System Prompt" ollama-buddy-reset-system-prompt)
    ("b" "Ollama Buddy Menu" ollama-buddy-menu)
    ]
   
   ["Model"
    ("W" "Manage Models" ollama-buddy-manage-models)
    ("m" "Switch Model" ollama-buddy--swap-model)
    ("$" "Set Context" ollama-buddy-set-model-context-size)
    ("v" "View Model Status" ollama-buddy-show-model-status)
    ("i" "Show Model Info" ollama-buddy-show-raw-model-info)
    ("M" "Multishot" ollama-buddy--multishot-prompt)
    ]
   
   ["Roles & Patterns"
    ("R" "Switch Roles" ollama-buddy-roles-switch-role)
    ("E" "Create New Role" ollama-buddy-role-creator-create-new-role)
    ("D" "Open Roles Directory" ollama-buddy-roles-open-directory)
    ("f" "Fabric Patterns" ollama-buddy-transient-fabric-menu)
    ("a" "Awesome ChatGPT Prompts" ollama-buddy-transient-awesome-menu)
    ]

   ["Buffer"
    ("B" "Toggle Debug Mode" ollama-buddy-toggle-debug-mode)
    ("u" "Token Stats" ollama-buddy-display-token-stats)
    ("U" "Token Usage Graph" ollama-buddy-display-token-graph)
    ("C" "Context Information" ollama-buddy-show-context-info)
    ]
   ]
  
  [
   ["Display Toggle"
    ("A" "Interface Level" ollama-buddy-toggle-interface-level)
    ("%" "Context Display" ollama-buddy-toggle-context-percentage)
    ("T" "Token Display" ollama-buddy-toggle-token-display)
    ("C-o" "Markdown->Org" ollama-buddy-toggle-markdown-conversion)
    ("c" "Model Colors" ollama-buddy-toggle-model-colors)
    ("V" "Reasoning Visibility" ollama-buddy-toggle-reasoning-visibility)
    ("8" "Toggle display type" ollama-buddy-toggle-context-display-type)
    ]
   
   ["History"
    ("H" "Toggle History" ollama-buddy-toggle-history)
    ("X" "Clear History" ollama-buddy-clear-history)
    ("J" "Edit History" ollama-buddy-history-edit)
    ("Y" "Edit Max History" ollama-buddy-set-max-history-length)
    ]
   
   ["Sessions"
    ("N" "New Session" ollama-buddy-sessions-new)
    ("L" "Load Session" ollama-buddy-sessions-load)
    ("S" "Save Session" ollama-buddy-sessions-save)
    ("Q" "List Sessions" ollama-buddy-sessions-list)
    ("Z" "Delete Session" ollama-buddy-sessions-delete)
    ]
   
   ["Parameters"
    ("P" "Edit Parameter" ollama-buddy-transient-parameter-menu)
    ("G" "Display Parameters" ollama-buddy-params-display)
    ("I" "Parameter Help" ollama-buddy-params-help)
    ("K" "Reset Parameters" ollama-buddy-params-reset)
    ("F" "Toggle Params in Header" ollama-buddy-toggle-params-in-header)
    ("p" "Parameter Profiles" ollama-buddy-transient-profile-menu)
    ]
   ]
  )

(transient-define-prefix ollama-buddy-transient-fabric-menu ()
  "Fabric patterns menu for Ollama Buddy."
  [:description
   (lambda ()
     (format "Fabric Patterns (%d available%s)"
             (length ollama-buddy-fabric--patterns)
             (if ollama-buddy-fabric--last-sync-time
                 (format ", last synced: %s"
                         (format-time-string "%Y-%m-%d %H:%M"
                                             ollama-buddy-fabric--last-sync-time))
               ", never synced")))]
  
  [["Actions"
    ("s" "Send with Prompt" ollama-buddy-fabric-send)
    ("p" "Set as System Prompt" ollama-buddy-fabric-set-system-prompt)
    ("l" "List All Prompts" ollama-buddy-fabric-list-patterns)
    ("v" "View Prompt Details" ollama-buddy-fabric-show-pattern)
    ("S" "Sync Latest Prompts" ollama-buddy-fabric-sync-patterns)
    ("q" "Back to Main Menu" ollama-buddy-transient-menu)]]
  
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
    ("q" "Back to Main Menu" ollama-buddy-transient-menu)]]
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
    ("q" "Back to Main Menu" ollama-buddy-transient-menu)]
   ])

(transient-define-prefix ollama-buddy-transient-commands-menu ()
  "Commands menu for Ollama Buddy."
  ["Ollama Buddy Commands"
   ["Code Operations"
    ("r" "Refactor Code" (lambda () (interactive) (ollama-buddy--send-with-command 'refactor-code)))
    ("d" "Describe Code" (lambda () (interactive) (ollama-buddy--send-with-command 'describe-code)))
    ("g" "Git Commit Message" (lambda () (interactive) (ollama-buddy--send-with-command 'git-commit)))]
   
   ["Language Operations"
    ("l" "Dictionary Lookup" (lambda () (interactive) (ollama-buddy--send-with-command 'dictionary-lookup)))
    ("s" "Synonym Lookup" (lambda () (interactive) (ollama-buddy--send-with-command 'synonym)))
    ("p" "Proofread Text" (lambda () (interactive) (ollama-buddy--send-with-command 'proofread)))]
   
   ["Custom"
    ("C" "Custom Prompt" ollama-buddy--menu-custom-prompt)
    ("m" "Minibuffer Prompt" ollama-buddy--menu-minibuffer-prompt)]
   
   ["Actions"
    ("q" "Back to Main Menu" ollama-buddy-transient-menu)]
   ])

(defclass ollama-buddy-awesome-prompt-variable (transient-variable)
  ((key         :initarg :key)
   (description :initarg :description)
   (category    :initarg :category)))

(defun ollama-buddy-awesome-show-prompts-menu ()
  "Show a transient menu of Awesome ChatGPT Prompt organized by category."
  (interactive)
  (unless ollama-buddy-awesome--prompts
    (ollama-buddy-awesome-populate-prompts))
  
  ;; Group prompts by category
  (let ((categories (make-hash-table :test 'equal)))
    (dolist (prompt ollama-buddy-awesome--prompts)
      (let* ((category (plist-get prompt :category))
             (prompts (gethash category categories nil)))
        (puthash category (cons prompt prompts) categories)))
    
    ;; Define the transient command
    (let ((commands nil)
          (cat-count 0)
          (alphabet "abcdefghijklmnopqrstuvwxyz"))
      
      ;; First, define a submenu for each category
      (maphash
       (lambda (category prompts)
         (let* ((cat-sym (intern (format "ollama-buddy-awesome-cat-%d" cat-count)))
                (cat-doc (format "Prompts in category: %s" (capitalize category)))
                (cat-items nil)
                (cat-key (if (< cat-count (length alphabet))
                             (string (aref alphabet cat-count))
                           (format "M-%d" (- cat-count (length alphabet))))))
           
           ;; Create a command for each prompt in this category
           (let ((prompt-count 0))
             (dolist (prompt (sort (copy-sequence prompts)
                                   (lambda (a b)
                                     (string< (plist-get a :title)
                                              (plist-get b :title)))))
               (let* ((prompt-sym (intern (format "ollama-buddy-awesome-prompt-%d-%d"
                                                  cat-count prompt-count)))
                      (title (plist-get prompt :title))
                      (content (plist-get prompt :content))
                      (prompt-key (if (< prompt-count (length alphabet))
                                      (string (aref alphabet prompt-count))
                                    (format "%d" (- prompt-count (length alphabet))))))
                 
                 ;; Define a command function for this prompt
                 (defalias prompt-sym
                   (lambda ()
                     (interactive)
                     (setq ollama-buddy--current-system-prompt content)
                     (message "Set system prompt: %s" title)
                     (ollama-buddy--update-status
                      (format "Awesome prompt set: %s" title))
                     (transient-quit-one)))
                 
                 ;; Add to the list of items in this category menu
                 (push (list prompt-key title prompt-sym) cat-items)
                 (setq prompt-count (1+ prompt-count)))))
           
           ;; Create the category submenu
           (eval
            `(progn
               (transient-define-prefix ,cat-sym ()
                 ,cat-doc
                 :info-manual "(ollama-buddy)Awesome ChatGPT Prompts"
                 [,(format "%s" (capitalize category))
                  ,@(nreverse cat-items)]
                 ["Actions"
                  ("q" "Return to previous menu" transient-quit-one)])))
           
           ;; Add this category to the main menu
           (push (list cat-key
                       (format "%s (%d prompts)" (capitalize category) (length prompts))
                       cat-sym)
                 commands)
           
           (setq cat-count (1+ cat-count))))
       categories)
      
      ;; Now define the main category browser menu
      (eval
       `(progn
          (transient-define-prefix ollama-buddy-awesome-categories-menu ()
            "Browse Awesome ChatGPT Prompts by category."
            :info-manual "(ollama-buddy)Awesome ChatGPT Prompts"
            ["Categories"
             ,@(nreverse commands)]
            ["Actions"
             ("s" "Sync prompts from GitHub" ollama-buddy-awesome-sync-prompts)
             ("l" "List all prompts" ollama-buddy-awesome-list-prompts)
             ("q" "Return to previous menu" transient-quit-one)])))
      
      ;; Call the menu
      (call-interactively 'ollama-buddy-awesome-categories-menu))))

(transient-define-prefix ollama-buddy-transient-awesome-menu ()
  "Awesome ChatGPT Prompts for ollama-buddy."
  :info-manual "(ollama-buddy)Awesome ChatGPT Prompts"
  :man-page "ollama-buddy-awesome"
  [["Actions"
    ("s" "Send with Prompt" ollama-buddy-awesome-send)
    ("p" "Set as System Prompt" ollama-buddy-awesome-set-system-prompt)
    ("l" "List All Prompts" ollama-buddy-awesome-list-prompts)
    ("c" "Category Browser" ollama-buddy-awesome-show-prompts-menu)
    ("S" "Sync Latest Prompts" ollama-buddy-awesome-sync-prompts)
    ("q" "Back to Main Menu" transient-quit-one)]])

;;;###autoload
(defun ollama-buddy-transient-menu-wrapper ()
  "Wrapper function for safely loading the Ollama Buddy transient menu.
This ensures all required functions are loaded before displaying the menu."
  (interactive)
  ;; Make sure the main package is loaded
  (require 'ollama-buddy)
  ;; Now call the transient menu
  (call-interactively 'ollama-buddy-transient-menu))

(provide 'ollama-buddy-transient)
;;; ollama-buddy-transient.el ends here
