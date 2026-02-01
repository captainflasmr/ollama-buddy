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
(declare-function ollama-buddy-display-token-stats "ollama-buddy")
(declare-function ollama-buddy-toggle-markdown-conversion "ollama-buddy")
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
  :info-manual "(ollama-buddy)Top"
  [
   "|o(Y)o| Ollama Buddy"
     
   ["Chat"
    ("o" "Open Chat" ollama-buddy--open-chat)
    ("A" "Attachments" ollama-buddy-transient-attachment-menu)
    ("b" "Custom Menu " ollama-buddy-menu)
    ]

   ["Actions"
    ("h" "Help/Menu" ollama-buddy--menu-help-assistant)
    ("l" "Send Region" (lambda () (interactive) (ollama-buddy--send-with-command 'send-region)))
    ("k" "Cancel" ollama-buddy--cancel-request)
    ("x" "Toggle Stream  " ollama-buddy-toggle-streaming)
    ]

   ["System Prompts"
    ("s" "User Defined" ollama-buddy-transient-user-prompts-menu)
    ("f" "Fabric" ollama-buddy-transient-fabric-menu)
    ("w" "Awesome" ollama-buddy-transient-awesome-menu)
    ("C-s" "Show" ollama-buddy-show-system-prompt-info)
    ("r" "Reset" ollama-buddy-reset-system-prompt)
    ]
   
   ["Model"
    ("W" "Manage" ollama-buddy-manage-models)
    ("m" "Switch" ollama-buddy--swap-model)
    ("c" "Cloud" ollama-buddy--swap-model-cloud)
    ("i" "Info" ollama-buddy-show-raw-model-info)
    ("M" "Multishot" ollama-buddy--multishot-prompt)
    ]

   ["Cloud Auth"
    ("2" "Sign In" ollama-buddy-cloud-signin)
    ("3" "Sign Out" ollama-buddy-cloud-signout)
    ("4" "Auth Status" ollama-buddy-cloud-status)
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
    ]
   
   ["Display Toggle"
    ("%" "Context Display" ollama-buddy-toggle-context-percentage)
    ("8" "Context Type" ollama-buddy-toggle-context-display-type)
    ("T" "Token Display" ollama-buddy-toggle-token-display)
    ("V" "Reasoning Vis" ollama-buddy-toggle-reasoning-visibility)
    ("9" "Global Prompt" ollama-buddy-toggle-global-system-prompt)
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
    ("Q" "List" ollama-buddy-sessions-list)
    ("Z" "Delete   " ollama-buddy-sessions-delete)
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

(transient-define-prefix ollama-buddy-transient-attachment-menu ()
  "File attachment menu."
  ["File Attachments"
   ("a" "Attach file" ollama-buddy-attach-file)
   ("w" "Show attachments" ollama-buddy-show-attachments)
   ("d" "Detach file" ollama-buddy-detach-file)
   ("0" "Clear all attachments" ollama-buddy-clear-attachments)
   ("q" "Quit" transient-quit-one)])

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

(provide 'ollama-buddy-transient)
;;; ollama-buddy-transient.el ends here
