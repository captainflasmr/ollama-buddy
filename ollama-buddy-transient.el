;; ollama-buddy-transient.el --- Transient menus for Ollama Buddy -*- lexical-binding: t; -*-

;;; Commentary:
;; This extension provides a transient-based menu system for ollama-buddy.
;; It organizes the commands into logical groups with descriptive prefixes.

;;; Code:

(require 'transient)
(require 'ollama-buddy)

(transient-define-prefix ollama-buddy-transient-main-menu ()
  "Ollama Buddy main menu."
  :info-manual "(ollama-buddy) Main Commands"
  [
   ;; " n___n\n|o(Y)o| Ollama Buddy\n===================="
   "|o(Y)o| Ollama Buddy"
   ;; " n___n\n|o(Y)o|\n"
   ["Chat"
    ("RET" "Send Prompt" ollama-buddy--send-prompt)
    ("h" "Help/Menu" ollama-buddy--menu-help-assistant)
    ("o" "Open Chat" ollama-buddy--open-chat)
    ("l" "Send Region" (lambda () (interactive) (ollama-buddy--send-with-command 'send-region)))
    ("k" "Kill/Cancel Request" ollama-buddy--cancel-request)
    ("O" "Commands" ollama-buddy-transient-commands-menu)
    ]
   
   ["Model"
    ("m" "Switch Model" ollama-buddy--swap-model)
    ("v" "View Model Status" ollama-buddy-show-model-status)
    ("i" "Show Model Info" ollama-buddy-show-raw-model-info)
    ("M" "Multishot (Multiple Models)" ollama-buddy--multishot-prompt)]
   
   ["Roles"
    ("R" "Switch Roles" ollama-buddy-roles-switch-role)
    ("E" "Create New Role" ollama-buddy-role-creator-create-new-role)
    ("D" "Open Roles Directory" ollama-buddy-roles-open-directory)]
   
   ["Prompts"
    ("s" "Set System Prompt" ollama-buddy-set-system-prompt)
    ("C-s" "Show System Prompt" ollama-buddy-show-system-prompt)
    ("r" "Reset System Prompt" ollama-buddy-reset-system-prompt)]
   ]
  
  [["History"
    ("H" "Toggle History" ollama-buddy-toggle-history)
    ("X" "Clear History" ollama-buddy-clear-history)
    ("V" "Display History" ollama-buddy-display-history)
    ("J" "Edit History" ollama-buddy-history-edit)]
   
   ["Sessions"
    ("N" "New Session" ollama-buddy-sessions-new)
    ("L" "Load Session" ollama-buddy-sessions-load)
    ("S" "Save Session" ollama-buddy-sessions-save)
    ("Q" "List Sessions" ollama-buddy-sessions-list)
    ("Z" "Delete Session" ollama-buddy-sessions-delete)]
   
   ["Parameters"
    ("P" "Edit Parameter" ollama-buddy-transient-parameter-menu)
    ("G" "Display Parameters" ollama-buddy-params-display)
    ("I" "Parameter Help" ollama-buddy-params-help)
    ("K" "Reset Parameters" ollama-buddy-params-reset)
    ("F" "Toggle Params in Header" ollama-buddy-toggle-params-in-header)]
   
   ["Display Options"
    ("A" "Toggle Interface Level" ollama-buddy-toggle-interface-level)
    ("B" "Toggle Debug Mode" ollama-buddy-toggle-debug-mode)
    ("T" "Toggle Token Display" ollama-buddy-toggle-token-display)
    ("U" "Display Token Stats" ollama-buddy-display-token-stats)
    ("C-o" "Toggle Markdown->Org" ollama-buddy-toggle-markdown-conversion)]
   ])

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
   ]
  
  [   ["Resource"
       ("c" "Num Ctx" (lambda () (interactive) (ollama-buddy-params-edit 'num_ctx)))
       ("b" "Num Batch" (lambda () (interactive) (ollama-buddy-params-edit 'num_batch)))
       ("g" "Num GPU" (lambda () (interactive) (ollama-buddy-params-edit 'num_gpu)))
       ("G" "Main GPU" (lambda () (interactive) (ollama-buddy-params-edit 'main_gpu)))
       ("K" "Num Keep" (lambda () (interactive) (ollama-buddy-params-edit 'num_keep)))]
      
      ["More Resource"
       ("P" "Num Predict" (lambda () (interactive) (ollama-buddy-params-edit 'num_predict)))
       ("S" "Seed" (lambda () (interactive) (ollama-buddy-params-edit 'seed)))
       ("N" "NUMA" (lambda () (interactive) (ollama-buddy-params-edit 'numa)))
       ("V" "Low VRAM" (lambda () (interactive) (ollama-buddy-params-edit 'low_vram)))
       ("o" "Vocab Only" (lambda () (interactive) (ollama-buddy-params-edit 'vocab_only)))]
      
      ["Memory"
       ("m" "Use MMAP" (lambda () (interactive) (ollama-buddy-params-edit 'use_mmap)))
       ("L" "Use MLOCK" (lambda () (interactive) (ollama-buddy-params-edit 'use_mlock)))
       ("C" "Num Thread" (lambda () (interactive) (ollama-buddy-params-edit 'num_thread)))]
      ]
  
  [["Actions"
    ("D" "Display All" ollama-buddy-params-display)
    ("R" "Reset All" ollama-buddy-params-reset)
    ("H" "Help" ollama-buddy-params-help)
    ("F" "Toggle Display in Header" ollama-buddy-toggle-params-in-header)
    ("q" "Back to Main Menu" ollama-buddy-transient-main-menu)]
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
    ("c" "Custom Prompt" ollama-buddy--menu-custom-prompt)
    ("m" "Minibuffer Prompt" ollama-buddy--menu-minibuffer-prompt)]
   
   ["Actions"
    ("q" "Back to Main Menu" ollama-buddy-transient-main-menu)]
   ])

;;;###autoload
(defun ollama-buddy-transient-menu ()
  "Show the Ollama Buddy transient menu."
  (interactive)
  (ollama-buddy-transient-main-menu))

(provide 'ollama-buddy-transient)
;;; ollama-buddy-transient.el ends here
