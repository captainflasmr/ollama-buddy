(setq ollama-buddy-command-definitions
      '(
        ;; General Commands
        (open-chat
         :key ?o
         :description "Open chat buffer"
         :action (lambda ()
                   (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
                   (when (= (buffer-size) 0)
                     (insert (ollama-buddy--create-intro-message))
                     (ollama-buddy--show-prompt))
                   (goto-char (point-max))))
        
        (show-models
         :key ?v
         :description "View model status"
         :action ollama-buddy-show-model-status)
        
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
        
        ;; Custom text transformation commands
        
        (bardify-text
         :key ?b
         :description "Turn text into Shakespearean prose"
         :model nil
         :prompt "Rewrite the following text in Shakespearean language, using poetic and dramatic phrasing:"
         :action (lambda () (ollama-buddy--send-with-command 'bardify-text)))

        (write-sonnet
         :key ?t
         :description "Convert text into a sonnet"
         :model nil
         :prompt "Transform the selected text into a 14-line Shakespearean sonnet:"
         :action (lambda () (ollama-buddy--send-with-command 'write-sonnet)))

        (translate-olde-english
         :key ?d
         :description "Ye Olde English"
         :model nil
         :prompt "Convert this modern text into Olde English style:"
         :action (lambda () (ollama-buddy--send-with-command 'translate-olde-english)))
        
        ;; System Commands
        (custom-prompt
         :key ?e
         :description "Custom prompt"
         :action (lambda ()
                   (when-let ((prefix (read-string "Enter prompt prefix: " nil nil nil t)))
                     (unless (use-region-p)
                       (user-error "No region selected. Select text to use with prompt"))
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
        
        (quit
         :key ?q
         :description "Quit"
         :action (lambda () (message "Quit Ollama Shell menu.")))
        )
      )
