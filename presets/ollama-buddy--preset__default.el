(setq ollama-buddy-command-definitions
  '((open-chat
     :key ?o
     :description "Open chat buffer"
     :model nil
     :action (lambda ()
               (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
               (when (= (buffer-size) 0)
                 (insert (ollama-buddy--create-intro-message)))
               (goto-char (point-max))))
    (show-models
               :key ?v  ; 'v' for view models
               :description "View model status"
               :model nil
               :action ollama-buddy-show-model-status)
    (swap-model
     :key ?m
     :description "Swap model"
     :model nil
     :action (lambda ()
               (if (not (ollama-buddy--ollama-running))
                   (error "!!WARNING!! ollama server not running.")
                 (progn
                   (setq ollama-buddy-current-model 
                         (completing-read "Model: " (ollama-buddy--get-models) nil t))
                   (ollama-buddy--update-status-overlay "Idle")))))
    (help
     :key ?h
     :description "Help assistant"
     :model nil
     :action (lambda ()
               (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
               (goto-char (point-max))
               (insert (ollama-buddy--create-intro-message))))
    (send-region
     :key ?l
     :description "Send region"
     :model nil
     :action (lambda () (ollama-buddy--send-with-command 'send-region)))
    (refactor-code
     :key ?r
     :description "Refactor code"
     :model nil
     :prompt "refactor the following code:"
     :action (lambda () (ollama-buddy--send-with-command 'refactor-code)))
    (git-commit
     :key ?g
     :description "Git commit message"
     :model nil
     :prompt "write a concise git commit message for the following:"
     :action (lambda () (ollama-buddy--send-with-command 'git-commit)))
    (describe-code
     :key ?c
     :description "Describe code"
     :model nil
     :prompt "describe the following code:"
     :action (lambda () (ollama-buddy--send-with-command 'describe-code)))
    (dictionary
     :key ?d
     :description "Dictionary Lookup"
     :model nil
     :prompt-fn (lambda ()
                  (concat "For the word {"
                          (buffer-substring-no-properties (region-beginning) (region-end))
                          "} provide a typical dictionary definition:"))
     :action (lambda () (ollama-buddy--send-with-command 'dictionary)))
    (synonym
     :key ?n
     :description "Word synonym"
     :model nil
     :prompt "list synonyms for word:"
     :action (lambda () (ollama-buddy--send-with-command 'synonym)))
    (proofread
     :key ?p
     :description "Proofread text"
     :model nil
     :prompt "proofread the following:"
     :action (lambda () (ollama-buddy--send-with-command 'proofread)))
    (make-concise
     :key ?z
     :description "Make concise"
     :model nil
     :prompt "reduce wordiness while preserving meaning:"
     :action (lambda () (ollama-buddy--send-with-command 'make-concise)))
    (custom-prompt
     :key ?e
     :description "Custom prompt"
     :model nil
     :action (lambda ()
               (when-let ((prefix (read-string "Enter prompt prefix: " nil nil nil t)))
                 (unless (string-empty-p prefix)
                   (ollama-buddy--send prefix)))))
    (save-chat
     :key ?s
     :description "Save chat"
     :model nil
     :action (lambda ()
               (with-current-buffer ollama-buddy--chat-buffer
                 (write-region (point-min) (point-max) 
                               (read-file-name "Save conversation to: ")
                               'append-to-file
                               nil))))
    (kill-request
     :key ?x
     :description "Kill request"
     :model nil
     :action (lambda ()
               (delete-process ollama-buddy--active-process)))
    (quit
     :key ?q
     :description "Quit"
     :model nil
     :action (lambda () (message "Quit Ollama Shell menu.")))))
