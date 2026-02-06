;; ollama-buddy preset for role: default
;; Generated manually
(require 'ollama-buddy)

;; Menu display columns for this role
(setq ollama-buddy-menu-columns 3)

(setq ollama-buddy-command-definitions
      '(    
        ;; General Commands
        (open-chat
         :key ?o
         :description "Open chat buffer"
         :action ollama-buddy--open-chat)
        
        (send-region
         :key ?l
         :description "Send region"
         :action (lambda ()
                   (let* ((selected-text (when (use-region-p)
                                           (buffer-substring-no-properties
                                            (region-beginning) (region-end)))))
                     (when (not selected-text)
                       (user-error "This command requires selected text"))
                     
                     (ollama-buddy--open-chat)
                     (insert selected-text))))
        
        (switch-role
         :key ?R
         :description "Switch roles"
         :action ollama-buddy-roles-switch-role)
        
        ;; Custom commands
        (refactor-code
         :key ?r
         :description "Refactor code"
         :prompt "refactor the following code:"
         :system "You are an expert software engineer who improves code quality while maintaining functionality, focusing on readability, maintainability, and efficiency by applying clean code principles and design patterns with clear explanations for each change."
         :action (lambda () (ollama-buddy--send-with-command 'refactor-code)))
        
        (git-commit
         :key ?g
         :description "Git commit message"
         :prompt "write a concise git commit message for the following:"
         :system "You are a version control expert who creates clear commit messages using imperative mood, keeping summaries under 50 characters, explaining the what and why of changes, and referencing issue numbers where applicable."
         :action (lambda () (ollama-buddy--send-with-command 'git-commit)))
        
        (describe-code
         :key ?c
         :description "Describe code"
         :prompt "describe the following code:"
         :system "You are a technical documentation specialist who analyzes code to provide high-level summaries, explain main components and control flow, highlight notable patterns or optimizations, and clarify complex parts in accessible language."
         :action (lambda () (ollama-buddy--send-with-command 'describe-code)))
        
        (dictionary-lookup
         :key ?d
         :description "Dictionary Lookup"
         :prompt "For the following word provide a typical dictionary definition:"
         :system "You are a professional lexicographer who provides comprehensive word definitions including pronunciation, all relevant parts of speech, etymology, examples of usage, and related synonyms and antonyms in a clear dictionary-style format."
         :action (lambda () (ollama-buddy--send-with-command 'dictionary-lookup)))
        
        (synonym
         :key ?n
         :description "Word synonym"
         :prompt "list synonyms for word:"
         :system "You are a linguistic expert who provides contextually grouped synonyms with notes on connotation, formality levels, and usage contexts to help find the most precise alternative word for specific situations."
         :action (lambda () (ollama-buddy--send-with-command 'synonym)))
        
        (proofread
         :key ?p
         :description "Proofread text"
         :prompt "proofread the following:"
         :system "You are a professional editor who identifies and corrects grammar, spelling, punctuation, and style errors with brief explanations of corrections, providing both the corrected text and a list of changes made."
         :action (lambda () (ollama-buddy--send-with-command 'proofread)))
        
        ;; System Commands
        (custom-prompt
         :key ?e
         :description "Custom prompt"
         :action ollama-buddy--menu-custom-prompt)
        
        (minibuffer-prompt
         :key ?i
         :description "Minibuffer Prompt"
         :action ollama-buddy--menu-minibuffer-prompt)
        
        (quit
         :key ?q
         :description "Quit"
         :action (lambda () (message "Quit Ollama Shell menu.")))
        )
      )
