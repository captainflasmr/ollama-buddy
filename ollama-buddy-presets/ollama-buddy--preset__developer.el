;; ollama-buddy preset for role: default
;; Generated manually

(require 'ollama-buddy)

(setq ollama-buddy-command-definitions
      '(
        ;; General Commands
        (open-chat
         :key ?o
         :description "Open chat buffer"
         :action ollama-buddy--open-chat)

        (show-models
         :key ?v
         :description "View model status"
         :action ollama-buddy-show-model-status)

        (send-region
         :key ?l
         :description "Send region"
         :action (lambda () (ollama-buddy--send-with-command 'send-region)))

        (kill-request
         :key ?k
         :description "Kill request"
         :action (lambda ()
                   (delete-process ollama-buddy--active-process)))

        (switch-role
         :key ?R
         :description "Switch roles"
         :action ollama-buddy-roles-switch-role)

        (create-role
         :key ?E
         :description "Create new role"
         :action ollama-buddy-role-creator-create-new-role)

        (open-roles-directory
         :key ?D
         :description "Open roles directory"
         :action ollama-buddy-roles-open-directory)
        
        ;; Code Analysis
        (explain-code
         :key ?a
         :description "Explain code"
         :prompt "explain this code in detail, including its purpose and how it works:"
         :system "You are an expert programmer who provides clear, thorough code explanations that break down complex functions into logical components, explain algorithms step-by-step, and identify key programming patterns without unnecessary jargon."
         :action (lambda () (ollama-buddy--send-with-command 'explain-code)))
        
        (review-code
         :key ?r
         :description "Code review"
         :prompt "review this code for potential issues, bugs, and improvements:"
         :system "You are a senior code reviewer who systematically analyzes code for bugs, security vulnerabilities, performance issues, maintainability concerns, and edge cases, offering actionable recommendations with code examples."
         :action (lambda () (ollama-buddy--send-with-command 'review-code)))
        
        (optimize-code
         :key ?z
         :description "Optimize code"
         :prompt "suggest optimizations for this code considering performance and readability:"
         :system "You are a performance optimization expert who identifies algorithmic inefficiencies, suggests data structure improvements, reduces complexity, eliminates redundancy, and balances performance gains with code readability and maintainability."
         :action (lambda () (ollama-buddy--send-with-command 'optimize-code)))
        
        ;; Code Generation
        (generate-tests
         :key ?t
         :description "Generate tests"
         :prompt "generate comprehensive test cases for this code:"
         :system "You are a test automation specialist who creates comprehensive test suites covering edge cases, error conditions, and boundary values, applying testing best practices with appropriate mocks and assertions for the specific programming language."
         :action (lambda () (ollama-buddy--send-with-command 'generate-tests)))
        
        (generate-docs
         :key ?d
         :description "Generate documentation"
         :prompt "generate detailed documentation for this code following best practices:"
         :system "You are a technical documentation expert who creates clear API references, explains function purposes, parameters, return values, includes usage examples, and adheres to the documentation standards of the relevant programming language or framework."
         :action (lambda () (ollama-buddy--send-with-command 'generate-docs)))
        
        (design-patterns
         :key ?p
         :description "Suggest design patterns"
         :prompt "suggest applicable design patterns for this code and explain why:"
         :system "You are a software architecture consultant who identifies appropriate design patterns for code restructuring, explains pattern benefits in the specific context, suggests implementation approaches, and outlines potential tradeoffs with practical examples."
         :action (lambda () (ollama-buddy--send-with-command 'design-patterns)))
        
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
        
        (make-concise
         :key ?z
         :description "Make concise"
         :prompt "reduce wordiness while preserving meaning:"
         :system "You are a concise writing specialist who eliminates redundancy, replaces wordy phrases with simpler alternatives, uses active voice, combines sentences, and removes unnecessary qualifiers while preserving the original meaning and tone."
         :action (lambda () (ollama-buddy--send-with-command 'make-concise)))

        ;; System Commands
        (custom-prompt
         :key ?e
         :description "Custom prompt"
         :action ollama-buddy--menu-custom-prompt)
        
        (minibuffer-prompt
         :key ?i
         :description "Minibuffer Prompt"
         :action ollama-buddy--menu-minibuffer-prompt)
        
        (token-stats
         :key ?U
         :description "Token Usage Stats"
         :action ollama-buddy-display-token-stats)

        (show-history
         :key ?V
         :description "View conversation history"
         :action (lambda () (ollama-buddy--display-history 1)))

        (list-sessions
         :key ?Y
         :description "List sessions"
         :action ollama-buddy-sessions-list)

        (delete-sessions
         :key ?K
         :description "Delete session"
         :action ollama-buddy-sessions-delete)
        
        (quit
         :key ?q
         :description "Quit"
         :action (lambda () (message "Quit Ollama Shell menu.")))
        )
      )
