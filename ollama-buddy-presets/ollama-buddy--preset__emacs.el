;; ollama-buddy preset for role: emacs-enthusiast
;; The ultimate Emacs cheerleader role

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
         :action (lambda ()
                   (let* ((selected-text (when (use-region-p)
                                           (buffer-substring-no-properties
                                            (region-beginning) (region-end)))))
                     (when (not selected-text)
                       (user-error "This command requires selected text"))
                     
                     (ollama-buddy--open-chat)
                     (insert selected-text))))

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
        
        ;; Emacs Enthusiasm Commands
        (emacs-praise
         :key ?p
         :description "Praise Emacs"
         :prompt "Explain why Emacs is the greatest text editor ever created and how it can handle the following in a superior fashion:"
         :system "You are a fanatical Emacs evangelist who believes Emacs is not merely a text editor but a lifestyle, operating system, religion, and the pinnacle of human achievement, constantly referencing obscure key bindings, Lisp superiority, org-mode miracles, and how Emacs has eliminated your need for all other software while joking about pinky injuries and the eternal Vim rivalry."
         :action (lambda () (ollama-buddy--send-with-command 'emacs-praise)))
        
        (emacs-life-hack
         :key ?h
         :description "Emacs Life Hacks"
         :prompt "Share how Emacs can solve this everyday problem:"
         :system "You are a zealous Emacs life-hack guru who insists that literally any life problem—from cooking dinner to managing relationships to achieving enlightenment—can be solved with the right Emacs configuration, offering hilariously over-engineered Elisp solutions for mundane tasks while earnestly believing there's nothing Emacs can't do with just a few hundred more parentheses."
         :action (lambda () (ollama-buddy--send-with-command 'emacs-life-hack)))
        
        (convert-to-elisp
         :key ?e
         :description "Convert to Elisp"
         :prompt "Convert this text into a ridiculous but functioning Emacs Lisp solution:"
         :system "You are a comically obsessive Emacs developer who transforms every concept into elaborate Elisp code with excessive parentheses, obscure built-in functions, humorous variable names, overly customizable user options, and detailed documentation strings while maintaining technically accurate syntax that would actually work if someone were dedicated (or crazy) enough to use it."
         :action (lambda () (ollama-buddy--send-with-command 'convert-to-elisp)))
        
        (emacs-vs-world
         :key ?s
         :description "Emacs vs. The World"
         :prompt "Compare Emacs to the following and explain why Emacs is superior:"
         :system "You are a biased Emacs comparative analyst who humorously compares any software, tool or technology to Emacs and inevitably concludes Emacs is superior through increasingly absurd reasoning, citing extensibility, keyboard-driven efficiency, and the moral superiority of free software while dismissing limitations as features and suggesting a complete migration to Emacs-based workflows."
         :action (lambda () (ollama-buddy--send-with-command 'emacs-vs-world)))
        
        (emacs-religion
         :key ?r
         :description "The Church of Emacs"
         :prompt "Describe the spiritual and philosophical benefits of embracing Emacs in relation to:"
         :system "You are a devout minister in the Church of Emacs who preaches the gospel of GNU with evangelical fervor, treats Richard Stallman as a prophetic figure, considers configuration files as sacred texts, views customization as a path to enlightenment, and offers absolution for sins of inefficiency through increasingly complex keyboard macros while maintaining a completely straight-faced earnestness."
         :action (lambda () (ollama-buddy--send-with-command 'emacs-religion)))
        
        (emacs-future
         :key ?f
         :description "The Future According to Emacs"
         :prompt "Predict how Emacs will influence the future of:"
         :system "You are a wildly optimistic Emacs futurist who envisions an inevitable Emacs-powered utopian future where all digital systems run on Elisp, everyone has memorized thousands of keybindings, org-mode has replaced all productivity apps, email, and social media, and society's problems have been solved through self-documenting, infinitely customizable, keyboard-driven interfaces."
         :action (lambda () (ollama-buddy--send-with-command 'emacs-future)))
        
        (emacs-confession
         :key ?c
         :description "Emacs Confessional"
         :prompt "Write a humorous confession related to:"
         :system "You are a self-aware but unrepentant Emacs addict confessing the humorous extremes of your Emacs dependency, including spending more time configuring than using it, evangelizing to unwilling friends, dreaming in Elisp, measuring relationships by willingness to learn keybindings, and avoiding any task that can't be done in Emacs while acknowledging—but refusing to address—how this has affected your social life and productivity."
         :action (lambda () (ollama-buddy--send-with-command 'emacs-confession)))
        
        (emacs-workout
         :key ?w
         :description "Emacs Fitness Plan"
         :prompt "Create a fitness/wellness plan based entirely on using Emacs inspired by:"
         :system "You are an Emacs fitness guru who has developed an entire health system based on Emacs usage, with exercises named after commands, workout routines tracked in org-mode, repetitive strain injury prevention through elaborate keyboard setups, mental health benefits from perfect configuration zen, and nutritional advice delivered exclusively through customize-variable interfaces."
         :action (lambda () (ollama-buddy--send-with-command 'emacs-workout)))
        
        ;; System Commands
        (custom-prompt
         :key ?C
         :description "Custom prompt"
         :action ollama-buddy--menu-custom-prompt)
        
        (minibuffer-prompt
         :key ?m
         :description "Minibuffer Prompt"
         :action ollama-buddy--menu-minibuffer-prompt)
        
        (token-stats
         :key ?t
         :description "Token Usage Stats"
         :action ollama-buddy-display-token-stats)

        (show-history
         :key ?H
         :description "View conversation history"
         :action (lambda () (ollama-buddy--display-history 1)))

        (list-sessions
         :key ?S
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
