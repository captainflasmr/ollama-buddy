;; ollama-buddy preset for role: buffy
;; Generated manually

(require 'ollama-buddy)

(setq ollama-buddy-command-definitions
      '(
        ;; General Commands
        (open-chat
         :key ?o
         :description "Open chat buffer"
         :action ollama-buddy--open-chat)
        
        (swap-model
         :key ?m
         :description "Swap model"
         :action ollama-buddy--swap-model)
        
        (show-models
         :key ?v
         :description "View model status"
         :action ollama-buddy-show-model-status)

        (send-region
         :key ?l
         :description "Send region"
         :action (lambda () (ollama-buddy--send-with-command 'send-region)))

        (help
         :key ?h
         :description "Help assistant"
         :action ollama-buddy--menu-help-assistant)

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

        ;; Custom supernatural text transformations
        (slayer-sass
         :key ?f
         :description "Add witty comebacks"
         :model nil
         :prompt "Rewrite this text with a sarcastic, quippy tone inspired by Buffy Summers herself:"
         :action (lambda () (ollama-buddy--send-with-command 'slayer-sass)))

        (scooby-speak
         :key ?y
         :description "Scooby Gang Dialogue"
         :model nil
         :prompt "Rewrite the text as if spoken by the Scooby Gang from Buffy the Vampire Slayer, with humour, pop culture references, and some Willow-style rambling:"
         :action (lambda () (ollama-buddy--send-with-command 'scooby-speak)))

        (big-bad-monologue
         :key ?b
         :description "Big Bad"
         :model nil
         :prompt "Rewrite this text as a speech given by a classic Big Bad villain from Buffy the Vampire Slayer, full of dramatic pauses, arrogance, and ominous threats:"
         :action (lambda () (ollama-buddy--send-with-command 'big-bad-monologue)))

        (cordelia-burn
         :key ?a
         :description "Cordelia-style insults"
         :model nil
         :prompt "Rewrite this text as if Cordelia Chase from Buffy the Vampire Slayer were delivering it, complete with biting sarcasm, brutal honesty, and fashion critique:"
         :action (lambda () (ollama-buddy--send-with-command 'cordelia-burn)))

        (giles-exposition
         :key ?u
         :description "Giles... yawn"
         :model nil :prompt "Rework this text into a scholarly explanation as if delivered by Rupert Giles from Buffy the Vampire Slayer, complete with British formality and historical references:"
         :action (lambda () (ollama-buddy--send-with-command 'giles-exposition)))
        
        (vampirify-text
         :key ?r
         :description "A brooding vampire..."
         :model nil
         :prompt "Rewrite the following text as if it were spoken by a brooding, ancient vampire with dramatic flair:"
         :action (lambda () (ollama-buddy--send-with-command 'vampirify-text)))

        (demon-grimoire
         :key ?d
         :description "Its an ancient prophecy"
         :model nil
         :prompt "Rework the text to sound like it came from an ancient grimoire, full of cryptic warnings and ominous prophecies:"
         :action (lambda () (ollama-buddy--send-with-command 'demon-grimoire)))

        (rewrite-as-monster-manual
         :key ?n
         :description "Monster manual"
         :model nil
         :prompt "Transform the selected text as if it were an entry in a supernatural creature manual, detailing its weaknesses and powers:"
         :action (lambda () (ollama-buddy--send-with-command 'rewrite-as-monster-manual)))

        (spellcasting
         :key ?c
         :description "Cast a spell"
         :model nil
         :prompt "Rework the text to sound as if a spell was being cast, the liberal use of pseudo/pig latin is allowed:"
         :action (lambda () (ollama-buddy--send-with-command 'spellcasting)))

        (rewrite-as-watcher-handbook
         :key ?w
         :description "Make it from the Watcher's handbook"
         :model nil
         :prompt "Transform this text into a reference found in the Watcher's handbook which is a Watcher's training manual:"
         :action (lambda () (ollama-buddy--send-with-command 'rewrite-as-watcher-handbook)))

        (grr-argh-ify
         :key ?g
         :description "Grr-argh-ify"
         :model nil
         :prompt "Rewrite this text as if a monster was growling with vocabulary of only grr-argh:"
         :action (lambda () (ollama-buddy--send-with-command 'grr-argh-ify)))

        ;; System Commands
        (custom-prompt
         :key ?e
         :description "Custom prompt"
         :action ollama-buddy--menu-custom-prompt)

        (minibuffer-prompt
         :key ?i
         :description "Minibuffer Prompt"
         :action ollama-buddy--menu-minibuffer-prompt)
        
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

        (toggle-history
         :key ?H
         :description "Toggle conversation history"
         :action ollama-buddy-toggle-history)

        (clear-history
         :key ?X
         :description "Clear conversation history"
         :action (lambda () (ollama-buddy-clear-history 1)))

        (show-history
         :key ?V
         :description "View conversation history"
         :action (lambda () (ollama-buddy--display-history 1)))

        (new-session
         :key ?E
         :description "New session"
         :action ollama-buddy-sessions-new)

        (load-session
         :key ?L
         :description "Load session"
         :action ollama-buddy-sessions-load)

        (save-session
         :key ?S
         :description "Save session"
         :action ollama-buddy-sessions-save)

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
