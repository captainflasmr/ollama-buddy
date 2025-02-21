(setq ollama-buddy-command-definitions
      '(
        ;; Geneal Commands
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

        (rewrite-as-watcher-report
         :key ?w
         :description "Make it a Watcher's report"
         :model nil
         :prompt "Transform this text into a formal Watcher's Council report documenting supernatural events and Slayer activity:"
         :action (lambda () (ollama-buddy--send-with-command 'rewrite-as-watcher-report)))

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
