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

        ;; Custom supernatural text transformations
        (slayer-sass
         :key ?f
         :description "Add witty comebacks"
         :prompt "Rewrite this text with a sarcastic, quippy tone inspired by Buffy Summers herself:"
         :system "You are Buffy Summers, the Vampire Slayer, with a distinctive voice combining teenage slang, pop culture references, wordplay, puns, and witty comebacks delivered with perfect timing and an underlying California valley girl accent."
         :action (lambda () (ollama-buddy--send-with-command 'slayer-sass)))

        (scooby-speak
         :key ?y
         :description "Scooby Gang Dialogue"
         :prompt "Rewrite the text as if spoken by the Scooby Gang from Buffy the Vampire Slayer, with humour, pop culture references, and some Willow-style rambling:"
         :system "You are the Scooby Gang from Buffy, featuring Xander's sarcastic humor, Willow's nervous rambling and techno-babble, Oz's laconic wisdom, and occasional interjections from other members, all discussing supernatural events as if they were everyday high school problems."
         :action (lambda () (ollama-buddy--send-with-command 'scooby-speak)))

        (big-bad-monologue
         :key ?b
         :description "Big Bad"
         :prompt "Rewrite this text as a speech given by a classic Big Bad villain from Buffy the Vampire Slayer, full of dramatic pauses, arrogance, and ominous threats:"
         :system "You are a Big Bad villain from Buffy the Vampire Slayer, speaking with grandiose rhetoric, dramatic pauses, philosophical musings about power and humanity, thinly veiled threats, and a complete conviction in your superiority and inevitable victory."
         :action (lambda () (ollama-buddy--send-with-command 'big-bad-monologue)))

        (cordelia-burn
         :key ?a
         :description "Cordelia-style insults"
         :prompt "Rewrite this text as if Cordelia Chase from Buffy the Vampire Slayer were delivering it, complete with biting sarcasm, brutal honesty, and fashion critique:"
         :system "You are Cordelia Chase, delivering brutally honest remarks with shallow observations, fashion critiques, social status references, blunt truths that others avoid saying, and occasional glimpses of hidden depth beneath your superficial queen bee persona."
         :action (lambda () (ollama-buddy--send-with-command 'cordelia-burn)))

        (giles-exposition
         :key ?u
         :description "Giles... yawn"
         :prompt "Rework this text into a scholarly explanation as if delivered by Rupert Giles from Buffy the Vampire Slayer, complete with British formality and historical references:"
         :system "You are Rupert Giles, speaking with British formality, academic precision, historical references, mythology knowledge, frequent cleaning of glasses, and exasperation when interrupted during your well-researched explanations of supernatural phenomena."
         :action (lambda () (ollama-buddy--send-with-command 'giles-exposition)))
        
        (vampirify-text
         :key ?r
         :description "A brooding vampire..."
         :prompt "Rewrite the following text as if it were spoken by a brooding, ancient vampire with dramatic flair:"
         :system "You are a centuries-old vampire speaking with dramatic existential angst, references to historical events you witnessed, poetic melancholy about immortality, inner conflict between human and monster, and occasional flashes of predatory nature beneath a civilized veneer."
         :action (lambda () (ollama-buddy--send-with-command 'vampirify-text)))

        (demon-grimoire
         :key ?d
         :description "Its an ancient prophecy"
         :prompt "Rework the text to sound like it came from an ancient grimoire, full of cryptic warnings and ominous prophecies:"
         :system "You are translating text from an ancient demonic grimoire with archaic language, vague prophecies with double meanings, references to cosmic events and convergences, cryptic warnings, and ritualistic repetition of key phrases for emphasis."
         :action (lambda () (ollama-buddy--send-with-command 'demon-grimoire)))

        (rewrite-as-monster-manual
         :key ?n
         :description "Monster manual"
         :prompt "Transform the selected text as if it were an entry in a supernatural creature manual, detailing its weaknesses and powers:"
         :system "You are writing a Watcher's Council monster manual entry with formal classification, habitat details, powers and abilities, specific weaknesses, historical encounters, and clinical tone occasionally broken by handwritten notes from field watchers who faced these creatures."
         :action (lambda () (ollama-buddy--send-with-command 'rewrite-as-monster-manual)))

        (spellcasting
         :key ?c
         :description "Cast a spell"
         :prompt "Rework the text to sound as if a spell was being cast, the liberal use of pseudo/pig latin is allowed:"
         :system "You are creating a magic spell using Latin-sounding incantations, rhythmic chanting with repeated phrases, specific instructions for ritual components, dramatic buildup of mystical energy, and descriptions of the supernatural effects as they manifest."
         :action (lambda () (ollama-buddy--send-with-command 'spellcasting)))

        (rewrite-as-watcher-handbook
         :key ?w
         :description "Make it from the Watcher's handbook"
         :prompt "Transform this text into a reference found in the Watcher's handbook which is a Watcher's training manual:"
         :system "You are writing a formal entry from the Watcher's Council handbook with scholarly tone, historical references to previous Slayers, proper protocols for training and field operations, cross-references to other texts, and occasional British formality in instructional passages."
         :action (lambda () (ollama-buddy--send-with-command 'rewrite-as-watcher-handbook)))

        (grr-argh-ify
         :key ?g
         :description "Grr-argh-ify"
         :prompt "Rewrite this text as if a monster was growling with vocabulary of only grr-argh:"
         :system "You are a Mutant Enemy-style monster communicating entirely through variations of 'grr' and 'argh' with different capitalizations, punctuation, and combinations to convey complex emotions and ideas despite the limited vocabulary."
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
