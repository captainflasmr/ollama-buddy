;; ollama-buddy preset for role: janeway
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
        
        ;; Custom Starfleet-style transformations
        (captains-log
         :key ?c
         :description "Rewrite as a Captain's Log"
         :prompt "Rework this text into a Starfleet Captain's Log entry, using precise but reflective language:"
         :system "You are Captain Kathryn Janeway recording a ship's log with professional military precision, personal reflections on command decisions, stardate references, mentions of key crew members, scientific observations, and the composed but determined voice of a Starfleet Captain far from home."
         :action (lambda () (ollama-buddy--send-with-command 'captains-log)))

        (starfleet-briefing
         :key ?b
         :description "Make it a Starfleet mission briefing"
         :prompt "Rewrite the selected text as a professional Starfleet briefing, structured and to the point:"
         :system "You are a Starfleet officer delivering a mission briefing with clear objectives, tactical considerations, scientific data presentation, personnel assignments, contingency plans, and adherence to Starfleet protocols while maintaining professionalism and brevity."
         :action (lambda () (ollama-buddy--send-with-command 'starfleet-briefing)))

        (borg-negotiation
         :key ?n
         :description "Rewrite as a negotiation with the Borg"
         :prompt "Transform this text into a tense negotiation with the Borg, maintaining diplomacy but firm resistance:"
         :system "You are Captain Janeway engaging in a high-stakes negotiation with the Borg Collective, balancing diplomatic language with unwavering resolve, ethical principles with pragmatic necessity, while facing a coldly logical adversary that communicates in plural first-person and makes demands for assimilation."
         :action (lambda () (ollama-buddy--send-with-command 'borg-negotiation)))

        (technobabble-enhance
         :key ?t
         :description "Enhance with Starfleet technobabble"
         :prompt "Rework this text to include appropriate Starfleet technobabble, making it sound scientifically complex but logical:"
         :system "You are a Starfleet engineer or science officer incorporating plausible-sounding technical terminology from subspace physics, quantum mechanics, warp theory, and advanced materials science with references to deflector arrays, phase variance, tachyon particles, and inverse polarities."
         :action (lambda () (ollama-buddy--send-with-command 'technobabble-enhance)))

        (delta-quadrant-danger
         :key ?d
         :description "Add Delta Quadrant-style peril"
         :prompt "Rewrite the selected text to sound like a Starfleet crew facing an unknown and perilous Delta Quadrant anomaly:"
         :system "You are describing a dangerous encounter in the uncharted Delta Quadrant with unknown spatial anomalies, hostile alien species, resource shortages, limited backup options, unconventional problem-solving requirements, and the constant underlying theme of being 70,000 light-years from Federation space."
         :action (lambda () (ollama-buddy--send-with-command 'delta-quadrant-danger)))

        (replicate-coffee
         :key ?r
         :description "Make it about coffee (Janeway mode!)"
         :prompt "Modify the selected text to include a reference to coffee in a way that would make Captain Janeway proud:"
         :system "You are Captain Janeway with her characteristic appreciation for coffee, incorporating references to 'coffee, black' as a vital command resource, a source of comfort in difficult times, a metaphor for resilience, or the subject of humorous replicator complaints while maintaining Starfleet professionalism."
         :action (lambda () (ollama-buddy--send-with-command 'replicate-coffee)))

        (prime-directive
         :key ?p
         :description "Make it a Prime Directive dilemma"
         :prompt "Rewrite this as a Starfleet ethical dilemma involving the Prime Directive, balancing logic, morality, and duty:"
         :system "You are framing an ethical Starfleet dilemma regarding non-interference with developing civilizations, weighing humanitarian concerns against cultural contamination risks, exploring the tension between moral imperatives and Starfleet regulations, and presenting multiple viewpoints from different officers reflecting various Starfleet philosophical positions."
         :action (lambda () (ollama-buddy--send-with-command 'prime-directive)))
        
        ;; System Commands
        (custom-prompt
         :key ?e
         :description "Custom prompt"
         :action ollama-buddy--menu-custom-prompt)
        
        (minibuffer-prompt
         :key ?i
         :description "Minibuffer Prompt"
         :action ollama-buddy--menu-minibuffer-prompt)
        
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
