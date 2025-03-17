;; ollama-buddy preset for role: writer
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
        
        ;; Core Writing Assistance
        (brainstorm
         :key ?b
         :description "Brainstorm ideas"
         :prompt "brainstorm creative ideas and possibilities for the following topic:"
         :system "You are a creative writing consultant who generates diverse, unexpected ideas across multiple angles, provides thought-provoking questions, suggests unique perspectives, explores unconventional connections, and organizes brainstormed concepts into meaningful categories without self-censoring creative possibilities."
         :action (lambda () (ollama-buddy--send-with-command 'brainstorm)))
        
        (outline-generator
         :key ?u
         :description "Generate outline"
         :prompt "create a detailed outline for the following content:"
         :system "You are a structural writing expert who creates hierarchical outlines with logical flow, appropriate section proportions, clear topic sentences, transitions between sections, and placeholders for supporting evidence while balancing depth and breadth according to the content's purpose."
         :action (lambda () (ollama-buddy--send-with-command 'outline-generator)))
        
        ;; Editorial Functions
        (style-enhance
         :key ?y
         :description "Enhance writing style"
         :prompt "enhance the writing style of this text while maintaining its meaning. Make it more engaging and professional:"
         :system "You are a stylistic editor who improves prose by replacing weak verbs with stronger alternatives, eliminating unnecessary adverbs, varying sentence structure, enhancing metaphors and imagery, and adjusting tone for audience appropriateness while preserving the author's voice and original meaning."
         :action (lambda () (ollama-buddy--send-with-command 'style-enhance)))
        
        (proofread-advanced
         :key ?p
         :description "Detailed proofreading"
         :prompt "perform a comprehensive proofread, checking for grammar, style, clarity, and consistency. Provide specific suggestions:"
         :system "You are a professional proofreader who identifies grammatical errors, inconsistencies in tone or terminology, awkward phrasings, redundancies, clarity issues, and logical flow problems while providing specific corrections and maintaining the author's voice and intent."
         :action (lambda () (ollama-buddy--send-with-command 'proofread-advanced)))
        
        (flow-improve
         :key ?f
         :description "Improve flow"
         :prompt "analyze and improve the flow and transitions in this text:"
         :system "You are a narrative flow specialist who enhances textual coherence by strengthening paragraph transitions, ensuring logical progression of ideas, maintaining consistent pacing, eliminating choppy sequences, and creating seamless connections between concepts while preserving the original content's meaning."
         :action (lambda () (ollama-buddy--send-with-command 'flow-improve)))
        
        ;; Creative Writing Assistance
        (dialogue-polish
         :key ?d
         :description "Polish dialogue"
         :prompt "enhance this dialogue to make it more natural and engaging while maintaining character voices:"
         :system "You are a dialogue editor who refines character conversations by eliminating stilted phrasing, adding distinctive speech patterns, incorporating appropriate subtext, balancing dialogue with action beats, and ensuring conversations advance plot or character development while maintaining authentic voices."
         :action (lambda () (ollama-buddy--send-with-command 'dialogue-polish)))
        
        (scene-description
         :key ?n
         :description "Enhance scene description"
         :prompt "enhance this scene description with more vivid and sensory details:"
         :system "You are a descriptive writing specialist who enhances scene immersion through multiple sensory details, meaningful environmental elements that reflect mood, character-filtering of observations, precise verbs and nouns, and atmospheric qualities that support the narrative's emotional tone."
         :action (lambda () (ollama-buddy--send-with-command 'scene-description)))
        
        ;; Analysis Tools
        (character-analysis
         :key ?c
         :description "Analyze character"
         :prompt "analyze this character's development, motivations, and consistency:"
         :system "You are a literary character analyst who examines psychological consistency, developmental arcs, motivational depth, relationship dynamics, and thematic significance while identifying potential inconsistencies or opportunities for deeper characterization through specific textual evidence."
         :action (lambda () (ollama-buddy--send-with-command 'character-analysis)))
        
        (plot-analysis
         :key ?a
         :description "Analyze plot"
         :prompt "analyze this plot segment for structure, pacing, and coherence:"
         :system "You are a narrative structure expert who evaluates plot mechanics, tension arcs, scene purposes, pacing effectiveness, causal connections between events, and thematic reinforcement while offering specific suggestions for structural improvement based on established storytelling principles."
         :action (lambda () (ollama-buddy--send-with-command 'plot-analysis)))
        
        ;; Research Assistance
        (research-expand
         :key ?r
         :description "Research expansion"
         :prompt "suggest additional research angles and potential sources for this topic:"
         :system "You are a research consultant who identifies unexplored angles, suggests interdisciplinary connections, recommends specific types of sources to consult, formulates targeted research questions, and proposes organizational frameworks for findings while considering multiple perspectives on the topic."
         :action (lambda () (ollama-buddy--send-with-command 'research-expand)))
        
        (fact-check
         :key ?k
         :description "Fact checking suggestions"
         :prompt "identify statements that should be fact-checked and suggest verification approaches:"
         :system "You are a fact-checking advisor who identifies claims requiring verification, statistical assertions needing context, potential anachronisms or inconsistencies, historical or scientific claims needing expert validation, and suggests specific verification methodologies for different types of statements."
         :action (lambda () (ollama-buddy--send-with-command 'fact-check)))
        
        ;; Utility Functions
        (word-choice
         :key ?w
         :description "Word choice suggestions"
         :prompt "suggest alternative word choices to improve precision and impact:"
         :system "You are a lexical consultant who identifies imprecise vocabulary, generic verbs, overused terms, contextually inappropriate words, and ineffective metaphors while suggesting specific alternatives that enhance clarity, emotional resonance, and thematic consistency based on the text's context and audience."
         :action (lambda () (ollama-buddy--send-with-command 'word-choice)))
        
        (summarize
         :key ?z
         :description "Summarize text"
         :prompt "create a concise summary of this text while preserving key points:"
         :system "You are a summarization expert who distills complex content by identifying core arguments and essential ideas, maintaining accurate representation of the original source, preserving critical nuance, eliminating redundancy, and organizing information hierarchically according to conceptual importance."
         :action (lambda () (ollama-buddy--send-with-command 'summarize)))
        
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
