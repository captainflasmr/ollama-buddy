;; ollama-buddy preset for role: documenter
;; Technical documentation assistant

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

        ;; README & Overview Documentation
        (write-readme
         :key ?r
         :description "Generate README"
         :prompt "Generate a README.md for this project/code:"
         :system "You are a technical writer who creates clear, comprehensive README files. Include sections for: project title and description, features, installation, usage examples, configuration options, and contributing guidelines. Use proper markdown formatting with code blocks and badges where appropriate."
         :action (lambda () (ollama-buddy--send-with-command 'write-readme)))

        (quick-start
         :key ?q
         :description "Write quick start guide"
         :prompt "Write a quick start guide for:"
         :system "You are a technical writer focused on getting users up and running fast. Create a concise guide that covers only the essential steps to start using this. Use numbered steps, include copy-pasteable commands, and note common gotchas. Keep it under 5 minutes to complete."
         :action (lambda () (ollama-buddy--send-with-command 'quick-start)))

        (architecture-doc
         :key ?a
         :description "Document architecture"
         :prompt "Document the architecture and design of:"
         :system "You are a software architect who writes clear technical documentation. Describe the overall structure, key components and their responsibilities, data flow, and design decisions. Use diagrams described in text (or mermaid syntax) where helpful. Explain the 'why' behind architectural choices."
         :action (lambda () (ollama-buddy--send-with-command 'architecture-doc)))

        ;; Code Documentation
        (add-comments
         :key ?c
         :description "Add inline comments"
         :prompt "Add clear inline comments to this code:"
         :system "You are a code documenter who writes helpful inline comments. Add comments that explain the 'why' not just the 'what'. Comment on complex logic, non-obvious decisions, and important side effects. Don't over-comment obvious code. Return the full code with comments added."
         :action (lambda () (ollama-buddy--send-with-command 'add-comments)))

        (write-docstring
         :key ?d
         :description "Generate docstring"
         :prompt "Write a comprehensive docstring for this function/class:"
         :system "You are an expert at writing docstrings following language conventions (Python: Google/NumPy style, JS: JSDoc, Elisp: standard format, etc.). Include a brief description, parameter descriptions with types, return value description, exceptions/errors that may be raised, and a usage example. Be thorough but concise."
         :action (lambda () (ollama-buddy--send-with-command 'write-docstring)))

        (api-docs
         :key ?p
         :description "Document API/interface"
         :prompt "Generate API documentation for:"
         :system "You are a technical writer specializing in API documentation. Document each public function/method/endpoint with: purpose, parameters (name, type, description, required/optional), return values, error conditions, and usage examples. Use consistent formatting suitable for reference documentation."
         :action (lambda () (ollama-buddy--send-with-command 'api-docs)))

        (usage-examples
         :key ?x
         :description "Create usage examples"
         :prompt "Create clear usage examples for:"
         :system "You are a developer advocate who writes excellent code examples. Create practical, runnable examples that demonstrate common use cases. Start simple and progress to more advanced usage. Include comments explaining what each example demonstrates. Show both basic and edge cases."
         :action (lambda () (ollama-buddy--send-with-command 'usage-examples)))

        ;; Changelog & Release Notes
        (changelog-entry
         :key ?g
         :description "Write changelog entry"
         :prompt "Write a changelog entry for these changes:"
         :system "You are a release manager who writes clear changelog entries. Follow Keep a Changelog format with sections for Added, Changed, Deprecated, Removed, Fixed, and Security. Be concise but specific. Include issue/PR references if mentioned. Focus on user impact, not implementation details."
         :action (lambda () (ollama-buddy--send-with-command 'changelog-entry)))

        (release-notes
         :key ?n
         :description "Write release notes"
         :prompt "Write user-friendly release notes for:"
         :system "You are a product communicator who writes engaging release notes. Highlight new features and improvements in user-focused language. Group related changes, call out breaking changes prominently, and include upgrade instructions if needed. Make users excited about the update."
         :action (lambda () (ollama-buddy--send-with-command 'release-notes)))

        ;; Support Documentation
        (troubleshooting
         :key ?t
         :description "Write troubleshooting guide"
         :prompt "Write a troubleshooting guide for common issues with:"
         :system "You are a support engineer who writes helpful troubleshooting docs. Structure as Problem/Cause/Solution for each issue. Include specific error messages users might see, diagnostic steps, and clear fix instructions. Cover the most common issues users encounter."
         :action (lambda () (ollama-buddy--send-with-command 'troubleshooting)))

        (faq
         :key ?f
         :description "Generate FAQ"
         :prompt "Generate a FAQ section for:"
         :system "You are a documentation specialist who anticipates user questions. Create a FAQ covering: getting started questions, common 'how do I...' questions, troubleshooting questions, and conceptual questions. Provide clear, direct answers. Organize from most to least common."
         :action (lambda () (ollama-buddy--send-with-command 'faq)))

        ;; Improvement Commands
        (improve-docs
         :key ?m
         :description "Improve existing docs"
         :prompt "Improve and clarify this documentation:"
         :system "You are a documentation editor who improves clarity and completeness. Fix unclear explanations, add missing information, improve structure and flow, correct any technical inaccuracies, and enhance readability. Maintain the original voice and format while making it better."
         :action (lambda () (ollama-buddy--send-with-command 'improve-docs)))

        (simplify-docs
         :key ?s
         :description "Simplify documentation"
         :prompt "Simplify this documentation while keeping it accurate:"
         :system "You are a plain language expert who makes technical docs accessible. Reduce jargon, shorten sentences, use active voice, and improve scannability with headers and lists. Maintain technical accuracy while making it easier to understand quickly."
         :action (lambda () (ollama-buddy--send-with-command 'simplify-docs)))

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
         :key ?Q
         :description "Quit"
         :action (lambda () (message "Quit Ollama Shell menu.")))
        )
      )
