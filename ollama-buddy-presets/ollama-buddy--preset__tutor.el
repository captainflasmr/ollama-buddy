;; ollama-buddy preset for role: tutor
;; Educational assistant for learning and understanding concepts

(require 'ollama-buddy)
(require 'info)

;; Menu display columns for this role
(setq ollama-buddy-menu-columns 3)

;; Helper function to collect all info nodes from a manual
(defun ollama-buddy-tutor--collect-info-nodes (manual)
  "Collect all node names from MANUAL (e.g., \"elisp\", \"emacs\")."
  (let ((nodes '())
        (visited (make-hash-table :test 'equal)))
    (condition-case nil
        (with-temp-buffer
          (Info-mode)
          (Info-goto-node (format "(%s)Top" manual))
          (let ((queue (list "Top")))
            (while queue
              (let ((node (pop queue)))
                (unless (gethash node visited)
                  (puthash node t visited)
                  (push (format "(%s)%s" manual node) nodes)
                  (condition-case nil
                      (progn
                        (Info-goto-node (format "(%s)%s" manual node))
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^\\* Menu:" nil t)
                            (while (re-search-forward "^\\* \\([^:\n]+\\)::" nil t)
                              (let ((item (match-string 1)))
                                (unless (gethash item visited)
                                  (push item queue)))))))
                    (error nil)))))))
      (error nil))
    (nreverse nodes)))

;; Cache for info nodes (manual . nodes-list)
(defvar ollama-buddy-tutor--info-cache (make-hash-table :test 'equal)
  "Cache of info nodes per manual.")

(defun ollama-buddy-tutor--get-info-nodes (manual)
  "Get info nodes for MANUAL, using cache if available."
  (or (gethash manual ollama-buddy-tutor--info-cache)
      (let ((nodes (ollama-buddy-tutor--collect-info-nodes manual)))
        (puthash manual nodes ollama-buddy-tutor--info-cache)
        nodes)))

(defun ollama-buddy-tutor--read-info-node ()
  "Prompt for an info manual, then complete on its nodes."
  (let* ((manual (completing-read "Info manual: "
                                  '("elisp" "emacs" "org" "info" "eintr"
                                    "cl" "eieio" "eshell" "gnus" "message"
                                    "tramp" "dired" "ediff" "calc" "woman")
                                  nil nil nil nil "elisp"))
         (nodes (progn
                  (message "Building node list for %s..." manual)
                  (ollama-buddy-tutor--get-info-nodes manual))))
    (if nodes
        (completing-read (format "Info node (%s): " manual) nodes nil t)
      (read-string "Info node (e.g., (elisp)Hooks): "))))

(setq ollama-buddy-command-definitions
      '(
        ;; General Commands
        (open-chat
         :key ?o
         :description "Open chat buffer"
         :group "General"
         :action ollama-buddy--open-chat)

        (send-region
         :key ?l
         :description "Send region"
         :group "General"
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
         :group "General"
         :action ollama-buddy-roles-switch-role)

        ;; Info Documentation Commands
        (fetch-info-node
         :key ?I
         :description "Fetch info + subnodes"
         :group "Info Docs"
         :action (lambda ()
                   (let* ((node (ollama-buddy-tutor--read-info-node))
                          (content
                           (condition-case err
                               (with-temp-buffer
                                 (Info-mode)
                                 (Info-goto-node node)
                                 (let ((result (buffer-substring-no-properties (point-min) (point-max)))
                                       (menu-items '())
                                       (file (Info-extract-pointer "File")))
                                   ;; Collect menu items if any
                                   (save-excursion
                                     (goto-char (point-min))
                                     (when (re-search-forward "^\\* Menu:" nil t)
                                       (while (re-search-forward "^\\* \\([^:\n]+\\)::" nil t)
                                         (push (match-string 1) menu-items))))
                                   ;; Fetch each submenu node
                                   (dolist (item (nreverse menu-items))
                                     (condition-case nil
                                         (progn
                                           (Info-goto-node (format "(%s)%s" file item))
                                           (setq result (concat result "\n\n" (make-string 70 ?=) "\n"
                                                                "Subnode: " item "\n"
                                                                (make-string 70 ?=) "\n\n"
                                                                (buffer-substring-no-properties (point-min) (point-max)))))
                                       (error nil)))
                                   result))
                             (error (format "Error fetching info node: %s" (error-message-string err))))))
                     (insert (format "\n%s\n=== Info: %s ===\n\n%s\n%s\n"
                                     (make-string 70 ?=) node content (make-string 70 ?=))))))

        (fetch-info-node-simple
         :key ?b
         :description "Fetch info node only"
         :group "Info Docs"
         :action (lambda ()
                   (let* ((node (ollama-buddy-tutor--read-info-node))
                          (content
                           (condition-case err
                               (with-temp-buffer
                                 (Info-mode)
                                 (Info-goto-node node)
                                 (buffer-substring-no-properties (point-min) (point-max)))
                             (error (format "Error: %s" (error-message-string err))))))
                     (insert (format "\n=== Info: %s ===\n\n%s\n" node content)))))

        (clear-info-cache
         :key ?C
         :description "Clear info node cache"
         :group "Info Docs"
         :action (lambda ()
                   (clrhash ollama-buddy-tutor--info-cache)
                   (message "Info node cache cleared")))

        ;; Explanation Commands
        (explain-simply
         :key ?s
         :description "Explain simply"
         :group "Explanation"
         :prompt "Explain this concept in simple terms for a beginner:"
         :system "You are a patient teacher who explains complex topics in simple, accessible language. Use everyday analogies, avoid jargon, and build understanding step by step. Always check if the explanation would make sense to someone new to the topic."
         :action (lambda () (ollama-buddy--send-with-command 'explain-simply))
         :destination chat)

        (explain-deeply
         :key ?d
         :description "Explain in depth"
         :group "Explanation"
         :prompt "Provide a detailed, technical explanation of:"
         :system "You are an expert educator who provides comprehensive, in-depth explanations. Cover the underlying principles, edge cases, and connections to related concepts. Assume the learner wants to deeply understand the topic, not just get a surface-level overview."
         :action (lambda () (ollama-buddy--send-with-command 'explain-deeply))
         :destination chat)

        (eli5
         :key ?5
         :description "Explain like I'm 5"
         :group "Explanation"
         :prompt "Explain this like I'm 5 years old:"
         :system "You are a friendly teacher explaining things to a young child. Use very simple words, fun comparisons to things kids know (toys, games, animals, food), and keep explanations short and engaging. Make it fun and easy to understand."
         :action (lambda () (ollama-buddy--send-with-command 'eli5))
         :destination chat)

        (analogy
         :key ?a
         :description "Create analogy"
         :group "Explanation"
         :prompt "Create a helpful analogy to explain:"
         :system "You are creative at making abstract concepts concrete through analogies. Create vivid, memorable analogies that map the key aspects of the concept to familiar everyday experiences. Explain how each part of the analogy corresponds to the original concept."
         :action (lambda () (ollama-buddy--send-with-command 'analogy))
         :destination chat)

        (step-by-step
         :key ?t
         :description "Break into steps"
         :group "Explanation"
         :prompt "Break this down into clear, sequential steps:"
         :system "You are a methodical instructor who excels at breaking complex processes into manageable steps. Number each step clearly, explain what happens and why, and note any decision points or variations. Make sure someone could follow along without prior knowledge."
         :action (lambda () (ollama-buddy--send-with-command 'step-by-step))
         :destination chat)

        ;; Assessment Commands
        (quiz-me
         :key ?z
         :description "Generate quiz"
         :group "Assessment"
         :prompt "Generate quiz questions to test understanding of:"
         :system "You are an educational assessment expert. Create a mix of question types (multiple choice, short answer, true/false) that test both recall and deeper understanding. Include answers at the end. Progress from basic to more challenging questions."
         :action (lambda () (ollama-buddy--send-with-command 'quiz-me))
         :destination chat)

        (check-understanding
         :key ?c
         :description "Check my understanding"
         :group "Assessment"
         :prompt "I think I understand this concept. Please check my understanding and correct any misconceptions:"
         :system "You are a supportive tutor who validates understanding. Acknowledge what the learner got right, gently correct any misconceptions with clear explanations, and fill in any gaps. Be encouraging while ensuring accuracy."
         :action (lambda () (ollama-buddy--send-with-command 'check-understanding))
         :destination chat)

        (practice-problems
         :key ?p
         :description "Practice problems"
         :group "Assessment"
         :prompt "Generate practice problems for:"
         :system "You are an educator who creates effective practice exercises. Generate problems that range from basic to challenging, include worked examples for the first one or two, and provide answers with explanations. Focus on building practical skills."
         :action (lambda () (ollama-buddy--send-with-command 'practice-problems))
         :destination chat)

        ;; Context Commands
        (prerequisites
         :key ?r
         :description "What should I know first?"
         :group "Context"
         :prompt "What prerequisite knowledge do I need to understand:"
         :system "You are a curriculum designer who understands learning progressions. Identify the foundational concepts needed before tackling this topic, briefly explain why each is important, and suggest a logical learning order. Keep it practical and focused."
         :action (lambda () (ollama-buddy--send-with-command 'prerequisites))
         :destination chat)

        (related-topics
         :key ?x
         :description "Related topics"
         :group "Context"
         :prompt "What topics are related to this and how do they connect:"
         :system "You are a knowledge mapper who sees connections between concepts. Identify related topics, explain how they connect to the main concept, and suggest which might be worth exploring next. Help build a mental map of the subject area."
         :action (lambda () (ollama-buddy--send-with-command 'related-topics))
         :destination chat)

        (real-world
         :key ?w
         :description "Real-world examples"
         :group "Context"
         :prompt "Give real-world examples and applications of:"
         :system "You are practical educator who connects theory to practice. Provide concrete, real-world examples showing how this concept is used in actual applications. Include examples from different domains if applicable. Make the relevance clear."
         :action (lambda () (ollama-buddy--send-with-command 'real-world))
         :destination chat)

        (common-mistakes
         :key ?m
         :description "Common mistakes"
         :group "Context"
         :prompt "What are common mistakes or misconceptions about:"
         :system "You are an experienced teacher who has seen students struggle. Identify the most common mistakes and misconceptions, explain why they happen, and provide clear corrections. Help learners avoid these pitfalls."
         :action (lambda () (ollama-buddy--send-with-command 'common-mistakes))
         :destination chat)

        ;; System Commands
        (custom-prompt
         :key ?e
         :description "Custom prompt"
         :group "System"
         :action ollama-buddy--menu-custom-prompt)

        (minibuffer-prompt
         :key ?i
         :description "Minibuffer Prompt"
         :group "System"
         :action ollama-buddy--menu-minibuffer-prompt)

        (quit
         :key ?q
         :description "Quit"
         :group "System"
         :action (lambda () (message "Quit Ollama Shell menu.")))
        )
      )
