;;; ollama-buddy-plan.el --- Plan mode for Ollama Buddy -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This module provides a lightweight "plan mode" for Ollama Buddy.
;; It instructs the LLM to produce structured plans as org-mode TODO
;; items, then lets the user manage step execution through org's
;; native TODO state cycling.
;;
;; Usage:
;;
;;   (require 'ollama-buddy-plan)
;;
;;   ;; From the chat buffer, use the /plan slash command:
;;   ;; Type: /plan
;;   ;; Then describe your task in the prompt area and send.
;;
;;   ;; Keybindings in plan blocks:
;;   ;;   S-<right> / S-<left>  - Cycle TODO state (org default)
;;   ;;   C-c C-t              - Set TODO state via completion
;;   ;;   /plan-next           - Execute the next TODO step
;;   ;;   /plan-all            - Execute all remaining TODO steps
;;   ;;   /plan-status         - Show plan progress summary
;;   ;;   /plan-stop           - Deactivate plan mode
;;
;; The plan system prompt instructs the model to output steps as:
;;
;;   ** TODO Step 1: Description
;;   Details of what this step involves.
;;
;;   ** TODO Step 2: Description
;;   ...
;;
;; Users can then cycle states (TODO -> DOING -> DONE | SKIP)
;; and ask the LLM to execute steps individually or in sequence.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ollama-buddy-core)

;; Forward declarations
(declare-function ollama-buddy--open-chat "ollama-buddy")
(declare-function ollama-buddy--send "ollama-buddy")
(declare-function ollama-buddy--send-backend "ollama-buddy-core")
(declare-function ollama-buddy--prepare-prompt-area "ollama-buddy-core")
(declare-function ollama-buddy--update-status "ollama-buddy-core")
(declare-function ollama-buddy--get-prompt-content "ollama-buddy")
(declare-function ollama-buddy--set-system-prompt-with-metadata "ollama-buddy-core")
(declare-function ollama-buddy-reset-system-prompt "ollama-buddy")

(defvar ollama-buddy--chat-buffer)
(defvar ollama-buddy--current-model)

;;; Customization

(defgroup ollama-buddy-plan nil
  "Plan mode configuration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-plan-")

(defcustom ollama-buddy-plan-todo-states
  '("TODO" "DOING" "DONE" "SKIP")
  "TODO states available for plan steps.
These are configured as org TODO keywords when plan mode is active."
  :type '(repeat string)
  :group 'ollama-buddy-plan)

(defcustom ollama-buddy-plan-auto-advance t
  "When non-nil, automatically advance to the next TODO step after DONE."
  :type 'boolean
  :group 'ollama-buddy-plan)

;;; State

(defvar-local ollama-buddy-plan--active nil
  "Non-nil when plan mode is active in this buffer.")

(defvar-local ollama-buddy-plan--saved-system-prompt nil
  "System prompt saved before plan mode activation.")

(defvar-local ollama-buddy-plan--saved-todo-keywords nil
  "Original `org-todo-keywords' saved before plan mode activation.")

;;; System prompt

(defconst ollama-buddy-plan--planning-prompt
  "You are a planning assistant. When the user describes a task, produce a structured plan as org-mode TODO headings.

RULES:
- Each step: ** TODO Step N: <title>
- Below each step: 1-3 lines of description (plain text)
- Use 3-8 steps depending on complexity
- End with: ** Notes (optional observations)
- Do NOT use checkboxes or numbered lists — ONLY ** TODO headings
- If you use <think> blocks, keep ALL TODO headings out of them

EXAMPLE:
** TODO Step 1: Create user model
Define the user schema with email, password hash, and timestamps.
** TODO Step 2: Add registration endpoint
Implement POST /register with input validation and password hashing.
** Notes
Consider rate-limiting login attempts."
  "System prompt for plan generation phase.")

(defconst ollama-buddy-plan--executing-prompt
  "You are executing a step from a plan. Use the tools available to you to carry out the work. After completing the step, write a brief summary of what you did."
  "System prompt for step execution phase.
Kept minimal so it does not interfere with tool calling.")

;;; Core functions

(defun ollama-buddy-plan--configure-todo-states ()
  "Configure org TODO states for plan mode in the chat buffer."
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    ;; Save original keywords
    (setq ollama-buddy-plan--saved-todo-keywords org-todo-keywords)
    ;; Set plan-specific states
    (setq-local org-todo-keywords
                '((sequence "TODO" "DOING" "|" "DONE" "SKIP")))
    ;; Refresh org's internal state
    (org-set-regexps-and-options)
    ;; Set faces for visual feedback
    (setq-local org-todo-keyword-faces
                '(("TODO" . org-todo)
                  ("DOING" . (:foreground "orange" :weight bold))
                  ("DONE" . org-done)
                  ("SKIP" . (:foreground "gray" :slant italic))))))

(defun ollama-buddy-plan--restore-todo-states ()
  "Restore original org TODO states."
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (when ollama-buddy-plan--saved-todo-keywords
      (setq-local org-todo-keywords ollama-buddy-plan--saved-todo-keywords)
      (org-set-regexps-and-options)
      (setq ollama-buddy-plan--saved-todo-keywords nil))))

(defun ollama-buddy-plan--collect-steps ()
  "Collect all plan steps from the chat buffer.
Returns a list of plists with :point, :state, :title, and :body.
Handles steps at any org heading level, steps without heading stars,
and orphaned Step 1 content split across Think/Response boundaries."
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (let ((step-re (format "^\\(\\*\\*+\\s-+\\)?\\(%s\\) \\(Step [0-9]+:.*\\)$"
                           (regexp-opt ollama-buddy-plan-todo-states)))
          (next-step-re (format "^\\(\\*\\*+\\s-+\\)?\\(%s\\) Step [0-9]+:"
                                (regexp-opt ollama-buddy-plan-todo-states)))
          (stop-re "^\\(\\*\\*+\\s-+\\)?Notes\\b\\|^\\* ")
          steps)
      (save-excursion
        ;; Collect well-formed steps
        (goto-char (point-min))
        (while (re-search-forward step-re nil t)
          (let* ((state (match-string-no-properties 2))
                 (title (match-string-no-properties 3))
                 (heading-point (line-beginning-position))
                 (body-start (1+ (line-end-position)))
                 (body-end (save-excursion
                             (cond
                              ((re-search-forward next-step-re nil t)
                               (line-beginning-position))
                              ((re-search-forward stop-re nil t)
                               (line-beginning-position))
                              (t (point-max)))))
                 (body (string-trim
                        (buffer-substring-no-properties body-start body-end))))
            (push (list :point heading-point
                        :state state
                        :title title
                        :body body)
                  steps)))
        ;; Check for orphaned Step 1 at start of Response section.
        ;; Some thinking models split Step 1 across the Think/Response
        ;; boundary, leaving just " N: <title>" after "*** Response".
        (unless (cl-find "Step 1:" steps
                         :key (lambda (s) (plist-get s :title))
                         :test #'string-prefix-p)
          (goto-char (point-min))
          (when (re-search-forward "^\\*\\*\\* Response\n+" nil t)
            (when (looking-at "\\s-*\\([0-9]+:\\s-*\\(.*\\)\\)$")
              (let* ((title (concat "Step " (string-trim (match-string-no-properties 1))))
                     (heading-point (line-beginning-position))
                     (body-start (1+ (line-end-position)))
                     (body-end (save-excursion
                                 (cond
                                  ((re-search-forward next-step-re nil t)
                                   (line-beginning-position))
                                  ((re-search-forward stop-re nil t)
                                   (line-beginning-position))
                                  (t (point-max)))))
                     (body (string-trim
                            (buffer-substring-no-properties body-start body-end))))
                (push (list :point heading-point
                            :state "TODO"
                            :title title
                            :body body)
                      steps))))))
      (sort steps (lambda (a b)
                    (< (plist-get a :point) (plist-get b :point)))))))

(defun ollama-buddy-plan--next-todo-step ()
  "Return the first step that is still TODO, or nil."
  (cl-find "TODO" (ollama-buddy-plan--collect-steps)
           :key (lambda (s) (plist-get s :state))
           :test #'string=))

(defun ollama-buddy-plan--progress-summary ()
  "Return a string summarizing plan progress."
  (let* ((steps (ollama-buddy-plan--collect-steps))
         (total (length steps))
         (done (cl-count "DONE" steps
                         :key (lambda (s) (plist-get s :state))
                         :test #'string=))
         (skipped (cl-count "SKIP" steps
                            :key (lambda (s) (plist-get s :state))
                            :test #'string=))
         (in-progress (cl-count "DOING" steps
                                :key (lambda (s) (plist-get s :state))
                                :test #'string=))
         (remaining (cl-count "TODO" steps
                              :key (lambda (s) (plist-get s :state))
                              :test #'string=)))
    (if (zerop total)
        "No plan steps found."
      (format "Plan: %d/%d done, %d doing, %d remaining, %d skipped"
              done total in-progress remaining skipped))))

(defun ollama-buddy-plan--set-step-state (step new-state)
  "Set STEP to NEW-STATE in the buffer.
STEP is a plist as returned by `ollama-buddy-plan--collect-steps'."
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (plist-get step :point))
        (when (looking-at
               (format "^\\(\\*\\*+\\s-+\\)?\\(%s\\)\\( .*\\)$"
                       (regexp-opt ollama-buddy-plan-todo-states)))
          (replace-match (concat (match-string 1) new-state (match-string 3))))))))

(defun ollama-buddy-plan--insert-step-indicator (step)
  "Insert a visual indicator for the current plan STEP being executed.
Inserted just before the prompt area so the user can see which step is active."
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (let ((inhibit-read-only t)
          (progress (ollama-buddy-plan--progress-summary)))
      (save-excursion
        (goto-char (point-max))
        (when (re-search-backward ">> PROMPT:" nil t)
          (beginning-of-line)
          (insert (format "\n** ⚒ DOING: %s\n%s\n"
                          (plist-get step :title)
                          progress)))))))

;;; Interactive commands

;;;###autoload
(defun ollama-buddy-plan-start ()
  "Activate plan mode.
Sets a planning system prompt and configures org TODO states.
The user should then describe their task in the prompt area and send."
  (interactive)
  (ollama-buddy--open-chat)
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    ;; Save current system prompt
    (setq ollama-buddy-plan--saved-system-prompt
          ollama-buddy--current-system-prompt)
    ;; Activate
    (setq ollama-buddy-plan--active t)
    ;; Set planning system prompt
    (ollama-buddy--set-system-prompt-with-metadata
     ollama-buddy-plan--planning-prompt "Plan Mode" "plan")
    ;; Configure TODO states
    (ollama-buddy-plan--configure-todo-states)
    ;; Update prompt area
    (ollama-buddy--prepare-prompt-area t)
    (ollama-buddy--update-status "Plan mode active — describe your task")))

;;;###autoload
(defun ollama-buddy-plan-stop ()
  "Deactivate plan mode and restore previous system prompt."
  (interactive)
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (setq ollama-buddy-plan--active nil)
    ;; Restore system prompt
    (if ollama-buddy-plan--saved-system-prompt
        (progn
          (setq ollama-buddy--current-system-prompt
                ollama-buddy-plan--saved-system-prompt)
          (setq ollama-buddy-plan--saved-system-prompt nil))
      (ollama-buddy-reset-system-prompt))
    ;; Restore TODO states
    (ollama-buddy-plan--restore-todo-states)
    (ollama-buddy--update-status "Plan mode deactivated")))

;;;###autoload
(defun ollama-buddy-plan-execute-next ()
  "Execute the next TODO step in the current plan.
Marks it DOING and sends it to the LLM for execution."
  (interactive)
  (unless ollama-buddy-plan--active
    (user-error "Plan mode is not active.  Use /plan to start"))
  (let ((step (ollama-buddy-plan--next-todo-step)))
    (if (not step)
        (progn
          (ollama-buddy--update-status "Plan complete — no remaining TODO steps")
          (message "%s" (ollama-buddy-plan--progress-summary)))
      ;; Mark as doing
      (ollama-buddy-plan--set-step-state step "DOING")
      (ollama-buddy--update-status
       (format "⚒ Plan: %s" (plist-get step :title)))
      ;; Insert plan step indicator in the buffer
      (ollama-buddy-plan--insert-step-indicator step)
      ;; Switch to minimal execution prompt so tool calling works cleanly
      (ollama-buddy--set-system-prompt-with-metadata
       ollama-buddy-plan--executing-prompt "Plan Execute" "plan")
      ;; Send execution request to LLM
      (let ((prompt (format "Execute this plan step now:\n\n%s\n%s\n\nCarry out the work described. When done, summarize what you did."
                            (plist-get step :title)
                            (plist-get step :body))))
        (ollama-buddy--open-chat)
        (ollama-buddy--send-backend prompt)))))

;;;###autoload
(defun ollama-buddy-plan-execute-all ()
  "Execute all remaining TODO steps sequentially.
After each step completes, the user can review before the next begins."
  (interactive)
  (unless ollama-buddy-plan--active
    (user-error "Plan mode is not active.  Use /plan to start"))
  (let ((step (ollama-buddy-plan--next-todo-step)))
    (if (not step)
        (message "%s" (ollama-buddy-plan--progress-summary))
      ;; Mark current step
      (ollama-buddy-plan--set-step-state step "DOING")
      (ollama-buddy--update-status
       (format "⚒ Plan: %s" (plist-get step :title)))
      ;; Insert plan step indicator in the buffer
      (ollama-buddy-plan--insert-step-indicator step)
      ;; Switch to minimal execution prompt so tool calling works cleanly
      (ollama-buddy--set-system-prompt-with-metadata
       ollama-buddy-plan--executing-prompt "Plan Execute" "plan")
      ;; Send with instruction to continue
      (let ((prompt (format "Execute this plan step:\n\n%s\n%s\n\nAfter completing this step, mark it done and immediately proceed to the next TODO step in the plan. Continue until all steps are complete. For each step, summarize what you did."
                            (plist-get step :title)
                            (plist-get step :body))))
        (ollama-buddy--open-chat)
        (ollama-buddy--send-backend prompt)))))

;;;###autoload
(defun ollama-buddy-plan-mark-done ()
  "Mark the current DOING step as DONE.
If `ollama-buddy-plan-auto-advance' is non-nil, report progress."
  (interactive)
  (unless ollama-buddy-plan--active
    (user-error "Plan mode is not active"))
  (let* ((steps (ollama-buddy-plan--collect-steps))
         (in-progress (cl-find "DOING" steps
                               :key (lambda (s) (plist-get s :state))
                               :test #'string=)))
    (if (not in-progress)
        (message "No DOING step to mark as DONE")
      (ollama-buddy-plan--set-step-state in-progress "DONE")
      ;; Restore planning prompt for next interaction
      (ollama-buddy--set-system-prompt-with-metadata
       ollama-buddy-plan--planning-prompt "Plan Mode" "plan")
      (message "%s" (ollama-buddy-plan--progress-summary)))))

;;;###autoload
(defun ollama-buddy-plan-status ()
  "Display the current plan progress."
  (interactive)
  (message "%s" (ollama-buddy-plan--progress-summary)))

;;;###autoload
(defun ollama-buddy-plan-goto-next ()
  "Jump to the next TODO step in the buffer."
  (interactive)
  (let ((step (ollama-buddy-plan--next-todo-step)))
    (if step
        (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
          (goto-char (plist-get step :point))
          (org-fold-show-entry)
          (message "Next: %s" (plist-get step :title)))
      (message "No remaining TODO steps"))))

(provide 'ollama-buddy-plan)

;;; ollama-buddy-plan.el ends here
