;;; ollama-buddy-test.el --- Tests for ollama-buddy -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for ollama-buddy.el using ERT.

;;; Code:

;; Ensure we have necessary dependencies for testing
(require 'ert)
(require 'cl-lib)
(require 'color) ; Explicitly require the color library

;; We need savehist
(require 'savehist)

;; Now we can require the package
(require 'ollama-buddy)

;; Mock functions to avoid actual network calls
(defvar ollama-buddy-test--is-running t)
(defvar ollama-buddy-test--mock-responses (make-hash-table :test 'equal))

;; Mock response setup
(defun ollama-buddy-test--setup-mocks ()
  "Set up mock functions and test data."
  ;; Replace actual network functions with mock versions
  (advice-add 'ollama-buddy--ollama-running :override
              (lambda () ollama-buddy-test--is-running))
  
  (advice-add 'ollama-buddy--make-request :override
              (lambda (endpoint method &optional payload)
                (gethash endpoint ollama-buddy-test--mock-responses))))
  
(defun ollama-buddy-test--teardown-mocks ()
  "Remove mock function advice."
  (advice-remove 'ollama-buddy--ollama-running #'(lambda () ollama-buddy-test--is-running))
  (advice-remove 'ollama-buddy--make-request 
                #'(lambda (endpoint method &optional payload)
                    (gethash endpoint ollama-buddy-test--mock-responses))))

;; Test utils
(defmacro ollama-buddy-test--with-mock-buffer (&rest body)
  "Execute BODY in a temporary buffer with ollama-buddy-mode enabled."
  `(let ((temp-buffer (generate-new-buffer "*ollama-buddy-test*")))
     (unwind-protect
         (with-current-buffer temp-buffer
           (ollama-buddy-mode 1)
           (setq ollama-buddy--chat-buffer (buffer-name))
           ,@body)
       (kill-buffer temp-buffer))))

;; Test parameter functions
(ert-deftest ollama-buddy-test-params-get-for-request ()
  "Test that only modified parameters are included in API requests."
  (let ((ollama-buddy-params-defaults '((temperature . 0.8) (top_k . 20) (top_p . 0.9)))
        (ollama-buddy-params-active '((temperature . 1.0) (top_k . 20) (top_p . 0.9)))
        (ollama-buddy-params-modified '(temperature)))
    ;; Should only include explicitly modified parameters
    (let ((params (ollama-buddy-params-get-for-request)))
      (should (= (length params) 1))
      (should (equal (alist-get 'temperature params) 1.0))
      (should-not (assq 'top_k params)))))

;; Test conversation history
(ert-deftest ollama-buddy-test-add-to-history ()
  "Test adding messages to conversation history."
  (let ((ollama-buddy--conversation-history-by-model (make-hash-table :test 'equal))
        (ollama-buddy--current-model "test-model")
        (ollama-buddy-history-enabled t)
        (ollama-buddy-max-history-length 10))
    
    ;; Add a user message
    (ollama-buddy--add-to-history "user" "Hello")
    (let ((history (gethash "test-model" ollama-buddy--conversation-history-by-model)))
      (should (= (length history) 1))
      (should (equal (alist-get 'role (car history)) "user"))
      (should (equal (alist-get 'content (car history)) "Hello")))
    
    ;; Add an assistant message
    (ollama-buddy--add-to-history "assistant" "Hi there")
    (let ((history (gethash "test-model" ollama-buddy--conversation-history-by-model)))
      (should (= (length history) 2))
      (should (equal (alist-get 'role (car history)) "user"))
      (should (equal (alist-get 'role (cadr history)) "assistant")))))

;; Test Markdown to Org conversion
(ert-deftest ollama-buddy-test-md-to-org-convert-region ()
  "Test conversion from Markdown to Org format."
  (ollama-buddy-test--with-mock-buffer
   (insert "# Heading\n\n**Bold text**\n\n_Italic text_\n\n```python\ndef hello():\n    print('world')\n```\n")
   (ollama-buddy--md-to-org-convert-region (point-min) (point-max))
   (should (string-match-p "\\* Heading" (buffer-string)))
   (should (string-match-p "\\*Bold text\\*" (buffer-string)))
   (should (string-match-p "/Italic text/" (buffer-string)))
   (should (string-match-p "#\\+begin_src python" (buffer-string)))
   (should (string-match-p "#\\+end_src" (buffer-string)))))

;; Test status check logic
(ert-deftest ollama-buddy-test-check-status ()
  "Test status checking with cache."
  (ollama-buddy-test--setup-mocks)
  (unwind-protect
      (progn
        ;; First check should set the cache
        (setq ollama-buddy--last-status-check nil)
        (should (ollama-buddy--check-status))
        (should ollama-buddy--last-status-check)
        
        ;; Changing status while cache is valid shouldn't update
        (let ((old-timestamp ollama-buddy--last-status-check)
              (old-status ollama-buddy--status-cache))
          (setq ollama-buddy-test--is-running nil)
          (should (ollama-buddy--check-status))
          (should (eq ollama-buddy--status-cache old-status)))
        
        ;; Expire the cache and check again
        (setq ollama-buddy--last-status-check (- (float-time) 100))
        (should-not (ollama-buddy--check-status)))
    (ollama-buddy-test--teardown-mocks)))

;; Test command definitions
(ert-deftest ollama-buddy-test-command-definitions ()
  "Test command definition access functions."
  (let ((ollama-buddy-command-definitions
         '((test-cmd :key ?t :description "Test" :model "test-model" :prompt "test prompt" :action ignore))))
    (should (ollama-buddy--get-command-def 'test-cmd))
    (should-not (ollama-buddy--get-command-def 'nonexistent))
    (should (equal (ollama-buddy--get-command-prop 'test-cmd :key) ?t))
    (should (equal (ollama-buddy--get-command-prop 'test-cmd :description) "Test"))
    (should (equal (ollama-buddy--get-command-prop 'test-cmd :model) "test-model"))
    (should (equal (ollama-buddy--get-command-prop 'test-cmd :prompt) "test prompt"))))

;; Run the tests
(when noninteractive
  (ert-run-tests-batch-and-exit))

(provide 'ollama-buddy-test)
;;; ollama-buddy-test.el ends here
