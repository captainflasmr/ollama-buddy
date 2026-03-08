;;; ollama-buddy-backend-test.el --- Tests for backend parity -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Dyer

;; Author: James Dyer <captainflasmr@gmail.com>

;;; Commentary:

;; ERT tests verifying that the network-process and curl backends
;; behave consistently.  All tests run offline (no live Ollama server).
;; Run with: make test-backend

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'ollama-buddy-core)
(require 'ollama-buddy)
(require 'ollama-buddy-curl nil t)

;; Forward declarations for byte-compiler
(defvar ollama-buddy--chat-buffer)
(defvar ollama-buddy--active-process)
(defvar ollama-buddy--request-cancelled)
(defvar ollama-buddy--current-model)
(defvar ollama-buddy--current-token-count)
(defvar ollama-buddy--current-token-start-time)
(defvar ollama-buddy--last-token-count)
(defvar ollama-buddy--last-update-time)
(defvar ollama-buddy--token-update-timer)
(defvar ollama-buddy--stream-pending)
(defvar ollama-buddy-streaming-enabled)
(defvar ollama-buddy-show-context-percentage)
(defvar ollama-buddy--skip-inline-processing)

;;; Backend Dispatcher Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-send-backend-routes-to-network-process ()
  "Test that send-backend routes to network-process when configured."
  :tags '(backend)
  (let ((send-called nil)
        (curl-called nil)
        (ollama-buddy-communication-backend 'network-process)
        (ollama-buddy-airplane-mode nil)
        (ollama-buddy--current-model "llama3.2:1b"))
    (cl-letf (((symbol-function 'ollama-buddy--send)
               (lambda (&rest _) (setq send-called t)))
              ((symbol-function 'ollama-buddy-curl--send)
               (lambda (&rest _) (setq curl-called t)))
              ((symbol-function 'ollama-buddy--get-effective-backend)
               (lambda () 'network-process)))
      (ollama-buddy--send-backend "test prompt")
      (should send-called)
      (should-not curl-called))))

(ert-deftest ollama-buddy-test-send-backend-routes-to-curl ()
  "Test that send-backend routes to curl when configured."
  :tags '(backend)
  (let ((send-called nil)
        (curl-called nil)
        (ollama-buddy-communication-backend 'curl)
        (ollama-buddy-airplane-mode nil)
        (ollama-buddy--current-model "llama3.2:1b"))
    (cl-letf (((symbol-function 'ollama-buddy--send)
               (lambda (&rest _) (setq send-called t)))
              ((symbol-function 'ollama-buddy-curl--send)
               (lambda (&rest _) (setq curl-called t)))
              ((symbol-function 'ollama-buddy--get-effective-backend)
               (lambda () 'curl)))
      (ollama-buddy--send-backend "test prompt")
      (should-not send-called)
      (should curl-called))))

(ert-deftest ollama-buddy-test-send-backend-passes-all-args ()
  "Test that all three arguments are forwarded to the backend."
  :tags '(backend)
  (let ((received-args nil)
        (ollama-buddy-communication-backend 'network-process)
        (ollama-buddy-airplane-mode nil)
        (ollama-buddy--current-model "llama3.2:1b"))
    (cl-letf (((symbol-function 'ollama-buddy--send)
               (lambda (&rest args) (setq received-args args)))
              ((symbol-function 'ollama-buddy--get-effective-backend)
               (lambda () 'network-process)))
      (ollama-buddy--send-backend "hello" "custom-model" t)
      (should (equal received-args '("hello" "custom-model" t))))))

(ert-deftest ollama-buddy-test-send-backend-curl-passes-all-args ()
  "Test that all three arguments are forwarded to the curl backend."
  :tags '(backend)
  (let ((received-args nil)
        (ollama-buddy-communication-backend 'curl)
        (ollama-buddy-airplane-mode nil)
        (ollama-buddy--current-model "llama3.2:1b"))
    (cl-letf (((symbol-function 'ollama-buddy-curl--send)
               (lambda (&rest args) (setq received-args args)))
              ((symbol-function 'ollama-buddy--get-effective-backend)
               (lambda () 'curl)))
      (ollama-buddy--send-backend "hello" "custom-model" t)
      (should (equal received-args '("hello" "custom-model" t))))))

(ert-deftest ollama-buddy-test-send-backend-airplane-blocks-cloud ()
  "Test that airplane mode blocks internet-requiring models."
  :tags '(backend)
  (let ((send-called nil)
        (ollama-buddy-airplane-mode t)
        (ollama-buddy--current-model "u:qwen3-coder:480b-cloud"))
    (cl-letf (((symbol-function 'ollama-buddy--send)
               (lambda (&rest _) (setq send-called t)))
              ((symbol-function 'ollama-buddy--internet-model-p)
               (lambda (_) t))
              ((symbol-function 'ollama-buddy--get-effective-backend)
               (lambda () 'network-process)))
      (ollama-buddy--send-backend "test" "u:qwen3-coder:480b-cloud")
      (should-not send-called))))

(ert-deftest ollama-buddy-test-send-backend-airplane-allows-local ()
  "Test that airplane mode allows local models through."
  :tags '(backend)
  (let ((send-called nil)
        (ollama-buddy-airplane-mode t)
        (ollama-buddy--current-model "llama3.2:1b"))
    (cl-letf (((symbol-function 'ollama-buddy--send)
               (lambda (&rest _) (setq send-called t)))
              ((symbol-function 'ollama-buddy--internet-model-p)
               (lambda (_) nil))
              ((symbol-function 'ollama-buddy--get-effective-backend)
               (lambda () 'network-process)))
      (ollama-buddy--send-backend "test" "llama3.2:1b")
      (should send-called))))

;;; Effective Backend Fallback Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-effective-backend-default-network-process ()
  "Test that network-process is returned when configured."
  :tags '(backend)
  (let ((ollama-buddy-communication-backend 'network-process))
    (should (eq 'network-process (ollama-buddy--get-effective-backend)))))

(ert-deftest ollama-buddy-test-effective-backend-curl-not-loaded ()
  "Test fallback when ollama-buddy-curl feature is not loaded."
  :tags '(backend)
  (let ((ollama-buddy-communication-backend 'curl))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &rest _)
                 (not (eq feature 'ollama-buddy-curl)))))
      (should (eq 'network-process (ollama-buddy--get-effective-backend)))
      (should (eq 'network-process ollama-buddy-communication-backend)))))

(ert-deftest ollama-buddy-test-effective-backend-curl-no-executable ()
  "Test fallback when curl executable is not found."
  :tags '(backend)
  (let ((ollama-buddy-communication-backend 'curl))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &rest _)
                 (or (eq feature 'ollama-buddy-curl)
                     (funcall (symbol-function 'featurep) feature))))
              ((symbol-function 'ollama-buddy-curl--validate-executable)
               (lambda () nil)))
      (should (eq 'network-process (ollama-buddy--get-effective-backend)))
      (should (eq 'network-process ollama-buddy-communication-backend)))))

(ert-deftest ollama-buddy-test-effective-backend-curl-valid ()
  "Test that curl is returned when properly configured."
  :tags '(backend)
  (let ((ollama-buddy-communication-backend 'curl))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &rest _)
                 (or (eq feature 'ollama-buddy-curl)
                     (funcall (symbol-function 'featurep) feature))))
              ((symbol-function 'ollama-buddy-curl--validate-executable)
               (lambda () t)))
      (should (eq 'curl (ollama-buddy--get-effective-backend))))))

;;; Validate Send Request Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-validate-send-offline-errors ()
  "Test that offline status signals an error."
  :tags '(backend)
  (cl-letf (((symbol-function 'ollama-buddy--check-status) (lambda () nil))
            ((symbol-function 'ollama-buddy--update-status) (lambda (&rest _) nil)))
    (should-error (ollama-buddy--validate-send-request "hello" nil)
                  :type 'user-error)))

(ert-deftest ollama-buddy-test-validate-send-empty-prompt-errors ()
  "Test that empty prompt without tool-continuation signals an error."
  :tags '(backend)
  (cl-letf (((symbol-function 'ollama-buddy--check-status) (lambda () t)))
    (let ((ollama-buddy-show-context-percentage nil))
      (should-error (ollama-buddy--validate-send-request "" nil)
                    :type 'user-error))))

(ert-deftest ollama-buddy-test-validate-send-empty-prompt-ok-for-tool ()
  "Test that empty prompt is allowed for tool continuations."
  :tags '(backend)
  (cl-letf (((symbol-function 'ollama-buddy--check-status) (lambda () t)))
    (let ((ollama-buddy-show-context-percentage nil))
      (ollama-buddy--validate-send-request "" t))))

(ert-deftest ollama-buddy-test-validate-send-valid-prompt-passes ()
  "Test that a valid prompt passes validation."
  :tags '(backend)
  (cl-letf (((symbol-function 'ollama-buddy--check-status) (lambda () t)))
    (let ((ollama-buddy-show-context-percentage nil))
      (ollama-buddy--validate-send-request "hello" nil))))

;;; Process Inline Prompt Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-process-inline-skip-flag ()
  "Test that skip flag prevents inline processing."
  :tags '(backend)
  (let ((ollama-buddy--skip-inline-processing t)
        (rag-called nil))
    (cl-letf (((symbol-function 'ollama-buddy-rag-process-inline)
               (lambda (p) (setq rag-called t) p)))
      (should (equal "test @file(foo)"
                     (ollama-buddy--process-inline-prompt "test @file(foo)")))
      (should-not rag-called))))

(ert-deftest ollama-buddy-test-process-inline-calls-processors ()
  "Test that inline processing calls all processors in sequence."
  :tags '(backend)
  (let ((ollama-buddy--skip-inline-processing nil)
        (call-order nil))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f &rest _)
                 (if (eq f 'ollama-buddy-web-search) nil
                   (funcall (symbol-function 'featurep) f))))
              ((symbol-function 'ollama-buddy-rag-process-inline)
               (lambda (p) (push 'rag call-order) (concat p "+rag")))
              ((symbol-function 'ollama-buddy--file-process-inline)
               (lambda (p) (push 'file call-order) (concat p "+file")))
              ((symbol-function 'ollama-buddy--skills-process-inline)
               (lambda (p) (push 'skills call-order) (concat p "+skills"))))
      (let ((result (ollama-buddy--process-inline-prompt "start")))
        (should (equal "start+rag+file+skills" result))
        (should (equal '(skills file rag) call-order))))))

;;; Curl JSON Line Delegation Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-curl-json-line-delegates ()
  "Test that curl process-json-line delegates to stream-process-json."
  :tags '(backend)
  (when (featurep 'ollama-buddy-curl)
    (let ((received-data nil))
      (cl-letf (((symbol-function 'ollama-buddy--stream-process-json)
                 (lambda (data) (setq received-data data))))
        (ollama-buddy-curl--process-json-line
         "{\"message\":{\"content\":\"hi\"},\"done\":false}")
        (should received-data)
        (should (equal "hi" (alist-get 'content
                                        (alist-get 'message received-data))))))))

(ert-deftest ollama-buddy-test-curl-json-line-ignores-empty ()
  "Test that empty/invalid lines are ignored by curl processor."
  :tags '(backend)
  (when (featurep 'ollama-buddy-curl)
    (let ((called nil))
      (cl-letf (((symbol-function 'ollama-buddy--stream-process-json)
                 (lambda (_) (setq called t))))
        (ollama-buddy-curl--process-json-line "")
        (should-not called)
        (ollama-buddy-curl--process-json-line "   ")
        (should-not called)
        (ollama-buddy-curl--process-json-line nil)
        (should-not called)
        (ollama-buddy-curl--process-json-line "not json")
        (should-not called)))))

;;; Cancellation Flag Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-cancel-sets-flag ()
  "Test that cancel-request sets the cancelled flag."
  :tags '(backend)
  (let ((ollama-buddy--request-cancelled nil)
        (ollama-buddy--active-process 'mock-process)
        (ollama-buddy--token-update-timer nil)
        (ollama-buddy--current-token-count 0)
        (ollama-buddy--current-token-start-time nil)
        (ollama-buddy--last-token-count 0)
        (ollama-buddy--last-update-time nil)
        (ollama-buddy--stream-pending "")
        (ollama-buddy--multishot-prompt nil)
        (ollama-buddy--multishot-sequence nil)
        (ollama-buddy--multishot-progress 0))
    (cl-letf (((symbol-function 'delete-process) (lambda (_) nil))
              ((symbol-function 'ollama-buddy--cancel-response-wait-timer) (lambda () nil))
              ((symbol-function 'ollama-buddy--multishot-cancel-timer) (lambda () nil))
              ((symbol-function 'ollama-buddy--update-status) (lambda (&rest _) nil)))
      (ollama-buddy--cancel-request)
      (should ollama-buddy--request-cancelled)
      (should-not ollama-buddy--active-process))))

(ert-deftest ollama-buddy-test-setup-clears-cancelled-flag ()
  "Test that setup-chat-send clears a stale cancelled flag."
  :tags '(backend)
  (let* ((ollama-buddy--request-cancelled t)
         (ollama-buddy--chat-buffer (generate-new-buffer " *test-chat*"))
         (ollama-buddy--multishot-sequence nil)
         (ollama-buddy--current-model nil)
         (ollama-buddy--current-prompt nil)
         (ollama-buddy--current-tool-calls nil)
         (ollama-buddy--tool-call-iteration 0)
         (ollama-buddy--active-process nil)
         (ollama-buddy-streaming-enabled t)
         (request (list :model "llama3.2:1b"
                        :original-model "llama3.2:1b"
                        :has-images nil
                        :prompt "test")))
    (unwind-protect
        (cl-letf (((symbol-function 'ollama-buddy--insert-response-header)
                   (lambda (&rest _) nil))
                  ((symbol-function 'ollama-buddy--update-status) (lambda (&rest _) nil))
                  ((symbol-function 'ollama-buddy--start-response-wait-timer) (lambda (&rest _) nil))
                  ((symbol-function 'set-register) (lambda (&rest _) nil))
                  ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
                  ((symbol-function 'ollama-buddy--create-intro-message) (lambda () "intro"))
                  ((symbol-function 'visual-line-mode) (lambda (&rest _) nil)))
          (ollama-buddy--setup-chat-send request nil)
          (should-not ollama-buddy--request-cancelled))
      (kill-buffer ollama-buddy--chat-buffer))))

;;; Backend Signature Parity Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-send-signatures-match ()
  "Test that both send functions accept the same number of arguments."
  :tags '(backend)
  ;; Both should accept 3 arguments: prompt, specified-model, tool-continuation-p
  (let ((net-args (help-function-arglist 'ollama-buddy--send))
        (curl-args (when (fboundp 'ollama-buddy-curl--send)
                     (help-function-arglist 'ollama-buddy-curl--send))))
    (when curl-args
      (should (= (length net-args) (length curl-args)))
      ;; Both must have tool-continuation-p as last arg
      (should (eq (car (last net-args)) (car (last curl-args)))))))

(ert-deftest ollama-buddy-test-send-backend-signature-arity ()
  "Test that the dispatcher accepts the same number of args as backends."
  :tags '(backend)
  (let ((dispatcher-args (help-function-arglist 'ollama-buddy--send-backend))
        (send-args (help-function-arglist 'ollama-buddy--send)))
    ;; Same total arg count (ignoring &optional placement)
    (should (= (length (cl-remove '&optional dispatcher-args))
               (length (cl-remove '&optional send-args))))))

;;; HTTP Error Handling Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-curl-filter-handles-429 ()
  "Test that curl filter handles 429 rate-limit errors like network-process."
  :tags '(backend)
  (when (featurep 'ollama-buddy-curl)
    (let* ((ollama-buddy-curl--headers-processed nil)
           (ollama-buddy-curl--http-error-status nil)
           (ollama-buddy--chat-buffer (generate-new-buffer " *test-chat*"))
           (proc-buffer (generate-new-buffer " *test-curl*"))
           (inserted-text nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ollama-buddy--update-status)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ollama-buddy--prepare-prompt-area)
                     (lambda (&rest _) nil))
                    ((symbol-function 'process-live-p)
                     (lambda (_) nil)))
            ;; Simulate curl output: HTTP headers + JSON error body
            (with-current-buffer proc-buffer
              (insert "HTTP/1.1 429 Too Many Requests\r\nContent-Type: application/x-ndjson\r\n\r\n")
              (insert "{\"StatusCode\":429,\"Status\":\"429 Too Many Requests\",\"error\":\"you have reached your weekly usage limit\"}"))
            ;; Create a mock process
            (let ((mock-proc (start-process "test" proc-buffer "true")))
              ;; Run the filter with the full output
              (ollama-buddy-curl--process-filter
               mock-proc
               "HTTP/1.1 429 Too Many Requests\r\nContent-Type: application/x-ndjson\r\n\r\n{\"StatusCode\":429,\"Status\":\"429 Too Many Requests\",\"error\":\"you have reached your weekly usage limit\"}")
              ;; Verify error was displayed in chat buffer
              (with-current-buffer ollama-buddy--chat-buffer
                (setq inserted-text (buffer-string)))
              (should (string-match-p "Error 429" inserted-text))
              (should (string-match-p "weekly usage limit" inserted-text))
              ;; Should NOT be treated as auth error
              (should-not (string-match-p "Authentication Error" inserted-text))
              (ignore-errors (delete-process mock-proc))))
        (ignore-errors (kill-buffer ollama-buddy--chat-buffer))
        (ignore-errors (kill-buffer proc-buffer))))))

(ert-deftest ollama-buddy-test-curl-filter-handles-401 ()
  "Test that curl filter handles 401 auth errors with sign-in message."
  :tags '(backend)
  (when (featurep 'ollama-buddy-curl)
    (let* ((ollama-buddy-curl--headers-processed nil)
           (ollama-buddy-curl--http-error-status nil)
           (ollama-buddy--chat-buffer (generate-new-buffer " *test-chat*"))
           (proc-buffer (generate-new-buffer " *test-curl*"))
           (inserted-text nil)
           (auth-cleared nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ollama-buddy--update-status)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ollama-buddy--prepare-prompt-area)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ollama-buddy--set-cloud-auth-status)
                     (lambda (_) (setq auth-cleared t)))
                    ((symbol-function 'process-live-p)
                     (lambda (_) nil)))
            (let ((mock-proc (start-process "test" proc-buffer "true")))
              (ollama-buddy-curl--process-filter
               mock-proc
               "HTTP/1.1 401 Unauthorized\r\n\r\n{\"error\":\"unauthorized: please sign in\"}")
              (with-current-buffer ollama-buddy--chat-buffer
                (setq inserted-text (buffer-string)))
              (should (string-match-p "Authentication Error" inserted-text))
              (should auth-cleared)
              (ignore-errors (delete-process mock-proc))))
        (ignore-errors (kill-buffer ollama-buddy--chat-buffer))
        (ignore-errors (kill-buffer proc-buffer))))))

(ert-deftest ollama-buddy-test-stream-process-json-handles-error-field ()
  "Test that the shared stream processor handles error JSON from both backends."
  :tags '(backend)
  (let* ((ollama-buddy--chat-buffer (generate-new-buffer " *test-chat*"))
         (inserted-text nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ollama-buddy--update-status)
                   (lambda (&rest _) nil)))
          ;; Simulate a JSON response with error field (as both backends send)
          (ollama-buddy--stream-process-json
           '((error . "you have reached your weekly usage limit")))
          (with-current-buffer ollama-buddy--chat-buffer
            (setq inserted-text (buffer-string)))
          (should (string-match-p "Error" inserted-text))
          (should (string-match-p "weekly usage limit" inserted-text)))
      (ignore-errors (kill-buffer ollama-buddy--chat-buffer)))))

(provide 'ollama-buddy-backend-test)
;;; ollama-buddy-backend-test.el ends here
