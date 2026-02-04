;;; ollama-buddy-core-test.el --- Tests for ollama-buddy-core -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Dyer

;; Author: James Dyer <captainflasmr@gmail.com>

;;; Commentary:

;; ERT tests for ollama-buddy-core.el
;; Run with: make test-core

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'ollama-buddy-core)

;;; Token Estimation Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-estimate-token-count-empty ()
  "Test token estimation with empty string."
  :tags '(core)
  (should (= 0 (ollama-buddy--estimate-token-count ""))))

(ert-deftest ollama-buddy-test-estimate-token-count-single-word ()
  "Test token estimation with single word."
  :tags '(core)
  ;; Single word = 1 word * 1.3 = 1 (rounded)
  (should (= 1 (ollama-buddy--estimate-token-count "hello"))))

(ert-deftest ollama-buddy-test-estimate-token-count-sentence ()
  "Test token estimation with a typical sentence."
  :tags '(core)
  ;; "The quick brown fox" = 4 words * 1.3 = 5 (rounded)
  (should (= 5 (ollama-buddy--estimate-token-count "The quick brown fox"))))

(ert-deftest ollama-buddy-test-estimate-token-count-longer-text ()
  "Test token estimation with longer text."
  :tags '(core)
  ;; 10 words * 1.3 = 13
  (let ((text "one two three four five six seven eight nine ten"))
    (should (= 13 (ollama-buddy--estimate-token-count text)))))

;;; Cloud Model Detection Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-cloud-model-p-suffix ()
  "Test cloud model detection by -cloud suffix."
  :tags '(core)
  (with-ollama-buddy-test-env
    (should (ollama-buddy--cloud-model-p "anything-cloud"))
    (should (ollama-buddy--cloud-model-p "model:tag-cloud"))))

(ert-deftest ollama-buddy-test-cloud-model-p-in-list ()
  "Test cloud model detection by membership in cloud models list."
  :tags '(core)
  (with-ollama-buddy-test-env
    (should (ollama-buddy--cloud-model-p "qwen3-coder:480b-cloud"))
    (should (ollama-buddy--cloud-model-p "deepseek-v3.1:671b-cloud"))))

(ert-deftest ollama-buddy-test-cloud-model-p-local ()
  "Test that local models are not detected as cloud."
  :tags '(core)
  (with-ollama-buddy-test-env
    (should-not (ollama-buddy--cloud-model-p "llama3.2:1b"))
    (should-not (ollama-buddy--cloud-model-p "qwen2.5-coder:3b"))
    (should-not (ollama-buddy--cloud-model-p "tinyllama:latest"))))

(ert-deftest ollama-buddy-test-cloud-model-p-nil ()
  "Test cloud model detection with nil input."
  :tags '(core)
  (should-not (ollama-buddy--cloud-model-p nil)))

;;; Title Extraction Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-extract-title-you-are ()
  "Test title extraction from 'You are a...' pattern."
  :tags '(core)
  ;; Note: capitalize produces Title Case, regex captures until punctuation
  (should (equal "Helpful Assistant"
                 (ollama-buddy--extract-title-from-content
                  "You are a helpful assistant.")))
  ;; Long titles get truncated at 40 chars with "..."
  (should (string-prefix-p "Expert Programmer Who Writes"
                 (ollama-buddy--extract-title-from-content
                  "You are an expert programmer who writes clean code."))))

(ert-deftest ollama-buddy-test-extract-title-act-as ()
  "Test title extraction from 'Act as...' pattern."
  :tags '(core)
  ;; Note: capitalize produces Title Case, regex captures until punctuation
  (should (equal "Python Expert"
                 (ollama-buddy--extract-title-from-content
                  "Act as a Python expert.")))
  ;; Captures full phrase up to punctuation
  (should (equal "Code Reviewer For This Project"
                 (ollama-buddy--extract-title-from-content
                  "Act as the code reviewer for this project."))))

(ert-deftest ollama-buddy-test-extract-title-i-want-you ()
  "Test title extraction from 'I want you to act as...' pattern."
  :tags '(core)
  ;; Note: capitalize produces Title Case
  (should (equal "Technical Writer"
                 (ollama-buddy--extract-title-from-content
                  "I want you to act as a technical writer."))))

(ert-deftest ollama-buddy-test-extract-title-your-role ()
  "Test title extraction from 'Your role is...' pattern."
  :tags '(core)
  ;; Note: capitalize produces Title Case
  (should (equal "Senior Developer"
                 (ollama-buddy--extract-title-from-content
                  "Your role is the senior developer."))))

(ert-deftest ollama-buddy-test-extract-title-fallback ()
  "Test title extraction fallback to first words."
  :tags '(core)
  (should (equal "Just some random"
                 (ollama-buddy--extract-title-from-content
                  "Just some random text here."))))

(ert-deftest ollama-buddy-test-extract-title-nil ()
  "Test title extraction with nil input."
  :tags '(core)
  (should (null (ollama-buddy--extract-title-from-content nil))))

(ert-deftest ollama-buddy-test-extract-title-empty ()
  "Test title extraction with empty string."
  :tags '(core)
  ;; Empty string after trim should return "Custom Prompt"
  (should (equal "Custom Prompt"
                 (ollama-buddy--extract-title-from-content "   "))))

;;; Unicode Escaping Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-escape-unicode-ascii ()
  "Test that ASCII strings pass through unchanged."
  :tags '(core)
  (should (equal "hello world"
                 (ollama-buddy-escape-unicode "hello world")))
  (should (equal "foo123!@#"
                 (ollama-buddy-escape-unicode "foo123!@#"))))

(ert-deftest ollama-buddy-test-escape-unicode-non-ascii ()
  "Test escaping of non-ASCII characters."
  :tags '(core)
  ;; café -> caf\u00E9
  (should (equal "caf\\u00E9"
                 (ollama-buddy-escape-unicode "café")))
  ;; 你好 -> \u4F60\u597D
  (should (equal "\\u4F60\\u597D"
                 (ollama-buddy-escape-unicode "你好"))))

(ert-deftest ollama-buddy-test-escape-unicode-mixed ()
  "Test escaping of mixed ASCII and non-ASCII."
  :tags '(core)
  ;; Mixed content should have non-ASCII escaped
  (let ((result (ollama-buddy-escape-unicode "hello, señor")))
    (should (string-prefix-p "hello, se" result))
    (should (string-match-p "\\\\u00F1or" result))))

(ert-deftest ollama-buddy-test-escape-unicode-empty ()
  "Test escaping empty string."
  :tags '(core)
  (should (equal "" (ollama-buddy-escape-unicode ""))))

;;; Model Name Handling Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-get-real-model-name-with-prefix ()
  "Test extracting real model name when prefix is present."
  :tags '(core)
  (with-ollama-buddy-test-env
    ;; When remote models exist, prefix should be stripped
    (let ((ollama-buddy-remote-models '("a:gpt-4")))
      (should (equal "llama3.2:1b"
                     (ollama-buddy--get-real-model-name "o:llama3.2:1b"))))))

(ert-deftest ollama-buddy-test-get-real-model-name-no-prefix ()
  "Test model name without prefix passes through."
  :tags '(core)
  (with-ollama-buddy-test-env
    (should (equal "llama3.2:1b"
                   (ollama-buddy--get-real-model-name "llama3.2:1b")))))

(ert-deftest ollama-buddy-test-get-real-model-name-no-remote ()
  "Test model name when no remote models configured."
  :tags '(core)
  (with-ollama-buddy-test-env
    ;; With no remote models, prefix should NOT be stripped
    (let ((ollama-buddy-remote-models nil))
      (should (equal "o:llama3.2:1b"
                     (ollama-buddy--get-real-model-name "o:llama3.2:1b"))))))

;;; Provider Indicator Tests
;; ============================================================================

(ert-deftest ollama-buddy-test-get-enabled-external-providers-none ()
  "Test provider detection when no providers loaded."
  :tags '(core)
  ;; When no provider features are present
  (cl-letf (((symbol-function 'featurep) (lambda (_) nil)))
    (should (equal '() (ollama-buddy--get-enabled-external-providers)))))

(provide 'ollama-buddy-core-test)
;;; ollama-buddy-core-test.el ends here
