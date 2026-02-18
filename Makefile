# Makefile for ollama-buddy
# Run tests, lint, and byte-compile Emacs Lisp files

EMACS ?= emacs
BATCH = $(EMACS) -Q --batch

# All elisp files
ELISP_FILES = ollama-buddy.el \
              ollama-buddy-core.el \
              ollama-buddy-curl.el \
              ollama-buddy-transient.el \
              ollama-buddy-awesome.el \
              ollama-buddy-fabric.el \
              ollama-buddy-user-prompts.el \
              ollama-buddy-remote.el \
              ollama-buddy-openai.el \
              ollama-buddy-claude.el \
              ollama-buddy-gemini.el \
              ollama-buddy-grok.el \
              ollama-buddy-copilot.el \
              ollama-buddy-codestral.el \
              ollama-buddy-deepseek.el \
              ollama-buddy-openrouter.el \
              ollama-buddy-rag.el

# Test files
TEST_DIR = tests
TEST_FILES = $(wildcard $(TEST_DIR)/*-test.el)

.PHONY: all test test-core test-rag lint checkdoc byte-compile clean ci help

all: ci

## Run all ERT tests
test:
	$(BATCH) \
		-L . \
		-L $(TEST_DIR) \
		-l $(TEST_DIR)/test-helper.el \
		$(foreach file,$(TEST_FILES),-l $(file)) \
		-f ert-run-tests-batch-and-exit

## Run only RAG tests (tagged :rag)
test-rag:
	$(BATCH) \
		-L . \
		-L $(TEST_DIR) \
		-l $(TEST_DIR)/test-helper.el \
		-l $(TEST_DIR)/ollama-buddy-rag-test.el \
		--eval "(ert-run-tests-batch-and-exit '(tag rag))"

## Run only core tests (tagged :core)
test-core:
	$(BATCH) \
		-L . \
		-L $(TEST_DIR) \
		-l $(TEST_DIR)/test-helper.el \
		-l $(TEST_DIR)/ollama-buddy-core-test.el \
		--eval "(ert-run-tests-batch-and-exit '(tag core))"

## Run package-lint on all elisp files
lint:
	$(BATCH) \
		--eval "(require 'package)" \
		--eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" \
		-l package-lint \
		-f package-lint-batch-and-exit $(ELISP_FILES)

## Run checkdoc on all elisp files
checkdoc:
	@for file in $(ELISP_FILES); do \
		echo "Checking $$file..."; \
		$(BATCH) \
			-L . \
			--eval "(require 'checkdoc)" \
			--eval "(setq checkdoc-spellcheck-documentation-flag nil)" \
			--eval "(let ((checkdoc-diagnostic-buffer \"*Checkdoc*\")) \
			          (checkdoc-file \"$$file\") \
			          (with-current-buffer \"*Checkdoc*\" \
			            (unless (= (point-min) (point-max)) \
			              (princ (buffer-string)) \
			              (kill-emacs 1))))"; \
	done

## Byte-compile all elisp files
byte-compile:
	$(BATCH) \
		-L . \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(ELISP_FILES)

## Remove compiled files
clean:
	rm -f *.elc
	rm -f $(TEST_DIR)/*.elc

## Run full CI check (lint + byte-compile + test)
ci: byte-compile test

## Show available targets
help:
	@echo "Available targets:"
	@echo "  test         - Run all ERT tests"
	@echo "  test-core    - Run tests tagged :core"
	@echo "  lint         - Run package-lint"
	@echo "  checkdoc     - Run checkdoc"
	@echo "  byte-compile - Byte-compile all files"
	@echo "  clean        - Remove .elc files"
	@echo "  ci           - Full CI check (byte-compile + test)"
	@echo "  help         - Show this help"
