.PHONY: tests clean

EMACS ?= emacs
# Adding the parent directory to the load path
BATCH := $(EMACS) -Q -batch -L . -L ..

# Run all tests
test:
	$(BATCH) -l tests/savehist-mock.el -l ollama-buddy.el -l tests/ollama-buddy-test.el -f ert-run-testss-batch-and-exit

# Run specific tests
test-%:
	$(BATCH) -l tests/savehist-mock.el -l ollama-buddy.el -l tests/ollama-buddy-test.el --eval "(ert-run-testss-batch-and-exit '(tag $*))"

# Clean up byte-compiled files
clean:
	rm -f *.elc ../*.elc
