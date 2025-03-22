# Set default values if not specified
PREFIX ?= /usr
infodir ?= $(PREFIX)/share/info

.PHONY: docs install-docs

# Build the info documentation
docs: ollama-buddy.info

ollama-buddy.info: ollama-buddy.texi
	makeinfo --no-split $< -o $@

# Install the info documentation
install-docs: docs
	@echo "Installing info documentation..."
	install -d $(DESTDIR)$(infodir)
	install -m 644 ollama-buddy.info $(DESTDIR)$(infodir)/
	install-info --info-dir=$(DESTDIR)$(infodir) ollama-buddy.info
