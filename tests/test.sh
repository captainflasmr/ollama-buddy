#!/bin/bash

# List of elisp files to compile
elisp_files=(
  "ollama-buddy-awesome.el"
  "ollama-buddy-claude.el"
  "ollama-buddy-core.el"
  "ollama-buddy.el"
  "ollama-buddy-fabric.el"
  "ollama-buddy-gemini.el"
  "ollama-buddy-openai.el"
  "ollama-buddy-transient.el"
)

# Directory above current one
elisp_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"

for el in "${elisp_files[@]}"; do
  emacs -Q --batch \
    --eval "(add-to-list 'load-path \"$elisp_dir\")" \
    -f batch-byte-compile "$elisp_dir/$el"
done
