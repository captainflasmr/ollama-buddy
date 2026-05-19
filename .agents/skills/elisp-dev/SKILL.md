---
name: elisp-dev
description: >
  Assist with Emacs Lisp (elisp) development, debugging, and parenthesis
  matching. Use this skill when the user is writing, editing, or debugging
  elisp code, struggling with unbalanced parentheses, or needs help with
  elisp development workflows. Trigger on phrases like "debug my elisp",
  "unbalanced parentheses", "paren matching", "elisp debugging", or any
  request involving writing or fixing Emacs Lisp code.
---

# Emacs Lisp Development Skill

You are assisting with Emacs Lisp development. Your goal is to help the user
write correct, idiomatic elisp and debug issues efficiently.

---

## Parenthesis Matching & Navigation

### Built-in Commands

| Key / Command | Action |
|---------------|--------|
| `C-M-f` (`forward-sexp`) | Move forward across one balanced expression |
| `C-M-b` (`backward-sexp`) | Move backward across one balanced expression |
| `C-M-n` (`forward-list`) | Move forward across one balanced list |
| `C-M-p` (`backward-list`) | Move backward across one balanced list |
| `C-M-u` (`backward-up-list`) | Move up one level of parentheses |
| `C-M-d` (`down-list`) | Move down one level of parentheses |
| `C-M-k` (`kill-sexp`) | Kill the balanced expression following point |
| `C-M-SPC` (`mark-sexp`) | Mark the balanced expression following point |
| `C-M-t` (`transpose-sexps`) | Transpose the sexps around point |

### Showing Matching Parens

Enable `show-paren-mode` globally:

```elisp
(show-paren-mode 1)
```

Or configure it:

```elisp
(setq show-paren-delay 0
      show-paren-style 'mixed)  ; 'parenthesis, 'expression, or 'mixed
```

### Structural Editing with `paredit` or `smartparens`

**Paredit** provides strict structural editing:

| Key | Action |
|-----|--------|
| `(` | Inserts `()` and places cursor inside |
| `)` | Moves past closing paren ("smart close") |
| `M-("` | Wraps the next sexp in parentheses |
| `M-s"` | Splits the current sexp at cursor |
| `M-r"` | Raises the current sexp (replaces parent with child) |
| `C-k"` | Kills the sexp, preserving structure |
| `M-<up>`` | Splice sexp, killing everything around it |
| `M-<down>`` | Splice sexp into parent |

**Smartparens** is more flexible and works with multiple languages:

```elisp
(require 'smartparens-config)
(smartparens-global-mode 1)
```

### Finding Unbalanced Parentheses

1. **Use `check-parens`**:
   - `M-x check-parens` jumps to the first unmatched paren
   - Works in any mode, not just elisp

2. **Use Emacs to scan**:
   ```elisp
   ;; Evaluate this to find the position of unbalanced parens
   (with-current-buffer (current-buffer)
     (goto-char (point-min))
     (condition-case err
         (while (not (eobp))
           (forward-sexp))
       (error (message "Unbalanced at position %d, line %d"
                       (point) (line-number-at-pos)))))
   ```

3. **Use `count-pairs` approach**:
   ```elisp
   ;; Count all parens in the file
   (let ((open (count-matches "(" (point-min) (point-max)))
         (close (count-matches ")" (point-min) (point-max))))
     (message "Open: %d, Close: %d, Diff: %d" open close (- open close)))
   ```

4. **Narrow to the problematic region**:
   - Select the region you think is correct
   - `C-x n n` (`narrow-to-region`)
   - `M-x check-parens`
   - `C-x n w` (`widen`) to restore

### Common Paren Mistakes

- **Missing close paren at end of file**: Emacs reports "End of file during parsing"
- **Extra close paren**: Emacs reports "Scan error: Unbalanced parentheses"
- **Mismatched string quotes**: A `"` inside a string without escaping breaks parsing
- **Unescaped backticks or commas** inside docstrings

---

## Debugging Techniques

### 1. `message` — The Print Debugger

```elisp
(message "Value of x: %S" x)  ; %S for lisp objects, %s for strings
(message "Point is at %d" (point))
(message "Buffer: %s, Line: %d" (buffer-name) (line-number-at-pos))
```

View the `*Messages*` buffer with `C-h e`.

### 2. `debug-on-error` — Automatic Stack Traces

```elisp
(setq debug-on-error t)        ; Enter debugger on any error
(setq debug-on-signal t)       ; Also break on signals (very verbose)
```

Toggle interactively: `M-x toggle-debug-on-error`

In the debugger:
- `c` — continue
- `d` — step through (single-step)
- `q` — quit debugger
- `e` — evaluate expression
- `h` — help

### 3. Edebug — The Source-Level Debugger

**Instrument a function for debugging:**
- `C-u C-M-x` (`eval-defun` with prefix) — instrument the current defun
- `M-x edebug-defun` — same thing
- `M-x edebug-all-defs` — instrument all subsequent defs as they're evaluated

**Execution control while in Edebug:**

| Key | Action |
|-----|--------|
| `SPC` | Step through (next expression) |
| `n` | Next (skip stepping into function calls) |
| `t` | Trace (show but don't stop at each step) |
| `T` | Rapid trace (faster, less display) |
| `g` | Go (run until next breakpoint) |
| `c` | Continue (run until done or break) |
| `f` | Forward (exit current function) |
| `o` | Out (step out of current function) |
| `i` | Step in (step into function call) |
| `q` | Quit Edebug |
| `b` | Set breakpoint at point |
| `u` | Unset breakpoint |
| `x` | Set conditional breakpoint |
| `e` | Evaluate expression (`M-:` in Edebug) |
| `r` | Return a value from current function |
| `d` | Display details of current expression |
| `w` | Where (show backtrace) |
| `?` | Help |

**Breakpoints:**
- `b` at point to set/unset
- `x` for conditional: `x (equal x 42)`
- `B` to show all breakpoints
- `U` to unset all breakpoints

**Evaluate expressions during debugging:**
- `e` — evaluate in minibuffer
- `M-:` — evaluate with completion

### 4. `trace-function` — Function Call Tracing

```elisp
(require 'trace)
(trace-function 'my-function)     ; Trace calls to my-function
(untrace-function 'my-function)   ; Stop tracing
(untrace-all)                     ; Stop all tracing
```

Output goes to `*trace-output*` buffer.

### 5. Backtrace on Demand

```elisp
(debug)  ; Enter debugger immediately when this is evaluated
```

Or use `M-x debug` to enter the debugger at the current position.

### 6. Timing and Performance

```elisp
;; Simple timing
(benchmark-run 1 (my-function))

;; More detailed
(benchmark-run-compiled 100 (my-function))

;; Profile a specific function
(profiler-start 'cpu)
(my-function)
(profiler-stop)
(profiler-report)
```

### 7. Inspecting Variables and State

```elisp
;; Describe a variable
C-h v variable-name

;; Describe a function
C-h f function-name

;; Apropos (search for symbols)
C-h a regexp

;; Inspect value
M-: variable-name RET

;; Pretty-print a complex structure
M-x pp-eval-expression RET variable-name RET
```

---

## Development Workflow

### Evaluating Code Incrementally

| Key | Action |
|-----|--------|
| `C-M-x` | Evaluate the current defun |
| `C-x C-e` | Evaluate the sexp before point |
| `C-j` | Evaluate and print result in current buffer (Lisp Interaction mode) |
| `M-:` | Evaluate expression in minibuffer |
| `C-x C-e` with prefix (`C-u`) | Insert result into buffer |

### Working with Multiple Files

```elisp
;; Load a file
(load-file "~/path/to/file.el")

;; Require a feature (triggers autoloads)
(require 'my-feature)

;; Load after another package loads
(with-eval-after-load 'org
  (require 'my-org-extension))
```

### Testing Changes

1. Edit the function
2. `C-M-x` to evaluate it
3. Test it in a scratch buffer or via `M-:`
4. If it's broken, `C-h e` to check messages
5. If needed, `C-u C-M-x` to instrument with Edebug

### Byte Compilation

```elisp
;; Byte-compile current buffer
M-x emacs-lisp-byte-compile

;; Byte-compile and load
M-x emacs-lisp-byte-compile-and-load

;; Check for warnings
M-x byte-compile-file
```

Always check compiler warnings — they often catch real bugs:
- Unused variables
- Undefined functions
- Obsolete function calls
- Wrong number of arguments

---

## Common Debugging Scenarios

### "End of file during parsing"

1. `M-x check-parens` — jumps to the problem
2. Look for missing close parens near the end of the file
3. Check for unterminated strings (`"` without matching `"`)
4. Check for unterminated comments (`;` is fine, but `#|` needs `|#`)

### "Symbol's function definition is void"

- The function hasn't been defined or loaded
- Check if you need `(require 'some-library)`
- Check for typos in the function name
- If it's a macro, ensure the file is loaded before the macro is used

### "Wrong number of arguments"

- Check the function signature with `C-h f function-name`
- Some functions changed signatures between Emacs versions
- Check if a macro is being called like a function

### "Args out of range" or "Beginning/End of buffer"

- Check buffer boundaries before operations
- Use `save-excursion` or `save-restriction` if modifying point

### "Invalid function"

- Usually means a list is being called as a function
- Check if you meant to use `funcall` or `apply`
- Often caused by missing `#'` before function names

### Infinite Loop or Hanging

- `C-g` to quit
- `M-x toggle-debug-on-quit` to get a backtrace on `C-g`
- Add `(message "…")` calls to find the loop
- Check `while` loops for missing increment/progress

---

## Useful Minor Modes

```elisp
;; Enable eldoc (show function signature in echo area)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;; Enable flymake for on-the-fly checking
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

;; Enable paredit or smartparens
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
;; or
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

;; Enable rainbow delimiters for visual paren matching
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
```

---

## Best Practices

1. **Always use `lexical-binding: t`** in the file header
2. **Use `#'` (function quote)** when passing functions: `(mapcar #'car list)`
3. **Use `defcustom`** for user-facing variables, `defvar` for internal
4. **Prefix all symbols** with your package name: `my-package--private`, `my-package-public`
5. **Write docstrings** for every public function and variable
6. **Use `when`/`unless`** instead of `(if … nil)`
7. **Use `condition-case`** for operations that might fail
8. **Use `with-temp-buffer`** instead of creating and managing temp buffers manually
9. **Check for nil** before calling `car`/`cdr` on potentially empty lists
10. **Test incrementally** — evaluate each function as you write it

---

## Quick Reference: Debugging Commands

```
C-M-x          eval-defun
C-u C-M-x      eval-defun + edebug instrument
C-x C-e        eval-last-sexp
M-:            eval-expression
C-h e          view-echo-area-messages
M-x edebug-defun
M-x toggle-debug-on-error
M-x check-parens
C-M-f          forward-sexp
C-M-b          backward-sexp
C-M-u          backward-up-list
C-M-d          down-list
```
