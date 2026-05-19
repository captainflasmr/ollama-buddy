---
name: elisp-review
description: >
  Review Emacs Lisp (elisp) packages for bugs, style/convention violations, and
  optimisation opportunities. Use this skill whenever the user asks to review,
  audit, check, or analyse an elisp or Emacs Lisp package, file, or set of
  files. Trigger on phrases like "review my package", "check my elisp", "audit
  this emacs package", "look for bugs in my lisp", "optimise my elisp", or any
  time the user shares .el files and wants feedback. Always use this skill when
  .el files are involved and improvement is the goal — even if the user just says
  "what do you think of this?" about an elisp file.
---

# Elisp Package Review Skill

You are performing a structured code review of an Emacs Lisp package. Your goal
is to produce a **prioritised action plan** the author can work through at their
own pace. Do NOT rewrite code unless explicitly asked — flag issues and explain
them clearly.

---

## Phase 1: Orientation

Before diving into issues, briefly orient yourself:

1. Read every `.el` file in the package.
2. Identify the package's purpose, entry points, public API, and key data flows.
3. Note the Emacs version target and any declared dependencies (`Package-Requires`).
4. Check whether `lexical-binding` is enabled in every file.

State a one-paragraph summary of what the package does before listing any issues.

---

## Phase 2: Review Dimensions

Work through each dimension below in order. Collect ALL findings before
presenting — do not present dimension-by-dimension.

### 1. Correctness & Bugs

Look for logic errors and runtime hazards:

- **Unguarded `car`/`cdr`** on potentially-nil values — use `when`, `if-let`, or
  `and` to guard.
- **Off-by-one errors** in list/string indexing.
- **Mutation of shared structure** — accidental aliasing via `setcar`/`setcdr`.
- **Process/buffer leaks** — processes or temp buffers created but never cleaned
  up; missing `unwind-protect`.
- **Async race conditions** — timer or sentinel callbacks that assume buffer/
  process state that may have changed.
- **Wrong equality predicate** — `eq` vs `equal` vs `string=` misuse.
- **Incorrect use of `mapcar` vs `mapc`** — using `mapcar` when return value is
  discarded (wasteful allocation).
- **`save-excursion` / `save-restriction` misuse** — forgetting to restore state
  after buffer modifications.
- **Hook not removed on cleanup** — hooks added in init but never removed in
  teardown/disable path.
- **Advice not removed** — `advice-add` without corresponding `advice-remove` in
  disable/unload path.
- **Missing `require`** — symbols used from libraries not explicitly required.
- **Circular or redundant `require`** — files requiring each other or requiring
  things already guaranteed by dependencies.

### 2. Emacs Lisp Style & Conventions

Check against established community conventions:

**File header:**
- `;;; package-name.el --- Short description  -*- lexical-binding: t -*-`
- `;;; Commentary:` section present and informative.
- `;;; Code:` marker present.
- `;;; package-name.el ends here` footer present.
- `Package-Version`, `Package-Requires`, `Author`, `Keywords`, `URL` headers
  present and accurate.

**Naming:**
- All public symbols prefixed with `package-name-` (or agreed namespace).
- Internal/private symbols prefixed with `package-name--` (double dash).
- Constants use `defconst` not `defvar`.
- Booleans named with `-p` suffix (`package-name-verbose-p`).

**Docstrings:**
- Every `defun`, `defvar`, `defcustom`, `defface`, `define-minor-mode` has a
  docstring.
- First line of docstring is a complete sentence ending in `.` and ≤80 chars.
- Interactive commands document their argument in the first line if applicable.
- `defcustom` docstrings describe valid values.

**Customisation:**
- User-facing variables use `defcustom`, not `defvar`.
- `defcustom` has correct `:type`, `:group`, and `:safe` where appropriate.
- A `defgroup` exists for the package.

**Functions:**
- Prefer `cl-lib` over deprecated `cl` package (`cl-loop`, `cl-destructuring-bind`, etc.).
- Prefer `seq-` functions over manual recursion for sequence operations.
- Avoid `flet`/`labels` — use `cl-flet`/`cl-labels`.
- Avoid `lexical-let` — unnecessary with `lexical-binding: t`.
- `interactive` spec is correct and uses modern forms (e.g. `(interactive "r")`
  not deprecated forms).
- Functions that modify buffers use `with-current-buffer` rather than relying on
  implicit current buffer.

**Control flow:**
- Prefer `when`/`unless` over `(if ... nil)` / `(if ... t)`.
- Prefer `cond` over deeply nested `if`.
- Prefer `pcase` over complex `cond` matching on structure.
- Avoid `(not (not x))` — use `(and x t)` or just trust truthiness.

### 3. Performance & Optimisation

- **Repeated `buffer-substring` / `buffer-string`** in tight loops — cache the
  result.
- **`re-search-forward` in loops without `narrow-to-region`** — can be
  O(n²); consider reorganising.
- **`append` in loops** — quadratic; prefer `push` + `nreverse`.
- **`length` on a list to check emptiness** — use `null` or `consp` instead.
- **Uncompiled lambdas in hot paths** — prefer named functions or ensure byte
  compilation.
- **Large `defconst` data** — consider lazy initialisation if not always needed.
- **`sit-for 0` / `redisplay` in loops** — usually a sign of a design smell;
  flag and explain.
- **Synchronous process calls blocking UI** — prefer async with sentinels or
  `make-process`.
- **Unnecessary `with-temp-buffer`** — if only string operations are needed,
  avoid buffer allocation.
- **Timer granularity** — timers firing too frequently (< 0.1s) without clear
  need.
- **`font-lock-add-keywords`** called repeatedly — should be called once, not on
  every mode activation.

### 4. Autoloads & Load-Time Cost

- All entry-point commands and public functions the user calls directly should
  have `;;;###autoload` cookies.
- No expensive computation at top level (i.e. outside any function) — this runs
  at load time.
- `defvar` / `defcustom` at top level is fine; `defun` bodies running at load
  time are not.
- `require` at top level is acceptable but flag heavy requires that could be
  deferred with `with-eval-after-load` or `autoload`.

### 5. Compatibility & Portability

- Flag use of functions introduced after the declared minimum Emacs version in
  `Package-Requires`.
- Flag OS-specific code paths without appropriate guards (`system-type` checks).
- Flag hard-coded paths.
- Flag any use of `(require 'cl)` — must use `(require 'cl-lib)`.

### 6. Error Handling & Robustness

- `condition-case` used where failures are plausible (network, file I/O,
  subprocess).
- Error messages are user-readable (not raw Lisp objects).
- `user-error` used for user-facing mistakes (not `error`), so Edebug doesn't
  trap them.
- `unwind-protect` used wherever resources (buffers, processes, overlays) are
  allocated.

---

## Phase 3: Output Format

Present findings as a **structured action plan** using the following format.
Group by severity. Within each group, order by file then by approximate line
number.

```
## Package Review: <package-name>

### Summary
<One paragraph: what the package does, overall impression, headline numbers>

---

### 🔴 Critical — Fix Before Release
Issues that will cause errors, data loss, or broken behaviour.

#### C1. <Short title>
**File:** `foo.el` **~Line:** 42
**Issue:** <Clear explanation of the problem and why it matters>
**Suggestion:** <What to do — no code rewrite, just direction>

#### C2. ...

---

### 🟠 Important — Strong Recommendation
Style violations, missing conventions, or meaningful inefficiencies.

#### I1. <Short title>
...

---

### 🟡 Minor — Worth Addressing
Small style issues, minor optimisations, nitpicks.

#### M1. <Short title>
...

---

### 💡 Optimisation Opportunities
Performance improvements worth considering, ordered by estimated impact.

#### O1. <Short title>
...

---

### ✅ Strengths
Brief list of things done well — keep this honest and specific.
```

---

## Phase 4: Closing Note

After the plan, add a short paragraph:

> "This is a plan for you to action at your own pace — not all items need to be
> addressed. Prioritise 🔴 Critical items first. Feel free to ask me to elaborate
> on any specific finding or to help implement a fix."

---

## Review Principles

- **Flag, don't fix.** Explain the problem and point in a direction. The author
  decides what to do.
- **Be specific.** Always cite the file and approximate line number.
- **Be proportionate.** A one-file utility and a major package deserve different
  levels of rigour — calibrate accordingly.
- **No ERT / testing review.** Do not comment on presence or absence of tests.
- **Respect intent.** If a pattern looks unusual but is clearly deliberate,
  note it as a question rather than a violation.
