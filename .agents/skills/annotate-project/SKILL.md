---
name: annotate-project
description: >
  Analyse a project's source code and generate a simply-annotate database file
  with LLM-created annotations categorised by tag: `defun` for functions and
  large syntactical blocks, `line` for smaller sub-blocks. File-wide narrative
  lives in a sibling `.org` overview file, not in an overlay. Use this skill
  when the user asks to annotate a project, generate code descriptions, or
  create a simply-annotate database for a codebase. Trigger on phrases like
  "annotate this project", "generate annotations", "describe this codebase",
  "create annotations for", or "annotate the source".
---

# Annotate Project Skill

You are generating a **simply-annotate** database file that describes a
project's source code using two tag categories — `defun` (functions, classes,
types, and other major syntactical blocks) and `line` (smaller sub-blocks worth
calling out). When the user opens files in Emacs with `simply-annotate-mode`,
they see these as tagged annotations they can filter and step through.

**Important: simply-annotate's annotation model is tag-based, not
level-based.** The legacy `level` field (`file`/`defun`/`line`) no longer
exists as a first-class concept — what used to be a level is now a tag on a
thread-format annotation. File-wide overview annotations (overlays spanning
the whole buffer) are **not produced**; that narrative lives exclusively in
the sibling directory overview `.org` file created in Step 9.

---

## Step 1: Discover Source Files

If the user provides a specific directory or glob argument (e.g.
`/annotate-project src/` or `/annotate-project java/cuis-server`), scan **only
that scope**. Otherwise scan the project root.

Use the Glob tool to find files matching common source extensions:
- `**/*.el` `**/*.py` `**/*.js` `**/*.ts` `**/*.tsx` `**/*.jsx`
- `**/*.rs` `**/*.go` `**/*.java` `**/*.c` `**/*.cpp` `**/*.h`
- `**/*.rb` `**/*.lua` `**/*.sh` `**/*.clj` `**/*.hs` `**/*.proto`

### Directories to skip

Respect `.gitignore` and always skip these directories/patterns:
- `node_modules`, `vendor`, `dist`, `build`, `target`, `.git`, `__pycache__`,
  `.elc`, `.class`
- Directories named `generated`, `gensrc`, `gen`, `auto-generated`
- Third-party / vendored code: `third_party`, `thirdparty`, `extern`, `deps`

### File count guard

After discovering files, **count them** and report the total to the user
**before doing any reading or annotation**. Include:

1. The total number of source files found.
2. A breakdown by immediate subdirectory (top 10 by count).
3. Any directories that look auto-generated (containing `generated`, `gensrc`,
   `gen`, `proto/generated`, `pb.go`, `.pb.h`, etc.) — flag these and ask
   whether to include them.

If the file count exceeds **100 files**, warn the user:

> Found N source files. Annotating all of them will take a while.
> Would you like to proceed, or narrow the scope to a specific subdirectory?

Wait for confirmation before continuing. If the user narrows scope, re-discover
within that scope.

If the count is **≤ 100**, proceed automatically.

---

## Step 2: Determine the Database Path

Annotations are stored in a **project-local** database file by default. This
keeps each project's annotations self-contained, makes them shareable via
version control if desired, and avoids one giant global database growing
unbounded.

Resolve the database path in this order:

1. **Project-local (default).** Find the project root by walking up from the
   current working directory looking for one of: `.git`, `.hg`, `.svn`,
   `package.json`, `Cargo.toml`, `pyproject.toml`, `go.mod`, `pom.xml`,
   `build.gradle`, `build.gradle.kts`, `Makefile`. Use that directory. If
   nothing is found, use the current working directory itself.

   Database path: `<project-root>/.simply-annotations.el`

2. **Global fallback.** Only use `~/.emacs.d/simply-annotations.el` if the
   user explicitly asks for the global database (e.g. "use the global db",
   "save to ~/.emacs.d/...").

Use the resolved path consistently in **all** subsequent steps (backup,
generate, validate). Refer to it below as `$DB_PATH`.

---

## Step 3: Write the Elisp Helper Script

Since the annotations target Emacs, use a **native elisp batch script** to
convert line numbers to Emacs point positions. This guarantees byte offsets
match what Emacs sees — no manual conversion needed.

The script produces annotations in the current thread-alist format used by
simply-annotate:

```elisp
((start . N)
 (end . N)
 (text . ((id . "thread-...")
          (created . "...")
          (status . "open")
          (priority . "normal")
          (tags . ("defun"))
          (comments . (((id . "comment-...")
                        (parent-id . nil)
                        (author . "Claude")
                        (timestamp . "...")
                        (text . "The actual annotation text")
                        (type . "comment")))))))
```

It also accepts an **optional project root** third argument. When supplied,
the DB keys are rewritten to project-relative paths (`src/foo.ts` rather than
`/home/user/project/src/foo.ts`) — this is **required** for project-local
databases, because simply-annotate's `simply-annotate--file-key` returns a
project-relative path when looking up annotations, and a DB keyed by absolute
paths will silently find nothing.

Write the following script **verbatim** to `/tmp/gen_annotations.el` at the
start of the session. Do not modify it — it is tested and known to work.

```elisp
;;; gen_annotations.el --- Convert line-based annotation JSON to simply-annotate (thread-tag format) -*- lexical-binding: t; -*-

;; Usage: emacs --batch -l /tmp/gen_annotations.el /tmp/annotations.json $DB_PATH [PROJECT_ROOT]
;;
;; When PROJECT_ROOT is supplied, keys are rewritten to project-relative paths.
;; This is REQUIRED for project-local databases.
;;
;; Input JSON shape (per file):
;;   { "/abs/path/file.ts": [
;;       {"sl": 8, "el": 52, "text": "...", "tag": "defun"},
;;       {"sl": 32, "el": 51, "text": "...", "tag": "line"}
;;     ] }

(require 'json)
(require 'cl-lib)

(defvar gen-annotations--counter 0)

(defun gen-annotations--uid (prefix)
  "Return a stable unique id with PREFIX."
  (setq gen-annotations--counter (1+ gen-annotations--counter))
  (format "%s-%s-%06d"
          prefix
          (format-time-string "%s")
          gen-annotations--counter))

(defun gen-annotations--make-thread (text tag)
  "Wrap TEXT into a minimal thread alist tagged with TAG."
  (let ((now (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
    `((id . ,(gen-annotations--uid "thread"))
      (created . ,now)
      (status . "open")
      (priority . "normal")
      (tags . (,tag))
      (comments . (((id . ,(gen-annotations--uid "comment"))
                    (parent-id . nil)
                    (author . "Claude")
                    (timestamp . ,now)
                    (text . ,text)
                    (type . "comment")))))))

(defun gen-annotations--line-to-point (buf line-number)
  "In BUF, return the point at the beginning of LINE-NUMBER (1-based)."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (if (and (> line-number 0)
               (zerop (forward-line (1- line-number))))
          (point)
        (point-max)))))

(defun gen-annotations--end-of-line-region (buf line-number)
  "In BUF, return the point at end of LINE-NUMBER (1-based)."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (if (and (> line-number 0)
               (zerop (forward-line (1- line-number))))
          (line-end-position)
        (point-max)))))

(defun gen-annotations--convert-file (filepath annotations)
  "Convert ANNOTATIONS for FILEPATH from line numbers to point positions.
Each input annotation has keys sl, el, text, tag."
  (if (not (file-exists-p filepath))
      (progn
        (message "WARNING: File not found, skipping: %s" filepath)
        nil)
    (let ((buf (generate-new-buffer " *gen-ann-temp*"))
          result)
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert-file-contents filepath))
            (dolist (ann annotations)
              (let* ((sl (alist-get 'sl ann))
                     (el (alist-get 'el ann))
                     (text (alist-get 'text ann))
                     (tag (alist-get 'tag ann))
                     (start (gen-annotations--line-to-point buf sl))
                     (end (if (= el -1)
                              (with-current-buffer buf (point-max))
                            (gen-annotations--end-of-line-region buf el)))
                     (thread (gen-annotations--make-thread text tag)))
                (push `((start . ,start)
                        (end . ,end)
                        (text . ,thread))
                      result))))
        (kill-buffer buf))
      ;; Sort by start position, ascending.
      (sort (nreverse result)
            (lambda (a b) (< (alist-get 'start a) (alist-get 'start b)))))))

(defun gen-annotations--load-existing (output-path)
  "Load existing database from OUTPUT-PATH if it exists."
  (when (file-exists-p output-path)
    (with-temp-buffer
      (insert-file-contents output-path)
      (let ((content (string-trim (buffer-string))))
        (unless (string-empty-p content)
          (condition-case err
              (car (read-from-string content))
            (error
             (message "WARNING: Could not parse existing database: %s"
                      (error-message-string err))
             nil)))))))

(defun gen-annotations--db-key (filepath project-root)
  "Return the DB key for FILEPATH, made project-relative when PROJECT-ROOT is set."
  (if (and project-root
           (stringp project-root)
           (> (length project-root) 0)
           (string-prefix-p (file-name-as-directory project-root) filepath))
      (substring filepath (length (file-name-as-directory project-root)))
    filepath))

(defun gen-annotations--main ()
  "Main entry point for batch annotation generation."
  (let* ((args (last command-line-args 3))
         (json-path (nth 0 args))
         (output-path (and (nth 1 args) (expand-file-name (nth 1 args))))
         (project-root (and (nth 2 args)
                            (file-name-as-directory
                             (expand-file-name (nth 2 args))))))

    (unless (and json-path output-path)
      (error "Usage: emacs --batch -l gen_annotations.el /tmp/annotations.json output.el [project-root]"))

    (let* ((json-object-type 'alist)
           (json-array-type 'list)
           (json-key-type 'symbol)
           (annotations (with-temp-buffer
                          (insert-file-contents json-path)
                          (json-read)))
           (existing-db (gen-annotations--load-existing output-path))
           (new-db existing-db)
           (file-count 0))

      (dolist (entry annotations)
        (let* ((filepath (symbol-name (car entry)))
               (db-key (gen-annotations--db-key filepath project-root))
               (file-annotations (cdr entry))
               ;; Read from absolute filepath for line-to-point conversion;
               ;; only the DB key is made relative.
               (converted (gen-annotations--convert-file filepath file-annotations)))
          (when converted
            (cl-incf file-count)
            (if (assoc db-key new-db #'string=)
                (setcdr (assoc db-key new-db #'string=) converted)
              (push (cons db-key converted) new-db)))))

      (setq new-db (sort new-db (lambda (a b) (string< (car a) (car b)))))

      (with-temp-file output-path
        (insert ";;; Simply Annotate Database\n")
        (insert ";;; This file is auto-generated. Do not edit manually.\n\n")
        (let ((print-level nil)
              (print-length nil))
          (prin1 new-db (current-buffer)))
        (insert "\n"))

      (message "Generated annotations for %d files -> %s" file-count output-path))))

(gen-annotations--main)
```

---

## Step 4: Read and Analyse Each File

For each source file, read the contents and produce annotations under two
tags: `defun` and `line`. Record annotations using **line numbers** (`sl`/`el`)
— the helper script will convert to byte offsets.

**Do not produce file-wide overlay annotations.** In the previous model there
was a `file` level covering the whole buffer (`sl: 1, el: -1`). That overlay
is no longer part of the new format — a single annotation spanning the whole
file overlaps every other annotation and confuses tag filtering. The file's
high-level narrative belongs in the sibling `.org` overview file created in
Step 9, where it can be read as prose.

### Tag: `defun`

One annotation per major syntactical block:
- **Elisp**: `defun`, `defmacro`, `defvar`, `defcustom`, `defclass`,
  `define-minor-mode`, `use-package` blocks
- **Python**: `def`, `class`, top-level decorators
- **JavaScript/TypeScript**: `function`, `class`, arrow function assignments,
  `export default`, React components
- **Rust**: `fn`, `impl`, `struct`, `enum`, `trait`, `mod`
- **Go**: `func`, `type`, `interface`
- **C/C++**: function definitions, `struct`/`class`/`enum` definitions
- **Ruby**: `def`, `class`, `module`
- **Java**: method definitions, class definitions
- **Shell**: function definitions
- **Proto**: `message`, `enum`, `service`, `rpc` definitions

The annotation `sl` and `el` should span the full block (from the first line of
the definition to the closing delimiter line). Write a description covering:
what it does, parameters/return values (briefly), key side effects, and how it
relates to the rest of the file. 1-3 sentences.

**Skip defun-tagged annotations for trivial files** (< 30 lines, or pure
boilerplate like single-exception classes, simple data holders, or marker
interfaces).

### Tag: `line`

Annotations for smaller but meaningful sub-blocks within defun-tagged regions:
- Significant `let`/`let*` binding blocks
- Important conditional branches (`if`/`cond`/`pcase`/`match`/`switch`)
- Loop bodies with non-trivial logic
- Error handling blocks (`condition-case`, `try`/`catch`/`except`)
- Key variable assignments that control flow
- Non-obvious expressions that benefit from explanation

Do NOT annotate every line — focus on blocks where a reader would benefit from
context. Aim for roughly 3-8 `line`-tagged annotations per `defun`-tagged
block, fewer for simple functions. **Skip `line` annotations entirely for
simple functions.**

---

## Step 5: Produce Annotation JSON

Collect all annotations into a single JSON object. Use **absolute paths** as
the keys (the helper script will rewrite them to project-relative paths using
the project-root argument in Step 7):

```json
{
  "/absolute/path/to/file.java": [
    {"sl": 25, "el": 80, "text": "Class description...", "tag": "defun"},
    {"sl": 42, "el": 55, "text": "Key conditional...", "tag": "line"}
  ]
}
```

Note there is **no file-wide entry** (`sl: 1, el: -1`) — that narrative goes
into the `.org` overview file in Step 9.

Write this JSON to `/tmp/annotations.json`.

---

## Step 6: Backup the Existing Database

Before generating, **always back up** the current database if it exists.
Use the `$DB_PATH` resolved in Step 2:

```bash
cp "$DB_PATH" "$DB_PATH.bak" 2>/dev/null || true
```

This ensures the user can recover if the generation or merge corrupts the file.
If something goes wrong, restore with: `cp "$DB_PATH.bak" "$DB_PATH"`.

---

## Step 7: Generate the Database

Run the elisp helper script (written in Step 3) against `$DB_PATH`. **Pass
the project root as a third argument** so keys are written as project-relative
paths — without this, simply-annotate will fail to find the annotations when
the user opens a file:

```bash
emacs --batch -l /tmp/gen_annotations.el /tmp/annotations.json "$DB_PATH" "$PROJECT_ROOT"
```

Where `$PROJECT_ROOT` is the directory resolved in Step 2 (the one containing
`.simply-annotations.el`). Omit the project-root argument **only** when using
the global database under `~/.emacs.d/`, which keys by absolute path.

The script handles everything:
- **Merging**: files already in the database are replaced; files not in the
  new JSON are preserved untouched.
- **Key rewriting**: when `PROJECT_ROOT` is supplied, absolute paths inside
  that root become project-relative (e.g. `src/index.ts`).
- **Thread format**: each annotation's `text` is a full thread alist with
  `tags`, matching the current simply-annotate on-disk format.
- **Format**: output is written with `prin1`, matching
  `simply-annotate--save-database` format — a raw alist readable by
  `read-from-string`. No `(setq ...)` wrapper.

---

## Step 8: Validate the Database

After generating, **always run the validation script** to confirm the database
is well-formed and won't break simply-annotate-mode. Write the following
**verbatim** to `/tmp/validate_annotations.el` and run it against `$DB_PATH`.

```elisp
;;; validate_annotations.el --- Validate a simply-annotate thread-tag database -*- lexical-binding: t; -*-

;; Usage: emacs --batch -l /tmp/validate_annotations.el $DB_PATH

(require 'cl-lib)

(defun validate-annotations--thread-p (obj)
  "Return non-nil if OBJ looks like a simply-annotate thread alist."
  (and (listp obj)
       (listp (car-safe obj))
       (assq 'id obj)
       (assq 'comments obj)))

(defun validate-annotations--main ()
  "Validate the simply-annotate database file."
  (let* ((db-path (expand-file-name (car (last command-line-args))))
         (errors 0)
         (warnings 0)
         (total-annotations 0)
         (tag-counts (make-hash-table :test 'equal))
         db)

    (unless (file-exists-p db-path)
      (message "FAIL: Database file does not exist: %s" db-path)
      (kill-emacs 1))

    ;; Step 1: Can we read it?
    (condition-case err
        (with-temp-buffer
          (insert-file-contents db-path)
          (setq db (car (read-from-string (string-trim (buffer-string))))))
      (error
       (message "FAIL: Cannot parse database: %s" (error-message-string err))
       (kill-emacs 1)))

    ;; Step 2: Is it a list?
    (unless (listp db)
      (message "FAIL: Database is not a list (got %s)" (type-of db))
      (kill-emacs 1))

    ;; Step 3: Validate each entry
    (dolist (entry db)
      (let ((filepath (car entry))
            (annotations (cdr entry)))

        (unless (stringp filepath)
          (message "ERROR: Entry key is not a string: %S" filepath)
          (cl-incf errors))

        (unless (listp annotations)
          (message "ERROR: Annotations for %s is not a list" filepath)
          (cl-incf errors))

        ;; File-existence check: project-relative keys won't exist as absolute
        ;; paths, so skip the check for non-absolute keys.
        (when (and (stringp filepath)
                   (file-name-absolute-p filepath)
                   (not (file-exists-p filepath)))
          (message "WARNING: File does not exist: %s" filepath)
          (cl-incf warnings))

        (dolist (ann annotations)
          (cl-incf total-annotations)
          (let ((start (alist-get 'start ann))
                (end   (alist-get 'end ann))
                (text  (alist-get 'text ann)))

            (unless (and start end text)
              (message "ERROR: %s: annotation missing required fields: %S" filepath ann)
              (cl-incf errors))

            (when (and start (not (and (integerp start) (> start 0))))
              (message "ERROR: %s: invalid start: %S" filepath start)
              (cl-incf errors))
            (when (and end (not (and (integerp end) (> end 0))))
              (message "ERROR: %s: invalid end: %S" filepath end)
              (cl-incf errors))
            (when (and start end (integerp start) (integerp end) (> start end))
              (message "ERROR: %s: start (%d) > end (%d)" filepath start end)
              (cl-incf errors))

            ;; text must be a string OR a thread alist.
            (unless (or (stringp text)
                        (validate-annotations--thread-p text))
              (message "ERROR: %s: text is neither a string nor a thread alist: %S"
                       filepath text)
              (cl-incf errors))

            ;; Accumulate tag counts for reporting.
            (when (validate-annotations--thread-p text)
              (dolist (tag (alist-get 'tags text))
                (puthash tag (1+ (gethash tag tag-counts 0)) tag-counts)))))))

    ;; Report
    (message "Validation complete: %d files, %d annotations, %d errors, %d warnings"
             (length db) total-annotations errors warnings)
    (when (> (hash-table-count tag-counts) 0)
      (message "Tag breakdown:")
      (maphash (lambda (tag count) (message "  %s: %d" tag count)) tag-counts))

    (if (> errors 0)
        (progn
          (message "FAIL: Database has %d errors. Restore backup with:" errors)
          (message "  cp %s.bak %s" db-path db-path)
          (kill-emacs 1))
      (message "OK: Database is valid.")
      (kill-emacs 0))))

(validate-annotations--main)
```

Run with:

```bash
emacs --batch -l /tmp/validate_annotations.el "$DB_PATH"
```

If validation **fails**, immediately restore the backup:

```bash
cp "$DB_PATH.bak" "$DB_PATH"
```

Then investigate the `/tmp/annotations.json` for the bad data and re-run
generation.

---

## Step 9: Generate Directory Overview Org File

For the annotated directory, create an **org file alongside it** with the same
name as the directory. For example, annotating `/project/java/client` creates
`/project/java/client.org`.

The org file should contain:

```org
#+TITLE: <directory-name> — Directory Overview
#+AUTO_TANGLE: nil

* Purpose

<2-4 sentence overview of what this directory/module does and its role in the
wider project.>

* Structure

<Brief description of how the directory is organised — subdirectories, key
groupings of files, and the relationships between them.>

** <subdirectory-or-grouping-1>

<1-2 sentences on what this group of files does.>

** <subdirectory-or-grouping-2>

...

* Key Files

| File | Description |
|------+-------------|
| file1.java | One-line summary |
| file2.java | One-line summary |
| ... | ... |

List ALL source files in the directory scope, not just the "important" ones.
Group by subdirectory if the directory has subdirectories.

* Important Notes

<Bullet points covering anything a newcomer should know: key patterns used,
auto-generated files to ignore, non-obvious conventions, gotchas, entry points,
important interfaces, etc. Skip this section if there is nothing noteworthy.>
```

### Rules for the org file

- **One org file per annotated directory scope** — if the user annotates
  `java/client`, create `java/client.org`. If they annotate `java/cuis-server`,
  create `java/cuis-server.org`.
- The file sits **alongside** (as a sibling of) the directory, not inside it.
- Keep it concise — this is a quick-reference map, not exhaustive documentation.
- The `Key Files` table and `Structure` section are the **primary home** for
  file-wide narrative (since there is no file-level overlay annotation). Each
  file's one-line summary here is what a file-level annotation used to
  provide.
- If the directory has auto-generated subdirectories (e.g. `gensrc/`), note
  them in Important Notes.

---

## Step 10: Report

After writing, report:
- Number of files annotated
- Total annotations generated (broken down by tag — `defun` and `line`)
- The database file path (clearly state whether it is project-local or global)
- Validation result (pass/fail)
- The directory overview org file path
- Remind the user to open files in Emacs and enable `simply-annotate-mode`
  (or use `simply-annotate-show-all` for the global overview)

---

## Guidelines

- **Be concise**: `defun`-tagged descriptions 1-3 sentences; `line`-tagged
  1 sentence. File-wide overview prose lives in the `.org` overview file
  (2-4 sentences per file in the Key Files table).
- **Be descriptive, not prescriptive**: Describe what the code does, not what
  it should do. This is documentation, not code review.
- **Use plain language**: Avoid jargon where possible. The goal is to help
  someone unfamiliar with the codebase orient themselves.
- **Process files in parallel**: Use the Agent tool to analyse multiple files
  simultaneously when practical — but keep batches reasonable (10-20 files per
  agent).
- **Handle large files**: For files over 1000 lines, you may need to read them
  in chunks. Still produce annotations for the full file.
- **Respect the user's scope**: If they say "just the src/ directory" or "only
  Python files", honour that constraint.
- **Don't annotate the world**: The goal is useful coverage of code the user
  cares about, not exhaustive annotation of every file in the repo.
