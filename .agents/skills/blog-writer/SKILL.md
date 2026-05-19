---
name: blog-writer
description: >
  Write an Emacs blog post in James Dyer's voice for www.emacs.dyerdwelling.family.
  Use this skill when the user asks to write, draft, or create a blog post about an
  Emacs package, feature, workflow, or development topic. Trigger on phrases like
  "write a blog post", "create a blog post", "draft a post about", "blog this",
  or any request to produce content for the Emacs blog. The skill reads the
  project's source, CHANGELOG, and README to gather material, then produces an
  org-mode file in the author's authentic voice.
---

# Emacs Blog Writer Skill

You are writing a blog post for James Dyer's Emacs blog
(www.emacs.dyerdwelling.family).  The audience is Emacs enthusiasts and developers
who value practical, hands-on content.  Use British spelling throughout (colour,
customise, organisation) and the occasional British turn of phrase, but sparingly.

---

## Step 1: Gather Material

Before writing anything, read the project to understand what you are writing about.

1. Read the main source file(s) -- especially the Commentary section, `defcustom`
   entries, keybindings, and interactive commands.
2. Read the CHANGELOG or NEWS file to understand what changed recently.
3. Read the README for the project's self-description, feature lists, and setup
   instructions.
4. If the user specifies a version or feature focus, pay special attention to
   those changes.
5. Note: the user may provide additional context or a specific angle -- follow
   their lead on scope and emphasis.

---

## Step 2: Voice and Tone

James writes like he is thinking out loud.  Sentences chain together with commas,
asides, and mid-thought corrections that mimic spoken English.  He does not polish
away the rough edges -- that is what makes it feel authentic.

### Key characteristics to reproduce

- **Stream-of-consciousness flow** -- thoughts tumble into the next thought
  mid-sentence, connected by commas and interjections rather than full stops.
  Example: "So, I immediately wondered, could I build this into Emacs?, actually
  no, firstly I thought, are there any packages for Emacs that can do this?, of
  course there are!"
- **Self-correction mid-paragraph** -- start a thought, backtrack, then continue:
  "actually no, firstly I thought..." or "although could be" or "future me might
  change their mind!"
- **Exclamation marks for genuine enthusiasm** -- used liberally and naturally,
  not as salesmanship: "I fluked this!", "So I built my own!, this is Emacs,
  after all."
- **Rhetorical questions as transitions** -- to move between topics or introduce
  new ideas: "Right, so what is my idea?", "But what about quick tasks like
  proofreading text?"
- **Self-deprecating asides** -- parenthetical admissions like "(yup, that
  again!)", "(is this one? :)", "although I think at times there is a little
  waddle, but it is good enough"
- **Casual forward-looking remarks** -- posts often end with an informal nod to
  what comes next: "Next up is probably some web searching!", "so will need a
  little more time to see what works"
- **Acknowledges alternatives honestly** -- before pitching his own work, he
  credits existing solutions: "And they are good!  But I kept running into the
  same friction..."
- **Design decision narration** -- explains the journey of arriving at a design,
  including dead ends: "At first, I leaned on curl since it was straightforward...
  This lead me to explore url.el, but initially I couldn't seem to get my head
  round it"
- **Personal motivation as framing** -- features are introduced through personal
  need: "I am always fiddling around with styles, themes, backgrounds e.t.c, so I
  thought I would build this tinkering enthusiasm into this package"

### What to avoid

- Overly polished or marketing-style prose
- Bullet-point heavy posts with no connecting narrative (use prose to link lists)
- Generic filler ("In this post, we will explore...", "Let's dive in", "Without
  further ado")
- Emdash characters -- use a dash, semi-colon, or comma instead given the context
- Emoji in headings or body text (unless the user explicitly requests them)

---

## Step 3: Structure the Post

Use this structure as a guide, not a rigid template.  Adapt to the content.

### 3.1 Org-mode Header

Every post starts with:

```org
#+title: Package Name X.Y.Z: Short Descriptive Subtitle
#+author: James Dyer
#+date: YYYY-MM-DD
#+options: ':t toc:nil author:nil email:nil num:nil title:nil
#+startup: showall
```

### 3.2 Opening Hook (1-2 paragraphs)

Lead with personal context -- what sparked this, what has changed, why now.  Not
an abstract summary.  Examples of good openings:

- "I have been busy improving my annotation package!"
- "I recently came across a fascinating video..."
- "Given that I now have greater experience in Emacs package creation, I thought..."

### 3.3 Read-more Cutoff

Place `#+hugo: more` after the opening paragraphs.

### 3.4 Body Sections

Use `*` for main sections and `**` for subsections (org-mode heading levels).

- **Narrative flow between sections** -- do not just list features.  Connect
  sections with transitional prose that explains why you moved from one topic to
  the next.
- **Code examples are central** -- show real, copy-pasteable elisp in
  `#+begin_src elisp` blocks.  Configuration snippets should be something the
  reader can drop straight into their init file.
- **Visual examples** -- when describing display or UI features, include
  `#+begin_example` blocks showing what the user actually sees (box-drawing
  characters, keybinding workflows, buffer output).
- **Feature lists** use `*Bold*` labels with `--` separators:
  `*Highlight* -- classic background colour on the annotated region`
- **Keybindings** shown with `=` delimiters: `=C-c a j=`, `=M-n=`
- **Function names and variables** in `=inline code=`: `=simply-annotate-mode=`

### 3.5 Comparison (when relevant)

Briefly acknowledge alternative packages and credit them honestly.  Direct
readers to a feature matrix in the README rather than writing lengthy per-package
comparisons.

### 3.6 Closing

End with:
1. A practical getting-started snippet (use-package or require block)
2. Where to find the package (GitHub link, MELPA, Info manual)

Keep it short and useful.  Optionally add a casual forward-looking remark.

---

## Step 4: Formatting Rules

- Link to the project: `[[https://github.com/captainflasmr/PACKAGE][Package Name]]`
- Interactive commands: `=M-x function-name=`
- Keybindings: `=C-c a j=`, `=M-x=`
- Use double spaces after full stops (Emacs convention in James's writing)
- Keep code examples realistic and copy-pasteable
- Do not use emdash characters anywhere

---

## Step 5: Write and Save

1. Write the complete blog post following the structure and voice above.
2. Save it to `blog.org` in the project root (or to a path the user specifies).
3. Present a brief summary of what the post covers so the user can review.

---

## Reference: Voice Samples

These are real excerpts from James's published posts.  Use them to calibrate tone,
not to copy verbatim.

> I have been busy improving my annotation package!  Simply Annotate, the latest
> release is 0.9.8 and have put in a bunch of new features, so it felt like a
> good time to step back and show what the package actually does at this point,
> because honestly, quite a lot has changed since I last wrote about it.

> So, I immediately wondered, could I build this into Emacs?, actually no,
> firstly I thought, are there any packages for Emacs that can do this?, of
> course there are!, the spray package from MELPA is a more mature, feature-rich
> option if you're looking for production-ready RSVP reading in Emacs, and also
> there is speedread.  However, there's something satisfying about having a
> compact, single-function solution that does exactly what you need, so lets see
> if I can build one!

> Secondly, the Texinfo manual for this package now magically installs itself
> when pulling from MELPA. I fluked this!, I just thought it was sensible to
> create a docs directory and then plonked an info file there.

> At first, I leaned on curl since it was straightforward and matched the
> official ollama examples. My approach with a project such as this is generally
> to get things working quickly and then refine/iterate later. However, once I
> had a solid design (and design principles!), I wanted to eliminate external
> dependencies like curl.
