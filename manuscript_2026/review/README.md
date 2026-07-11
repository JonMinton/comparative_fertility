# Review workflow for the 2026 manuscripts

Two channels collect feedback on the working papers; one register receives it;
one changelog records what was done. Preprint versions are the unit of
revision (SocArXiv preserves every numbered version under a stable DOI).

## Channel 1 — conversational / text feedback

Comments made in chat sessions (Jon ↔ Claude), emails, LinkedIn messages, OSF
comments, or any other prose. Whoever processes them (usually a Claude
session) logs each distinct point as an entry in [`FEEDBACK.md`](FEEDBACK.md).

## Channel 2 — Word review copies

`*_REVIEW_COPY.docx` files in this directory are rendered from the canonical
`.qmd` sources and carry a reviewer-instructions banner (visible only in the
Word render). Contributors comment via Word's **Comments** and **Track
Changes** and return the file to <jon.will.minton@gmail.com>.

Processing a returned file:

1. Save it to `review/returned/` as
   `<paper>_v<X>_<reviewer-initials>_<date>.docx` (returned files are
   git-ignored by default if they may contain private remarks — commit only
   with the reviewer's consent).
2. Extract the comments and tracked changes (a Claude session can do this
   directly — docx comments live in `word/comments.xml` inside the archive).
3. Log each distinct point as a `FEEDBACK.md` entry, crediting the reviewer.

## From feedback to a new preprint version

1. Triage entries in `FEEDBACK.md` (status: `received → accepted/declined`).
2. Apply accepted changes to the canonical `.qmd` (never to the `.docx` — the
   Word copies are regenerated, not edited).
3. Record what changed in [`REVISIONS.md`](REVISIONS.md) under the target
   version heading, referencing feedback IDs.
4. Re-render (`html`, `docx`, `typst` for the PDF), upload the new PDF as the
   next version on the existing SocArXiv preprint record, and regenerate the
   `_REVIEW_COPY.docx` files with a bumped version in the filename.
5. Update each feedback entry's status to `actioned (vX)`.

Contributors whose input meets authorship criteria are invited onto the author
list for subsequent versions (see the authorship-model notes in the `.qmd`
front matter).
