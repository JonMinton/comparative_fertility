# Feedback register

One entry per distinct point, either channel. Papers: **DF** = descriptive
finding ("The exceptions collapse"); **CC** = corridor article ("The closing
corridor").

Entry template:

```
## F-NNN
- date: YYYY-MM-DD
- source: chat | word-doc | email | linkedin | osf | other (+ who)
- paper: DF | CC | both
- locus: section / figure / line
- comment: <verbatim or faithful summary>
- status: received | accepted | declined | actioned (vX)
- action/response:
```

---

## F-001
- date: 2026-07-11
- source: chat (Jon)
- paper: CC
- locus: Results — "The wall on the surface" / featured figures
- comment: The USA featured-wall panel is only pointed to in the text, not
  embedded. Given it carries the censored-vs-wall-terminated distinction
  (contrast with Norway), consider embedding it as a figure.
- status: accepted
- action/response: planned for v2 — copy `featured_wall_usa.png` into
  `manuscript_2026/figures/`, add figure block + comparative sentence.

## F-002
- date: 2026-07-11
- source: chat (Jon)
- paper: CC (discussion) and/or DF (US section)
- locus: US interpretation
- comment: The USA was historically unusual for a flat-peaked (potentially
  bimodal, subgroup-mixture) ASFR age profile. Is it still exceptional?
  Analysis run in-session: the US plateau (>=9 single-year ages within 90% of
  peak) was sustained 1991–2008 and near-unique in the panel (only Chile
  matches); it collapsed from the left after ~2008 with the teen/early-20s
  fertility decline; by 2024 the US profile is a conventional late single peak
  (width 6 vs panel median 5). Total-schedule analysis cannot distinguish
  subgroup convergence from early-subgroup collapse — birth-order data would.
- status: accepted
- action/response: planned for v2 — add a "vanished plateau" paragraph to the
  CC discussion (and possibly a sentence in DF's US section), noting the link
  to the fading US exception and flagging the birth-order extension as the
  test. Analysis script to be added to `scripts/` for reproducibility before
  the paragraph lands.

## F-003
- date: 2026-07-11
- source: chat (Jon, pre-submission)
- paper: both
- locus: Methods / reproducibility statements
- comment: State that the 2020 findings are reproducible from the frozen
  repository data but not exactly replicable from fresh source downloads,
  since HFD/HFC revise their series.
- status: actioned (v1)
- action/response: added to DF Methods and the repo README before the v1
  preprints were submitted.
