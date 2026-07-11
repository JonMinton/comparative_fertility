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

## F-004
- date: 2026-07-11
- source: chat (Jon, via forked side-question)
- paper: both (decision affects both)
- locus: figure design — the ceiling overlay
- comment: The dotted ceiling line is a fifth graphing dimension in the
  grammar-of-graphics sense (x, y, hue, contour weight were already four).
  4D was plausibly already beyond most researchers' comfort — a likely factor
  in the 2020 paper's limited uptake — so a fifth encoding needs care re:
  reach/accessibility. Verdict: relevant to CC; too much for DF.
- status: actioned (in source, ships with v2)
- action/response: CC gains a "note on visual complexity" paragraph framing
  the ceiling as a fifth encoding / third reading of one field and stating
  the deliberate confinement to CC. DF stays four-encoding throughout (its
  new USA panel is the plain composite, no ceiling).

## F-005
- date: 2026-07-11
- source: chat (Jon)
- paper: DF
- locus: Results — US section and panel-as-a-whole
- comment: (a) If DF shows the Norway panel it should show the USA panel too.
  (b) Note that Norway and the USA were never the only populations at
  replacement — only the notably rich ones whose cohort fertility RECOVERED;
  others (Iceland, France, NZ, N. Ireland ~1980 cohorts; Macedonia 1974,
  Albania 1973) also crossed.
- status: actioned (in source, ships with v2)
- action/response: plain `usa_only.png` generated and embedded in the US
  section; "never the only populations" passage added with crossing cohorts
  from data/derived_2026/last_cohort_replacement.csv, sharpening what the
  2020 exception claim actually was (rich-world recovery).
