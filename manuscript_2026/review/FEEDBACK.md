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

## F-006
- date: 2026-07-12
- source: chat (Jon, during Zoom call with Laura Vanderbloemen)
- paper: DF (primarily; CC framing benefits too)
- locus: figures / framing
- comment: The 2020-era annotated figure `figures/usanor_annotated_two_contour.png`
  contains labelled "speculative extrapolation of replacement contour" lines
  (magenta dotted) — Norway continuing at ~age 43, USA at ~37 — i.e., the
  published paper DREW its forecast. The update should exploit this:
  (a) a "prediction vs outcome" figure overlaying the 2020 extrapolations with
  the realized 2026 contours (Norway: failed immediately — vertical escape at
  cohorts >~1972; USA: half-right — crossings continued but at 39-40, drifting
  toward the wall, censored beyond 1985); (b) reframe the update as the
  RESOLUTION of an explicitly drawn forecast, not merely newer data. Note also
  the 2020 annotation "Replacement age 43" = the corridor paper's ceiling,
  annotated in plain sight.
- status: accepted
- action/response: planned for v2 — rebuild the overlay figure (old two-contour
  panels + magenta extrapolation + 2026 realized contours), add framing
  sentence to DF intro/conclusions; CC discussion may cite the 2020 annotation
  as the ceiling's first (untheorized) appearance.

## F-007
- date: 2026-07-14
- source: word-doc (Laura Vanderbloemen, returned copy in review/returned/)
- paper: DF
- locus: anchored at Figure 3 caption (fifteen-panel figure)
- comment: (verbatim) "awesome, everything is very clear. Up to this point.
  Just occurred to me after reading that a little bit more context from the
  2020 paper might be worthwhile kind of like a little summary box of key
  pts, just for helping the reader to catch up w the complexity"
- status: received
- action/response: to be discussed with JM before any edit. Candidate: a
  compact "the 2020 baseline in brief" element placed before the panel-wide
  results — possibly as a small claims-then-vs-now table, which would also
  serve F-006's prediction-vs-outcome reframe. DR format constraints on
  text boxes to be checked (a table survives typesetting; a box may not).

## F-008
- date: 2026-07-14
- source: word-doc (Laura Vanderbloemen)
- paper: CC
- locus: figures (featured surfaces)
- comment: (verbatim) "the graphs look really great! Nice to see the Korea data"
- status: received
- action/response: no change required; confirms keeping the Korea featured
  panel prominent.

## F-009
- date: 2026-07-14
- source: word-doc (Laura Vanderbloemen)
- paper: CC (discussion; touches DF framing via F-006)
- locus: discussion — COVID and the 2020 prediction
- comment: (verbatim) "also, interesting that your prediction held back in
  2020, even though you wouldn't have known the extent of Covid at that
  time, which maybe suggests that Covid didn't move the trends though that
  does seem kind of surprising, or maybe Covid was part of a broader trend
  that affected, both (fert & pandemic likelihood)?"
- status: received
- action/response: to be discussed with JM before any edit. Candidate: short
  CC-discussion paragraph — the post-2008 acceleration predates the pandemic
  by a decade; pandemic-era observations (2020–24) show dip-and-rebound
  texture without visible deflection of the corridor trend; the disciplined
  test lives in the preregistered paper-3 design (osf.io/j3tbq), which
  excludes COVID years from estimation. Laura's common-cause speculation
  acknowledged as beyond scope (light-not-heat rule). Precision guard: per
  F-006 the 2020 prediction only half-held (USA), and failed for Norway —
  any "prediction held" sentence must say which part.

## F-010
- date: 2026-07-14
- source: chat (Jon)
- paper: DF ("the main paper")
- locus: whole-paper framing
- comment: Reframe DF around the qualitative extrapolations the 2020 paper
  made for the USA and Norway — correct in direction, but the realized
  declines were more severe than the visual extrapolation suggested. Include
  the existing 2020 figures, then the updates, highlighting the visual
  method's value for "informal model building" about demographic structure
  and fertility trends. Compatible with F-007.
- status: actioned (in source, 2026-07-14)
- action/response: Verification first (Jon's precondition): the published
  paper's Figure 2 is annotated ("Replacement age 43/37") but contains NO
  drawn extrapolation lines; the extrapolations are VERBAL in §3.2
  (pp. 699–700); the magenta "speculative extrapolation of replacement
  contour" lines exist in the pre-publication working figure, git-dated
  2018-09-29, public on GitHub (cited by the published paper). Reframe
  implemented with that honest provenance: new Background subsection with
  both figures and verbatim quotes; scoreboard table; resolution paragraphs;
  informal-model-building conclusion; abstract rewrite. See REVISIONS.md
  v0.2 entry.

## F-011
- date: 2026-07-14
- source: osf (SocArXiv moderator, Philip N. Cohen — identical wording on
  both papers)
- paper: both
- locus: submission metadata / author block
- comment: (verbatim) "The 'note' is in the metadata form, but not in the
  paper. The unconsenting co-authors are not listed. This is irregular. We
  only accept complete papers with consistent metadata that matches across
  forms and the paper."
- status: accepted
- action/response: DF source fixed 2026-07-14: subtitle no longer says
  "not yet reviewed by all listed contributors"; draft-status callout states
  sole authorship explicitly, acknowledges the 2020 co-authors without
  implying authorship, adds "with consent" to the co-authorship offer, and
  carries the AI-assistance note in the paper body. Same treatment TO DO for
  CC. Strategic response (Jon): no resubmission until the manuscript is
  substantially more developed; single-vs-merged decision first.

## F-012
- date: 2026-07-16
- source: word-doc (Laura Vanderbloemen, returned merged-draft copy in
  review/returned/)
- paper: MERGED
- locus: Introduction
- comment: (verbatim) "perhaps at some point good to remember/remind about
  the distinction among age, period and cohort effects, how they differ, and
  why it matters to be able to distinguish among them. Maybe an example of an
  application in which not distinguishing among them, would cause an error in
  judgement, or or an example, that shows how distinguishing among them can
  help refine our understanding of populations, and improve prediction"
- status: actioned (in source, 2026-07-17)
- action/response: APC primer paragraph added to the merged Introduction,
  with the US as the worked example (period accounting misdates the end of
  the US exception by ~two decades) and a closing line mapping age/cohort/
  period onto the surface axes.

## F-013
- date: 2026-07-16
- source: word-doc (Laura Vanderbloemen)
- paper: MERGED
- locus: Methods — "Do these arrows need help" (combination-rules para);
  "x doesn't look right" (visual-spec para)
- comment: math-mode symbols ($>$, $\times$) render poorly in the docx
  channel.
- status: actioned (in source, 2026-07-16 comments; fixed 2026-07-17)
- action/response: replaced inline math-mode $>$ and $\times$ with plain
  Unicode (">", "×") in the merged source; re-rendered docx/html.

## F-014
- date: 2026-07-16
- source: word-doc (Laura Vanderbloemen)
- paper: MERGED
- locus: Table 1
- comment: (verbatim) "I think this table might look better with borders"
- status: actioned (2026-07-17)
- action/response: custom-reference.docx generated (pandoc default + borders
  injected into the "Table" style); merged qmd docx format now uses it;
  verified the rendered docx references the bordered style.

## F-015
- date: 2026-07-16
- source: word-doc (Laura Vanderbloemen)
- paper: MERGED
- locus: Results II — ceiling decade medians
- comment: (verbatim) "another driver might possibly have been 40-ish,
  cohorts, becoming grandmothers in contexts where Mum is employed outside
  home & childcare was scarce, and therefore culturally became more likely to
  help their own daughters with childcare, rather than continue with
  fertility until later ages of their 40s?"
- status: received
- action/response: substantive candidate mechanism for WHY the ceiling fell
  and stays low — an intergenerational-childcare/grandmothering channel
  (kin substitution: potential late-fertility years reallocated to
  grandparental care where maternal employment is high and formal childcare
  scarce). Proposed (pending JM): add to the ceiling discussion as a
  candidate behavioural mechanism alongside parity control, with literature
  check (grandparental childcare & daughters' fertility; grandmother
  hypothesis). NOTE: this is the kind of substantive contribution the
  contributorship→authorship route was designed for — strengthens the CRediT
  case for LV. ACTIONED (in source, 2026-07-17): "Why did the ceiling fall,
  and why does it stay low?" paragraph added to the ceiling subsection —
  parity control + the grandmothering/kin-care channel, cited to Hawkes et
  al. 1998 (long post-reproductive helping span), Aassve/Meroni/Pronzato
  2012 (EJP; grandparental childcare raises daughters' fertility) and
  Tanskanen & Rotkirch 2014 (DemRes; fertility intentions), with a testable
  signature flagged (ceiling recovery slowest where grandparental care
  substitutes for formal provision). LV credited by name in the
  reproducibility/acknowledgements note. Status: actioned (in source).

## F-016
- date: 2026-07-16
- source: word-doc (Laura Vanderbloemen)
- paper: MERGED
- locus: Conclusions
- comment: (verbatim) "Awesome paper! I really like the way the graphs are
  incorporated with the text and explanations are very clear and nicely tied
  with the graphs."
- status: received
- action/response: no change; endorsement of the merged structure and
  figure-text integration.

## F-017
- date: 2026-07-17
- source: chat (Jon)
- paper: MERGED
- locus: Results II — new subsection
- comment: Combine the age from which future contributions to completed
  cohort fertility become very small (the effective ceiling) with the age of
  peak fertility, showing a "squeeze": peak age rising, minimal-fertility
  boundary lowering. Colour-code peak age by the age-year-specific fertility
  rate — a simplification/distillation of the Lexis surfaces. Tie to the
  OWID/Ritchie TFR convergence-down piece: our added value is the means by
  which the levelling-down occurred.
- status: actioned (in source, 2026-07-17)
- action/response: scripts/squeeze_2026.R + derived CSV + 45-panel figure;
  "The squeeze" subsection in Results II; abstract and Discussion updated;
  Ritchie (OWID, 2026-07-16) cited in Introduction, squeeze subsection, and
  stylized-facts paragraph. Caveats recorded: ties-to-youngest modal ages
  (US plateau, F-002), small-population noise, per-country panels primary.

## F-018
- date: 2026-07-19
- source: word-doc (Jon; tracked changes + comments on the rendered merged
  docx, archived as review/returned/closing_corridor_merged_JM_2026-07-19.docx)
- paper: MERGED
- locus: Introduction, opening paragraph ("Fertility decline appears to have
  accelerated… turned more steeply downward at some point after the late
  2000s")
- comment: "Since when? Sources?" — the acceleration claim needs dating and
  citation support at the point it is first made.
- status: received
- action/response: pending discussion. Candidate anchors already in the bib:
  ritchie2026convergence (OWiD), burnmurdoch2026phones (FT), plus the
  scholarly acceleration citations in §6; question is which to surface in
  the opening paragraph vs later per the F-021 restructure.

## F-019
- date: 2026-07-19
- source: word-doc (Jon)
- paper: MERGED
- locus: global style
- comment: "hypens not em-dashes throughout".
- status: received
- action/response: pending — mechanical find/replace in the qmd once the
  restructure settles (em-dashes → hyphens/spaced dashes per Jon's
  preference).

## F-020
- date: 2026-07-19
- source: word-doc (Jon; comment on §2 heading "The 2020 baseline and its
  forecasts")
- paper: MERGED
- locus: overall structure
- comment: §2 "maybe needs placing a bit later. We are combining two aims at
  once": (1) first briefly reintroduce the visualisation method and how to
  read it; (2) then show that the earlier results could be successfully
  updated because code, algorithm and sources were all reproducible; (3) the
  informal forecasts embedded in the 2020 pedagogic figures then form a
  specific LATTER part of the manuscript. Parenthetical for the Discussion:
  present agentic scaffolds + frontier models for replication/update work on
  ALREADY-ESTABLISHED codebases as a safe and defensible first use of
  agentic AI in coding, contrasted with from-scratch LLM paper development
  where the risk of neither understanding nor verifying method/claims is
  much greater.
- status: received
- action/response: pending discussion — this is the spine of the proposed
  restructure (with F-021); §7.3 gains the safe-first-use vs from-scratch
  contrast.

## F-021
- date: 2026-07-19
- source: word-doc (Jon; comment on Results I "Norway" heading)
- paper: MERGED
- locus: Results I — figure architecture and narrative order
- comment: Simplify and merge the existing forecast-resolution figures into
  one 2×2 composite (leaning 2×2 over 3×2): columns = old-with-projection
  vs updated; rows = Norway, USA. The annotated 2020 figures do double duty
  — reintroducing how to read the plots AND stating the falsifiable claims
  the new data verify/falsify. The scoreboard table + conclusion follow the
  composite. THEN a passage on why Norway lost replacement earlier and USA
  declines were more severe than projected, which is the point where
  Ritchie, Burn-Murdoch and related theses get introduced — with the
  squeeze diagrams presented as responses to them.
- status: received
- action/response: pending discussion. Supersedes the open F-006 decision
  (overlay vs two-figure treatment): neither — a 2×2 old/new grid. Also
  relocates the debate-facing material from the Introduction to a
  post-resolution position.

## F-022
- date: 2026-07-19
- source: word-doc (Jon; comment on "The panel as a whole" heading)
- paper: MERGED
- locus: Results I — panel subsection
- comment: "This isn't the panel as a whole. We have more populations."
  Either justify in-text why only these figures appear in the manuscript,
  and/or move panel-scale figures to an appendix/supplement referred to
  parenthetically.
- status: received
- action/response: pending discussion — interacts with DR's format (DR
  handles supplementary materials well; the Shiny app is the natural
  full-panel home and is already cited).

## F-023
- date: 2026-07-19
- source: word-doc (Jon; comment on the shared-failure-mode paragraph)
- paper: MERGED
- locus: Methods vs Results boundary
- comment: "Consider if we need the methods to be more clearly defined. If
  so should we have more in methods?" — anchored where the ceiling concept
  does interpretive work in Results.
- status: received
- action/response: pending discussion — candidate moves: ceiling/corridor
  operational definitions fully into §3 Methods, leaving Results to apply
  them; squeeze construction (peak age, ties-to-youngest, colour encoding)
  likewise.

## F-024
- date: 2026-07-19
- source: word-doc (Jon; tracked rewrite of the Introduction roadmap
  paragraph "We proceed in three movements…")
- paper: MERGED
- locus: Introduction — contributions statement
- comment: Rewritten in tracked changes to "This paper makes three
  contributions in the following sequence" then enumerates FOUR: (1) update
  the 2020 visualization paper with new data, noting it included qualitative
  forecasts for Norway and the USA; (2) compare those projections with the
  updated data — both correct (Norway lost replacement, USA did not),
  though declines greater than projected in either case; (3) formalize the
  ceiling intuition (age beyond which future contribution to completed
  cohort fertility is marginal); (4) the squeeze — ceiling falling while
  the age of peak fertility ("midpoint") rises. The DELETED text is the old
  three-movements framing, including the "bridge" movement (surface
  visualization as informal model building; M1/M2/M3; the preregistered
  protocol as formal companion) — no longer billed as a headline
  contribution. Small tracked insertions elsewhere: "(TFRs)" and "(OWiD)"
  abbreviations at first use.
- status: received
- action/response: pending discussion. Two flags for Jon: (a) "three
  contributions" vs four enumerated; (b) the USA claim — per the F-009
  precision guard, "the USA would not [lose replacement]" only half-held
  (last cohort to reach replacement at 40 is 1984; retention was achieved by
  pushing against the terminal ages), so the resolution sentence should say
  which part held. Also to decide: what §6 becomes if the model-building
  bridge is no longer a headline contribution (fold M1/M2/M3 + prereg into
  Discussion?).
