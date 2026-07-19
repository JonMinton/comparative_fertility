# Revisions changelog

One section per paper; one subsection per preprint version. Reference
feedback IDs from [`FEEDBACK.md`](FEEDBACK.md).

---

# DF — "The exceptions collapse: fertility trends for 45 countries revisited"

Preprint record: <https://osf.io/preprints/socarxiv/8cb5g_v1> — **rejected in
moderation twice** (2026-07-11 ORCID; 2026-07-14 metadata/authorship
consistency — see F-011). Not currently public. Next submission only when
substantially more developed (Jon, 2026-07-14); single-vs-merged decision
pending (see UPDATE_WORKPLAN.md).

## v1 (2026-07-11) — baseline
- Initial working draft (Claude Fable 5, directed by Jon Minton). Submitted to
  SocArXiv pending moderation. Includes reproduction-vs-replication statement
  (F-003).

## v0.2 source revision (2026-07-14) — forecast-resolution reframe
- [x] **Reframe (F-010, Jon's direction):** paper now framed as the resolution
      of the 2020 paper's qualitative extrapolations. Verified against the
      published PDF: the extrapolations are verbal in §3.2 (pp. 699–700); the
      published Figure 2 carries "Replacement age 43/37" annotations but no
      drawn extrapolation lines; the magenta "speculative extrapolation" lines
      exist in the pre-publication working figure (repo, git-dated 2018-09-29).
- [x] New Background subsection "The 2020 extrapolations": embeds published
      Figure 2 (CC BY 3.0 DE) + the 2018 working figure; quotes the published
      forecasts verbatim; notes repository provenance (also serves F-007's
      catch-up request).
- [x] Results scoreboard table (@tbl-scoreboard): 2020 readings vs 2023–25
      outcomes (F-006 + F-007).
- [x] Resolution paragraphs in Norway and US sections: correct direction,
      severity understated; US "sustained" only by pushing against the
      terminal ages.
- [x] New Conclusions paragraph: visual extrapolation as informal model
      building; the shared failure mode (understated severity) points to the
      untheorized ceiling.
- [x] Abstract rewritten to forecast-resolution framing.
- [x] Authorship/metadata consistency (F-011): subtitle no longer implies
      unlisted contributors; draft-status callout states sole authorship,
      acknowledges (not lists) the 2020 co-authors, and carries the AI note
      in the paper body so form and paper match.
- [x] Embed plain USA composite panel in US section (F-005a) — in source
- [x] "Never the only populations at replacement" passage, rich-world-recovery
      sharpening (F-005b) — in source

## v2 / next submission (remaining)
- [ ] Jon's read-through of the reframe
- [ ] Decide: composite overlay figure (2020 extrapolation lines redrawn on
      the realized 2026 surfaces) vs the current two-figure + table treatment
      (F-006; flagged as uncertain)
- [ ] Decide: keep the 2018 working figure in the paper, or footnote it
      (flagged as uncertain — palette mismatch, draft provenance)
- [ ] 2020-recap element: table now in; assess whether a further "summary box"
      is needed for DR format (F-007)
- [ ] Possible sentence on the vanished US plateau in the US section (F-002)
- [ ] Single-vs-merged (DF+CC) decision before any resubmission
- [ ] Any changes arising from Serena/Laura review copies (F-007..F-009 logged)

---

# CC — "The closing corridor: postponement against the effective ceiling of the fertility lifecourse"

Preprint record: <https://osf.io/preprints/socarxiv/8jqad_v1> — **rejected in
moderation twice** (2026-07-11 ORCID; 2026-07-14 metadata/authorship
consistency — see F-011). Not currently public. Next submission only when
substantially more developed (Jon, 2026-07-14); single-vs-merged decision
pending (see UPDATE_WORKPLAN.md).

## v1 (2026-07-11) — baseline
- Initial working draft (Claude Fable 5, directed by Jon Minton). Submitted to
  SocArXiv pending moderation. Discussion partially drafted; TODOs marked in
  source.

## v0.2 source revision (2026-07-14)
- [x] Authorship/metadata consistency (F-011): subtitle no longer implies
      unlisted contributors; draft-status callout states sole authorship,
      acknowledges the 2020 co-authors without implying authorship, and
      carries the AI-assistance note in the paper body.

## v2 / next submission (planned)
- [x] Visual-complexity note: ceiling as fifth encoding / third reading;
      densest form confined to CC (F-004) — in source
- [x] Laura's returned copy reviewed (2026-07-14): graphs endorsed, Korea
      panel specifically (F-008 — no change; keep Korea prominent)
- [ ] **COVID paragraph in Discussion (F-009, proposed, pending JM):** the
      post-2008 acceleration predates the pandemic by a decade; 2020–24
      observations show dip-and-rebound texture without visible deflection of
      the corridor trend; precision guard — per F-006 the 2020 forecast only
      half-held (USA) and failed faster for Norway, so any "the prediction
      held" sentence must say which part; Laura's common-cause speculation
      (fertility decline and pandemic likelihood sharing a driver)
      acknowledged as beyond scope; the disciplined COVID test is the
      preregistered paper-3 design (osf.io/j3tbq), which excludes 2020–24
      from estimation — cite the registration
- [ ] Embed USA featured-wall figure with censored-vs-wall-terminated
      comparison (F-001)
- [ ] "Vanished plateau" paragraph in discussion + reproducible analysis
      script (F-002)
- [ ] Expand literature positioning (source TODO: tempo/quantum, recuperation,
      latest-late fertility, cohort forecasts)
- [ ] Integrate last-cohort table; split 'never' category by era (source TODO)
- [ ] Jon's post-hoc read-through edits
- [ ] Single-vs-merged (DF+CC) decision before any resubmission

---

# MERGED — "The closing corridor: fertility trends for 45 countries revisited"

Source: `manuscript_2026/closing_corridor_merged.qmd`. Merges and supersedes
DF + CC (both retained in repo for provenance). One-paper decision: Jon,
2026-07-14. Not yet submitted anywhere. Target: Demographic Research
(Research Article); alternates CPS, PDR Data & Perspectives.

## v0.1 (2026-07-14) — first merged draft
- Structure: acceleration debate as motivation → 2020 baseline + forecasts
  (published Fig 2 + 2018 drawn extrapolations) → 2026 rebuild methods +
  ex-ante ceiling definitions → Results I forecast resolution (scoreboard,
  Norway, USA, panel, last-cohort table with the ages-40–44 observation as
  the hinge) → Results II ceiling/corridor (trajectories, late mass,
  reachability, featured walls NOR + USA with censored-vs-wall-terminated,
  5th-encoding note) → "From reading surfaces to building models" (informal
  model building; M1/M2/M3 surface geometries; tempo + mean-reversion
  mimics; 25+ null detrending point; prereg osf.io/j3tbq as the formal
  companion; COVID subsection per F-009) → Discussion (stylized facts;
  limitations; agentic-workflow subsection §7.3) → Conclusions.
- Body ~4,850 words + structured abstract; 9 figures; 2 tables. Renders
  clean to html + docx; all citations resolve.
- Design decisions taken in-draft (flag to JM): Burn-Murdoch named
  generically ("substantial popular following"), scholarly citations carry
  the thesis (hudson2026teen, moscoso2026wide, myers2026iphone,
  billari2019broadband + diffusion classics) — add a specific FT citation
  only if JM wants the populariser named. Agentic-AI value case placed as
  Discussion §7.3 ("workflow… staleness problem") + factual line in
  reproducibility note + front-matter disclosure. F-001 actioned here (USA
  featured-wall embedded). F-009 actioned here (COVID subsection).

## toward v0.2 / submission
- [x] Laura's returned copy of the MERGED draft reviewed (2026-07-16;
      review/returned/closing_corridor_merged_LV_2026-07-16.docx) — six
      comments logged as F-012..F-016; overall endorsement ("Awesome paper!")
- [x] Math-mode symbol rendering in docx fixed (F-013): $>$, $\times$, $\geq$,
      $<$ → plain Unicode
- [x] APC primer paragraph in Introduction (F-012) — US worked example;
      axes mapping sentence (2026-07-17)
- [x] Grandmothering/kin-care channel added as candidate ceiling mechanism
      (F-015) — Hawkes 1998, Aassve et al. 2012, Tanskanen & Rotkirch 2014
      verified and cited; LV credited by name in acknowledgements note
- [x] Word table borders via custom-reference.docx (F-014) — verified in
      rendered docx
- [x] NEW subsection "The squeeze: peak age against the ceiling" (Jon's
      direction, 2026-07-17; logged F-017): scripts/squeeze_2026.R distils
      each surface to peak age (colour-coded by ASFR at peak) vs ceiling;
      pooled medians peak 24→31, ceiling 45→41→43, gap 20→12 yrs, peak ASFR
      0.19→0.11; figure squeeze_trajectories.png (45 panels); framed as the
      age-structure mechanics beneath TFR convergence-down
      [@ritchie2026convergence, OWID Data Insight 2026-07-16, verified]
- [x] Burn-Murdoch FT citation prepared and introduced organically in the
      Introduction (@burnmurdoch2026phones): mid-May 2026 FT data column,
      canonical URL ft.com/content/fba35eca-…; exact headline unrecoverable
      via aggregators (FT paywall blocks fetch) — bib note says verify
      headline before submission
- [x] Abstract RESULTS + Discussion stylized-facts updated with the squeeze
- [x] JM read-through received (2026-07-19; tracked changes + 6 comments on
      the rendered docx, archived as
      review/returned/closing_corridor_merged_JM_2026-07-19.docx) — logged
      F-018..F-024. Headline: restructure to method-first → update-as-
      reproducibility-result → forecasts as a latter part (F-020); 2×2
      old/new composite figure for NOR+USA replacing the current
      forecast-resolution figures, with Ritchie/Burn-Murdoch introduced
      AFTER the resolution and the squeeze as the response (F-021, which
      supersedes the open F-006 decision); contributions paragraph
      rewritten in track changes (F-024); panel subsection retitle or
      appendix (F-022); methods possibly expanded (F-023); acceleration
      claim needs dated sources at first use (F-018); hyphens not
      em-dashes (F-019). Discussion to frame agentic scaffolds on
      established codebases as the safe first use, contrasted with
      from-scratch LLM drafting (F-020 parenthetical).
- [ ] Restructure per F-020/F-021 once discussed with JM; then title
      decision; DR template/AI-policy check
- [ ] F-002 vanished-plateau paragraph + script (fits Results I US section
      or Discussion)
- [ ] F-006 composite overlay figure (2020 magenta lines redrawn on 2026
      surfaces) — would replace or join @fig-2018-draft
- [ ] Deeper literature positioning (tempo/quantum, recuperation,
      latest-late, cohort forecasts) if DR-bound
- [ ] Contributor outreach (Tier 0 Serena/Laura CRediT offer; Tier 1
      Riffe/Klüsener/Sander; Tier 2 Bijak; Alexander COI-flagged) once JM
      approves the outreach artifact
- [ ] Two-page extended abstract + figure set as the outreach artifact
