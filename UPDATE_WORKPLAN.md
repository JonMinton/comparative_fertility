# Workplan: Updating the Comparative Fertility Analysis

*Drafted 2026-07-11 (Claude Fable 5 session). Timebox: ~2 human hours this weekend.*
*This file is self-contained: all findings and decisions from the planning session are captured here so any future session (or human) can pick it up cold.*

---

## ⚡ STATE OF PLAY (handover, 2026-07-19)

**The live manuscript is `manuscript_2026/closing_corridor_merged.qmd`**
("The closing corridor: fertility trends for 45 countries revisited") —
one merged paper per Jon's decision; DF and CC qmds are superseded but
retained for provenance. Renders to html + docx (docx uses
`custom-reference.docx` for bordered tables). Feedback register
`manuscript_2026/review/FEEDBACK.md` runs F-001..F-025; changelog in
`review/REVISIONS.md` (see its MERGED section for the to-do list).

**2026-07-19: v0.2 restructure EXECUTED** per Jon's three-artifact spine
directive (F-025) and same-day call answers (F-026/F-027; design record in
`manuscript_2026/RESTRUCTURE_PLAN.md`). The MS now runs: method + 3D
terrain (fig lexis3d_ridge_norway) + why-uptake-was-limited → Methods with
formal AAPF/ANR algebra → forecast check (2×2 old/new composite,
`forecast_check_2x2.png`, left column re-rendered from tag `demres-2020`
data with the 2018 extrapolations redrawn) → beyond-TFR → the shortening
runway (pooled squeeze chart) → Discussion (incl. safe-first-use agentic
framing) → Conclusions. Terminology: **ANR** (age of no return) and
**AAPF** (age and amount of peak fertility); usable runway 21→12 yrs.
Title PROVISIONAL: "Running out of runway: fertility trends for 45
countries revisited" — awaiting Jon. Supplement `supplementary_materials.qmd`
S1–S8 (S8 closes F-002: US plateau 1992–2006, 13 plateau years, longest in
panel). Both render clean (html+docx). New scripts:
forecast_check_2x2_2026.R, squeeze_pooled_2026.R, lexis_3d_ridge_2026.R,
us_plateau_2026.R. To-do list: review/REVISIONS.md "toward v0.3".
NOTE: create a pre-submission archive tag at submission (F-026 condition).

Done and in the merged draft: forecast-resolution framing (published Fig 2
+ 2018 drawn extrapolations, both verified against the DemRes 42-23 PDF);
ceiling/corridor results; **the squeeze** (scripts/squeeze_2026.R — peak
age 24→31 vs ceiling 45→41→43, gap 20→12 yrs, peak ASFR 0.19→0.11;
45-panel figure); APC primer (F-012); grandmothering ceiling mechanism
(F-015, Laura's suggestion, credited by name — CRediT case); visualization
→ modelling bridge citing prereg osf.io/j3tbq; COVID subsection; agentic-
workflow discussion §7.3; Ritchie OWID convergence insight (2026-07-16)
cited as the TFR-only foil; Burn-Murdoch FT column (May 2026) cited —
**exact FT headline still to verify** (paywall; bib note marks it).

Status elsewhere: paper-3 preregistration REGISTERED and public
(osf.io/j3tbq; WPP-2024 download now permitted, not yet done). SocArXiv:
both v1 preprints rejected twice (final reason = metadata/authorship
consistency, F-011 — fixed in sources); strategy = no resubmission until
the merged paper is complete, then submit fresh. Shiny app redeployed with
2026 data. ORCID/OSF linked and populated.

Next actions (in rough order): Jon's read-through of the merged draft →
his framing calls (title; keep 2018 draft figure vs F-006 overlay rebuild;
Burn-Murdoch named or generic) → remaining content items F-006 (overlay
figure) and F-002 (US vanished-plateau paragraph + script) → contributor
outreach (Tier 0: Serena/Laura CRediT offers; Tier 1: Riffe/Klüsener/
Sander; Tier 2: Bijak; NOTE Monica Alexander is a current DR associate
editor = COI-flagged; do NOT approach EiC Matysiak) via a 2-page extended
abstract + figure set → DR template + AI-policy check → SocArXiv fresh
submission when genuinely complete. Paper 3 (WPP pull) can proceed in
parallel any time.

---

## Semaphore key

- 🔴 **Jon must do this** — blocks everything downstream; cannot be automated
- 🟡 **Jon decides** — a default is proposed; overriding takes one line in a prompt
- 🟢 **Automatable** — Claude does this; no human time needed beyond review

---

## 🔴 Actions for Jon (do these first — ~30–40 min total)

1. **[ ] Re-register / log in at the HFD** — <https://www.humanfertility.org> (Account → Registration).
   The site was rebuilt since the original 2015–16 downloads; assume the old login is dead.
   Registration is free.
2. **[x] ~~Download HFD bulk data~~ — DONE 2026-07-11.** Jon downloaded the
   "all types of HFD data" bundle (`HFD.zip`, build of 2026-07-02); relocated and
   extracted to `data/hfd_2026/` (git-ignored — raw HFD data should not be committed).
   Verified contents: 39 populations; `asfrRR.txt` present (whitespace-delimited,
   `Code Year Age ASFR`, age codes `12-`/`55+` need parsing); also `birthsRR`/`exposRR`,
   and precomputed `cpfrRR.txt` + `ccfrVH.txt` (useful for validating our own CPCFR).
   Coverage highlights: NOR 1967–2024, USA 1933–2024, **KOR 2000–2024** (repo previously
   stopped 2007), DNK to 2025; laggards UKR (2013), SVK (2014), DEU* (2017).
   Sanity checks passed: NOR 2009 TFR 1.98 → 2024 1.44; USA 2007 2.12 → 2024 1.59;
   KOR 2024 0.75.
3. **[x] ~~Download refreshed HFC file~~ — DONE 2026-07-11.** `HFC_ASFRstand_TOT`
   (build 2026-07-07) and `HFC_ASFRstand_BO` (by birth order, for possible parity
   analysis in Phase 3) extracted to `data/hfc_2026/` (git-ignored).
   Format matches the 2016 combine script's expectations (same columns; new bonus
   `CPFR` column). Coverage of the paper's HFC-only countries after the published
   filters (ACY, STAT/ODE/RE, single-year):
   - **Extended well**: AUS →2024, BIH →2024, MDA →2024, MKD →2024, NZL →2025, TUR →2024
   - **Still stale (ragged edge)**: ALB 2008, GRC 2009, LVA 2009, ROU 2013, RKS 1987
   - **⚠ FRANCE FIX**: old combined data had France ending 2008 because it came from
     HFC code `FRA`, while HFD covers France as `FRATNP` (1946–2023). The 2026 ingest
     must map `FRATNP → FRA` in `code_definitions.csv` — recovers 15 years for France
     from the higher-quality source. Audit all HFD composite codes for similar
     mismatches (e.g., new HFD members HRV/KOR/JPN/TWN vs the old code list).
4. **[ ] (Optional, Phase 2 only) shinyapps.io token** — redeploying the app needs the
   `datascapes` account token (`rsconnect::setAccountInfo`). Skip if the app isn't this
   weekend's priority.

**That's the entire critical path for human hands.** Everything below is 🟢 unless marked.

### 🟡 Decisions (defaults proposed — say nothing and these apply)

| Decision | Default |
|---|---|
| Keep South Korea? (old pipeline data ended 2007; now world's lowest fertility) | **Yes — include, and consider featuring** |
| Country ranking year (hardcoded as 2007 in `scripts/data_prep.R`) | **Latest year with near-complete panel coverage (likely ~2020)** |
| Existing exclusions (`CHL, CHN, TUR, GBR_NP, DEUTNP`) | **Re-examine CHL/TUR (HFD quality may have improved); keep excluding aggregates** |
| Ragged right edge (countries end 2023–2025 at different points) | **Show ragged edge, grey annotation — don't truncate to common year** |
| Commit refreshed combined CSV to public repo? | **Check current HFD/HFC terms first** — derived/combined data was committed in 2016; terms may have changed |

### 🔴 Post-gallop review checklist (Jon, after the 2-hour sprint)

In review order (items 5–7 and the push are DONE; v0.1 went public before full
human review, per Jon's preprint-early strategy — review is now post-hoc):
1. **Read the two drafts** — `manuscript_2026/descriptive_finding.qmd` (or its
   rendered .html) first, then `wall_corridor_article.qmd`. Check especially the
   US claim wording and the co-author invitation text. Revisions become
   preprint v2 (Edit button on each preprint page).
2. **Eyeball the figures** — `figures/figures_2026/`: norway_only, the three
   splits, featured_wall_{nor,usa,kor}, wall_trajectories.
3. **Skim the two reports** — `reports/validation_2026.md`, `reports/wall_analysis_2026.md`.
4. ~~Decide: commit the combined 2026 CSV?~~ — DONE 2026-07-11: Jon approved; committed (c1ce25d).
5. ~~Contact Serena & Laura~~ — DONE 2026-07-11 via LinkedIn.
6. ~~Push~~ — DONE 2026-07-11 (master, feature-claude-update, demres-2020 all on
   GitHub). Remaining: create a GitHub Release from the tag; optionally enable
   Zenodo first so the release mints a DOI.
7. ~~OSF~~ — **DONE 2026-07-11** (Claude-driven browser session; Jon did the two
   native file-picker clicks):
   - Umbrella project (public): <https://osf.io/3ju29> — description carries the
     AI-drafting provenance statement
   - Components: Descriptive finding <https://osf.io/mkr79>; Research article
     <https://osf.io/2yzmc>
   - Preprint 1: "The exceptions collapse" —
     <https://osf.io/preprints/socarxiv/8cb5g_v1> (pending moderation)
   - Preprint 2: "The closing corridor" —
     <https://osf.io/preprints/socarxiv/8jqad_v1> (pending moderation)
   - Both: CC-BY 4.0; Sociology/Population subject; GitHub repo as public-data
     link; honest "no preregistration; choices fixed in public workplan"
     assertion; AI-assistance note in abstract; component linked as supplemental.
   - REJECTION + FIX (2026-07-11, same day): both v1 submissions were rejected
     by SocArXiv moderation for one reason — no ORCID linked to the OSF
     profile (SocArXiv requires an ORCID with public identity/affiliations).
     Fixed same day: Jon's ORCID record updated from his CV (employment
     end-dated PHS 2024-09, added Smith & Nephew Senior Statistician
     2024-09→present, three education entries added: PhD York 2011, MA
     Nottingham 2004, BEng Nottingham 2003; CodeClan Professional Software Development SCQF-8, Feb-Jun 2023, Cohort E63); ORCID connected to OSF profile (Jon's OAuth click);
     both preprints now PENDING MODERATION again.
   - RESUBMITTED (2026-07-12, Jon): both preprints back in the moderation
     queue with the sole flagged issue resolved — ORCID
     (0000-0003-1207-6259) is linked to the OSF profile and publicly shows
     research activity from 2017 and earlier (45 works, employment,
     education). Anonymous API still 403s both (= pending, as expected).
   - REJECTED AGAIN (2026-07-14), identical moderator wording on both papers
     (Philip N. Cohen): "The 'note' is in the metadata form, but not in the
     paper. The unconsenting co-authors are not listed. This is irregular. We
     only accept complete papers with consistent metadata that matches across
     forms and the paper." Logged as F-011. STRATEGY (Jon, 2026-07-14): no
     rapid resubmission — SocArXiv moderation is not a rapid-iteration
     channel; next submission only when the manuscript is substantially more
     developed. Authorship/metadata consistency fixed in both qmd sources
     same day (v0.2). OPEN DECISION: merge DF+CC back into a single fuller
     paper (Jon leaning; fits DR's range better than an update-alone paper;
     the 5th-encoding + visualisation-as-informal-model-building material
     could carry a merged paper) vs keep two. Decision to be informed by DR
     article types/lengths and alternative journals — discussion in session
     2026-07-14. DF meanwhile reframed as forecast-resolution (F-010).
   - DIRECTION HARDENING (Jon, 2026-07-14 evening): ONE paper, not many;
     more people potentially involved via contributorship→authorship; an
     explicit visualisation→modelling bridge; the social-media/acceleration
     debate as the motivating example; AAI's role (rapid update of existing
     research, preventing staleness) presented as a FEATURE of the workflow,
     not a point of shame. Merged-article structure + contributor-outreach
     analysis drafted in session (see chat log 2026-07-14). DR editorial
     facts checked: EiC Anna Matysiak; Monica Alexander is a DR associate
     editor (COI note if approached); 2020 collection guest editors
     Riffe/Klüsener/Sander and past EiC Bijak (2018–2024) are unconflicted
     outreach candidates. Awaiting Jon's decisions on structure + outreach.
   - CO-AUTHORS (2026-07-12, Jon): .docx builds of both drafts shared with
     Serena Pattaro and Laura Vanderbloemen (tracked-changes/comments
     channel per the two-channel review setup; join-by-contribution
     invitation for v2+).
   - SocArXiv pre-moderation ~1–2 days; DOIs assigned on acceptance.
   - Remaining OSF nice-to-haves: upload this workplan file to project storage;
     verify components display as public; GitHub add-on link (needs Jon's OAuth).

### Suggested split of the 2-hour timebox

| Block | Time |
|---|---|
| Registration + downloads (items 1–3 above) | 30–40 min |
| Review Claude's data-validation report + regenerated core figures | 40 min |
| Review Phase 3 prototype (the "wall" figures) + decide next steps | 30 min |
| Buffer / app redeploy if time remains | 10–20 min |

---

## Context: why update, and what we already know (planning-session findings)

The published paper — Pattaro, Vanderbloemen & Minton (2020), *Visualizing fertility
trends for 45 countries using composite lattice plots*, Demographic Research 42(23):
689–712, doi:10.4054/DemRes.2020.42.23 — used data ending **2014–15** (median last
year 2014; FRA/NLD/GRC/BGR stop 2008–09; KOR stops 2007).

Three justifications for updating, in priority order:

**(1) The data now contradicts the paper's headline finding.** The abstract named
Norway and the USA as the exceptions that returned toward replacement. Since then:
Norway TFR 1.98 (2009) → 1.40 (2023, record low); USA → 1.6 (2024, record low);
South Korea → ~0.72. The two showcase exceptions became leading examples of the new
accelerated decline. This alone justifies the update.

**(3) The "wall" / compression analysis — HIGHER priority than (2) because it
continues the last paper's internal narrative.** Findings from the repo's own data
(pooled median across country-years; highest age with ASFR ≥ 0.005):

| Period | Effective fertility ceiling | Cumulative ASFR from ages 40+ |
|---|---|---|
| 1850s–70s | 48 | — |
| 1950s | 44 | 0.114 |
| 1980s–90s | **41 (minimum)** | 0.023 |
| 2000s | 42 | 0.035 |
| 2010s (to 2014) | 43 | 0.054 |

The ceiling is behavioural, not biological (it was 47–48 under natural fertility;
contraception pulled it down; it's creeping back ~1 yr per 15 yrs — US 40+ rates rose
24% 2015–2024). But the mass behind it is tiny: the entire 40+ segment contributes
~0.05 children, ~2.6% of replacement. Meanwhile age at first birth rose ~5–6 years.
**Replacement must now be achieved inside a ~13-year corridor (≈30→43).** Postponement
converts to quantum loss geometrically — regardless of *why* the floor rises. This
directly extends the 2020 paper's story: it explains *why* countries that fall below
2.05 don't come back, and why the Norway/USA exceptions collapsed.

**(2) The social-media / acceleration debate — real but lower priority.** The
composite plots' contribution is as a discipline device: the debate's contested claims
(4G study: teens respond, 25+ doesn't; period-shock vs cohort-socialisation variants;
tempo vs quantum) are all claims about age–period–cohort structure, which the Lexis
surface renders directly and TFR line charts destroy. Frame any output as "the
fingerprint any explanation must match", never "the picture that proves it was the
phones". Key risks: right-edge censoring of the very cohorts under debate (born
1995–2010) — grey out censored regions; co-timed period shocks (2008 crisis, housing,
phones) are not separable by geometry alone.

---

## Phase 1 — Data refresh 🟢 — **DONE 2026-07-11**

Completed same session as the downloads:
- `scripts/ingest_2026.R` written and run → `data/data_combined_and_standardised_2026.csv`
  (51 countries, 168,099 rows, 1850–2025, with `source` column: hfd/hfc/interp).
- Validation report: `reports/validation_2026.md` — build accepted; all overlap
  discrepancies explained (HFD back-revisions for IRL/ESP/DNK/…; source-switch for KOR;
  refcode selection for HFC-only countries). France fix verified (old build's HFD
  France sat under unmapped `FRATNP` and was silently dropped by the `to_keep` join).
- 🟡 NEW DECISION for Phase 2: published figures are `lattice`-based
  (`scripts/two_contour_version_of_figures.R`, levelplot + contourplot via
  latticeExtra). Stay with lattice for continuity, or migrate to ggplot2?
  Default: **stay with lattice** — reuse the published figure code unchanged.

Original plan (for reference):

1. New ingest script `scripts/ingest_2026.R` (leave the 2016 pipeline untouched for
   reproducibility of the published paper):
   - Parse new-format HFD `asfrRR` (note: age codes `12-` / `55+` need cleaning;
     check for `OpenInterval` column).
   - Parse refreshed `HFC_ASFRstand_TOT`; verify the `Collection` codes
     (STAT/ODE/RE) and `AgeDef == "ACY"` filter still hold.
   - Reapply the published combination rules: HFD over HFC; within HFC,
     STAT → ODE → RE; single-year periods only; linear interpolation for gaps.
   - Output `data/data_combined_and_standardised_2026.csv`.
2. **Validation report** (the main thing Jon reviews):
   - Regression test: for overlapping country-years, new ASFR vs old ASFR
     (flag any |Δ| > 0.001 — HFD does revise back-series).
   - Coverage table: per country, first/last year, source (HFD/HFC), gap years.
   - Note which HFC-only countries gained nothing (much of HFC is static;
     the update's gains come mostly from HFD).
3. Expect ~10 extra years for HFD countries (through 2023–25) and possible new
   HFD member countries since 2016 — list them in the report.

## Phase 2 — Regenerate core figures 🟢 — **CORE DONE 2026-07-11** / app 🟡

Done: `scripts/figures_2026.R` (no latticeExtra needed — its `interp` dependency
fails to compile on this machine's toolchain, and latticeExtra IS still on CRAN,
so this was a local build issue; fill + contours are drawn in a single lattice
panel function instead, visually identical). Outputs in `figures/figures_2026/`:
`norway_only.png`, `overall_gridded.png` (45 countries, data to 2025, cohorts to
2013), `overall_split_{1,2,3}.png`, and `ccfr_in_2007_from_2026_data.csv`
(compare with old `ccfr_in_2007.csv` to check ordering stability).
The Norway panel now shows the full arc: the 2.05 contour goes vertical (~age 43)
for cohorts born ~1950, RETURNS to ~age 40 for the 1958–68 cohorts (the paper's
exception), then goes vertical again at cohorts born ~1972 — the reversal, in one
image. App REDEPLOYED 2026-07-12 with the 2026 dataset (first redeploy since
2019; datascapes token still valid locally; fixes: explicit library()
calls replacing tidyverse meta-package, archive latticeExtra 0.6-28,
removed stale %T>% debug idiom; verified live). Remaining 🟡: Norway/USA
annotation figures for the manuscript (F-006 overlay covers this).

## Preprint-early strategy (Jon, 2026-07-11) + manuscript drafts — STARTED

Jon's decision: post preprints EARLY (v0.1, before full human review) — OSF/SocArXiv
preprints are version-controlled (formal versioning: updated files preserved as
numbered versions under one stable DOI), so early posting + public iteration is
viable and the repo makes development visible.
**DONE**: `manuscript_2026/references.bib` (42 extracted published refs + verified
new additions, flagged for pre-submission checking) and two full working drafts,
both rendering cleanly via Quarto (html + docx): `descriptive_finding.qmd` (v0.1,
substantially complete) and `wall_corridor_article.qmd` (v0.1, results complete,
discussion partially drafted, TODOs marked).
**VERIFIED**: USA crossing finding — cohorts 1982/83/84 crossed 2.05 at ages
39/39/40 (cum. 2.14/2.13/2.09), zero interpolated cells, neighbours consistent;
1985 at 2.04 by age 39 (censored). Drafts state the nuanced version: US exception
persisted through ~1984 cohorts, fading in the censored region — NOT "collapsed".
**Authorship model (decided)**: v0.1 posts under Jon's sole authorship with the
2020 co-authors acknowledged and explicitly invited to join by contribution;
OSF preprint contributor lists are editable after posting, so co-authors can be
added in later versions (invite them via email as OSF contributors when they
accept). Both drafts now carry this note.
🔴 **BEFORE any public posting**: (1) courtesy heads-up email to Pattaro &
Vanderbloemen (not consent — their names aren't on v0.1 — but it extends joint
work); (2) Jon's own read-through of both drafts; (3) SocArXiv moderation takes
~1-2 days and requires the poster to be an author. AI-assistance disclosure is
already in both drafts' notes.

## Manuscript modernization — DISTINCT TASK (Jon, 2026-07-11)

Treat as its own work package, separate from Phases 1–4:

1. **.bibification**: the published manuscript's ~44 references are hand-formatted
   in Word (no .bib anywhere in the repo). Extract them from the final docx
   (`manuscript/second_revision/Submission #4457 R1 Third revised version_17Jan2020 JM.docx`,
   already text-extracted in a prior session) into `manuscript_2026/references.bib`.
   Mechanical; good first step of any manuscript session.
2. **.qmdification**: convert the published text to Quarto
   (`manuscript_2026/manuscript.qmd` + the .bib + figures from
   `figures/figures_2026/`) for machine parsability and reproducible figure
   embedding. Note Demographic Research submits via their own house
   template — the .qmd is the working/preprint version, not the submission file.
3. **New references to search for** (during manuscript drafting):
   - Nordic decline: Comolli et al. (2021, Eur J Population); Hellstrand, Nisén
     & Myrskylä (2020, Demography, Finland); Hellstrand et al. Nordic cohort work
   - US: Kearney, Levine & Pardue (2022, J Econ Perspectives — the "puzzle")
   - Korea: Yoo & Sobotka (2018, Demographic Research — tempo in ultra-low fertility)
   - Late/"wall" fertility: Beaujouan (2020, Pop & Dev Review — latest-late
     fertility); Beaujouan & Sobotka on late first births; Leridon (2004) on
     human reproductive ageing and postponement recuperation
   - COVID: Sobotka et al. on pandemic fertility responses; HFD STFF citation
   - Connectivity debate: Billari, Giuntella & Stella (broadband and fertility);
     the 2025/26 4G-rollout fertility study (find exact citation); Burn-Murdoch
     FT columns and Alice Evans essays as discourse references
   - Refresh HFD/HFC methods-protocol citations to current versions
   ⚠ **Search phrasing rule (Jon, 2026-07-11): restrict all literature searches to
   HUMAN fertility/demography terms. Do not phrase searches comparatively with
   other species — biology-adjacent queries can trip model guardrails; demographic
   phrasing does not.**

## Publication strategy — DECIDED 2026-07-11: two manuscripts (+ outreach)

OSF structure (agreed): **one umbrella OSF project**, GitHub repo linked via the
OSF-GitHub add-on, with **two components** (one per manuscript: Descriptive
Finding; wall/corridor article). Each manuscript becomes its own **SocArXiv
preprint** connected to its component as supplemental material. The workplan is
uploaded at project level as the time-stamped analysis plan ("plan + initial
results" framing). Optional: GitHub releases minted with Zenodo DOIs so both
papers can cite the exact code version. Raw HFD/HFC data stays off OSF (terms) —
derived tables only. All OSF clicks are Jon's (~15 min once ready to be public).

The two outputs:
1. Fast **Descriptive Finding** (Demographic Research format): the reversal of
   the 2020 paper's Norway/USA exceptions on the updated 45-country panel.
2. Full research article: the wall/compression/reachability framework, with the
   acceleration-debate material as its *discussion* stance (stylized facts any
   explanation must match), not a separate paper.
3. Outreach (N-IUSSP-style piece / refreshed app / blog): the debate-facing
   translation, citing 1 and 2. Not peer-reviewed, so it can move at news speed.
Birth-order extension = a real future paper #3, when built.
**Candidate paper #3-or-#4 (Jon, 2026-07-11): the universality test.** A key
implication of connectivity/smartphone hypotheses is that fertility decline
has accelerated in ALL countries regardless of economic development — unlike
recession- or policy-based explanations, which predict development-graded
timing. Investigable, but NOT with HFD/HFC alone (rich-country biased):
needs UN WPP-type estimates for global coverage, i.e. a distinct data
pipeline, lower age-resolution, different quality caveats. Park until papers
1–2 are through review; note it connects Phase 4's cross-country-timing idea
to a global panel. Strategy options + objections drafted (2026-07-11):
`manuscript_2026/universality_strategy.md`. LITERATURE PHASE COMPLETE
(2026-07-11): `manuscript_2026/universality_lit_notes.md` — four parallel
research tracks; focal PDFs obtained (no SSRN login needed; local copies
git-ignored); headline finding: H&MB's "25+ null" is near-tautological given
their eq.-6 detrending (verified against the PDF), reconciling it with
Myers & Hooper's 33-52% all-ages claim and defining paper 3's niche. ~35
verified refs added to references.bib Part 3. PROTOCOL FROZEN & REGISTERED
(2026-07-12): `manuscript_2026/universality_prereg_protocol.md` v1.0 frozen at
commit `2eb0733`; registered on OSF as an Open-Ended Registration, public
immediately: **https://osf.io/j3tbq** (component zs3wn, project 3ju29), with
the protocol .md + .pdf archived inside the registration and the full decision
rules + SHA permalink in the registration summary. Registration precedes any
WPP download — the WPP 2024 pull is now unblocked. — recommended architecture is
fingerprint-first (WPP descriptive surface), estimated break dates with a
permutation null, adoption-alignment and age-margin modules for
discrimination; the crude fixed-2008 shift survives only as a baseline.
Venue note: Comparative Population Studies published a direct methodological
descendant (GDR paper, 2026) — evidence of a receptive venue for 1 or 2.

## OSF / preregistration decision (considered 2026-07-11)

Recommendation: **no formal preregistration; time-stamped analysis plan + preprint after.**
Rationale: this is descriptive research whose headline result (Norway/USA reversal)
is already knowable from public data — confirmatory-style prereg would be theater.
The genuine researcher-degrees-of-freedom sit in Phase 3 (ceiling threshold 1/200
vs 1/1000, per-country vs pooled, reachability definition). Cheap credibility move:
create an OSF project linking the GitHub repo and upload this workplan as a
time-stamped analysis plan BEFORE running Phase 3 on the new data (disclose that
wall prototypes were computed on the OLD data during planning). Weaker fallback:
push + git-tag the repo publicly. Preprint (e.g., SocArXiv) once drafted.
Jon's OSF account is active (dashboard checked in-session; no fertility project
exists yet).

1. Rerun composite lattice plots on the extended panel (new ranking year per the
   🟡 decision; rebuilt country ordering).
2. **Rewrite the Norway annotation.** The repo's most recent commits added
   Norway-as-exception figures; the updated figure should show the reversal —
   arguably the single most striking image the update produces.
3. Add a censoring convention for the ragged right edge / incomplete cohorts.
4. App (`cumulative_fertility_app/`): swap in the new CSV; redeploy needs the
   🟡 token. Defer if tight.

## Phase 3 — The wall / compression analysis — **CORE DONE 2026-07-11**

Done via `scripts/wall_2026.R` (+ shared `scripts/functions_2026.R`). Outputs:
`data/derived_2026/*.csv`, `reports/wall_analysis_2026.md`, and four figure sets
in `figures/figures_2026/` (wall trajectories, late-mass trends, reachability
crossing ages, featured NOR/USA/KOR surfaces with the ceiling overdrawn).
Headline numbers (2026 panel): pooled ceiling 45 (1950s) → 41 (1980s–90s) → 43
(2020s); 40+ mass 0.117 → 0.023 → 0.065 (creep continues into the 2020s);
43 of 45 countries have post-war cohorts observed past 44 that never crossed 2.05;
only 9 countries have any cohort born ≥ 1970 crossing.
⚠ To VERIFY before featuring: the US 1984 cohort crossing 2.05 at age 40 (a big,
quotable claim — check sensitivity to interpolation and the 2024 data edge);
the `first_never_cohort` column mixes pre-transition historical cohorts (FRA 1892,
SWE 1898) with modern ones — split by era before publishing.
NOTE: ran before any OSF time-stamp (Jon's call to proceed); the workplan's
pre-specified choices (1/200 primary, 1/1000 sensitivity, per-country primary,
never = observed past 44) were applied unchanged.

Original plan (for reference):

**Geometry reminder (the "4th dimension").** The composite plots carry four
dimensions: x = period, y = age, colour = ASFR level, and **contour lines = CPCFR**
(cumulative *pseudo*-cohort fertility rate: period ASFRs cumulated along cohort
diagonals). Each contour traces, per cohort, the age at which a fertility milestone
(0.5, 1.0, 1.5, 2.05) is reached. Because cumulative fertility barely increments
above ~40 (the thin tail), the inverse function — age as a function of milestone —
has near-infinite slope there: **contours characteristically go vertical at ~42–43**.
That verticalization IS the wall, expressed in contour form; Phase 3 formalizes an
already-visible feature of the 2020 figures. One care point: a contour can terminate
for two distinct reasons — (a) right-edge/cohort **censoring** (data ran out) or
(b) the **wall** (fertility mass ran out). These look similar on the surface and must
be visually distinguished (e.g., grey censored terminations, mark wall asymptotes).

Priority over Phase 4. Concrete outputs:

1. **Per-country ceiling trajectories** — each country's own "highest age with
   ASFR ≥ 1/200" line over time (the pooled medians above are composition-sensitive:
   only 179 country-years in the 2010s vs ~500 in the 2000s — per-country is the
   defensible version).
2. **Wall overlay on the Lexis surfaces** — draw the ceiling as an explicit line;
   the reader sees a near-horizontal boundary since 1990 while the colour mass
   migrates upward beneath it.
3. **Reachability figure** — for each cohort, does the 2.05 CPCFR contour cross
   before the trajectory hits the wall? For post-~1975 cohorts in many countries
   the contour will visibly terminate at the boundary uncrossed.
4. **40+ mass quantification** — cumulative ASFR above 40 (and 35) by country and
   period, bounding the maximum possible late recuperation (~0.05–0.11).

Threshold sensitivity (1/200 vs 1/1000) should be shown once in an appendix figure.

## Phase 4 — Debate-facing outputs *(later; only after 1–3 are solid)*

1. Period-vs-cohort geometry annotations (vertical vs diagonal features) on the
   post-2008 region of selected surfaces.
2. Cross-country timing: does the break track smartphone/4G adoption dates or sit
   fixed at 2008? (Needs an adoption-year lookup per country — small side dataset.)
3. Tempo-vs-quantum: recuperation contours for recent cohorts vs the wall (reuses
   Phase 3 machinery — this is why Phase 3 comes first).
4. Consider HFD's STFF monthly series for the post-2020 edge.
5. Publication target: Demographic Research descriptive-finding format, or a
   follow-up to the 2020 special issue. Framing rule from the planning session:
   **stylized facts any explanation must match — light, not heat.**

---

## Session-order summary

| When | What | Who |
|---|---|---|
| Now (before data) | Workplan (this file); optionally pre-adapt ingest + wall-prototype scripts against old data | 🟢 |
| Weekend, first 40 min | Register, download HFD + HFC, drop into `data/*_2026/` | 🔴 |
| Weekend, rest | Phase 1 validation + Phase 2 core figures; review | 🟢 + review |
| Next session | Phase 3 (wall) in full | 🟢 + review |
| Later | Phase 4 (debate framing), app, manuscript | mixed |
