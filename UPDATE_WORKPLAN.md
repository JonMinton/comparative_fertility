# Workplan: Updating the Comparative Fertility Analysis

*Drafted 2026-07-11 (Claude Fable 5 session). Timebox: ~2 human hours this weekend.*
*This file is self-contained: all findings and decisions from the planning session are captured here so any future session (or human) can pick it up cold.*

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

All Claude-side work is committed locally; nothing is pushed. In review order:
1. **Read the two drafts** — `manuscript_2026/descriptive_finding.qmd` (or its
   rendered .html) first, then `wall_corridor_article.qmd`. Check especially the
   US claim wording and the co-author invitation text.
2. **Eyeball the figures** — `figures/figures_2026/`: norway_only, the three
   splits, featured_wall_{nor,usa,kor}, wall_trajectories.
3. **Skim the two reports** — `reports/validation_2026.md`, `reports/wall_analysis_2026.md`.
4. **Decide**: commit the combined 2026 CSV? (HFD terms check; currently untracked.)
5. **Send the courtesy/invitation email** to Serena Pattaro & Laura Vanderbloemen.
6. **Push**: `git push origin master feature-claude-update demres-2020` — this
   makes everything public (master already fast-forwarded locally; the
   `demres-2020` tag preserves the 2020-paper state and is signposted in the
   README). Then create a GitHub Release from the tag (name it after the paper)
   so paper-followers see it prominently; optionally enable Zenodo first so the
   release mints a DOI.
7. **OSF**: create project + two components, link GitHub, submit the two
   SocArXiv preprints (v0.1), upload workplan as time-stamped plan.

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
image. Remaining 🟡: app data refresh + redeploy (needs shinyapps token);
Norway/USA annotation figures for the manuscript.

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
Birth-order extension = the real future paper #3, when built.
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
