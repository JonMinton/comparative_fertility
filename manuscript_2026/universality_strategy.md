# Research strategy options — candidate paper 3: "the universality test"

*Drafted 2026-07-11 (session with Jon). Status: strategy only — no analysis
begun. See UPDATE_WORKPLAN.md publication-strategy section for how this paper
relates to papers 1–2.*

## The claim to be tested

The connectivity/smartphone family of explanations carries a distinctive,
falsifiable implication: fertility decline should have **accelerated in
essentially all countries at about the same calendar time (~2008–2013),
regardless of economic development level**. Development-graded explanations
(recession exposure, housing, policy) instead predict acceleration whose
timing and size track country circumstances. The paper's job is to establish
whether the universal-synchronous pattern is a fact, as a disciplined
stylized-fact contribution — not to adjudicate mechanism.

## The central methodological hazard (applies to every strategy below)

**"Acceleration relative to what?"** Any acceleration estimate is a deviation
from a counterfactual trend, and the sceptics' strongest critique of existing
work is that declines measured against extrapolated pre-2008 trends look
artificially sudden — especially since the 2000s rebound (the "end of
lowest-low" period) may itself have been the anomaly, making post-2008 decline
partial *mean reversion* rather than regime change. Whatever strategy is
chosen: pre-specify trend windows and functional forms in this repo before
estimation; show robustness across counterfactual choices; and test explicitly
whether post-2008 behaviour is distinguishable from reversion to the pre-2000
trajectory.

---

## Strategy S1 — imposed break: before/after-2008 shift terms (Jon's opener)

Per-country model of TFR (or of specific margins) with trend + slope-shift
and/or level-shift terms at a fixed 2008 breakpoint; then examine the
distribution of shift coefficients across countries and across development
strata.

**For:** transparent, communicable, one parameter per country answers "did
this country bend?"; strata comparisons are trivial.

**Objections:**
- *Crude, as suspected.* Imposing 2008 conflates the financial crisis with the
  smartphone/social-media era (mass adoption 2010–13); a single fixed date
  begs the question the paper is asking.
- The linear-trend counterfactual is exactly the artefact the sceptics attack.
- Period TFR confounds tempo and quantum: a synchronized *postponement* wave
  and a synchronized *quantum* fall look identical.
- Serially correlated, near-unit-root series make break terms spuriously
  significant; needs HAC/bootstrap care that then undercuts the simplicity.

**Verdict:** keep — but demoted to a transparent *baseline* inside a battery,
never the headline estimate.

## Strategy S2 — estimated breaks: let each country date its own bend

Segmented-trend estimation per country (Bai–Perron or simple grid-search over
break dates), then the object of interest is the **distribution of estimated
break dates**: does it spike at a common date across the entire development
gradient, or spread out?

**For:** directly operationalizes "all changed at once" without assuming the
date; the break-date histogram (optionally by development tercile) is a
publishable figure in itself.

**Objections:**
- Break dates from short, noisy, revised series are unstable; window and
  minimum-segment choices materially move the histogram.
- Needs a serious null: break dates cluster mechanically under global shocks
  of *any* kind (COVID guarantees one) — permutation/simulation null required.
- End-of-sample boundary effects: a bend near 2019 is hard to date; COVID
  bump-and-bust at the right edge must be excluded or modelled.

**Verdict:** strong candidate for the analytic core, if paired with a
pre-specified null and estimation window (e.g., 1990–2019).

## Strategy S3 — event-time alignment on adoption

Re-align countries on connectivity milestones (3G/4G rollout, smartphone or
social-platform penetration thresholds) instead of calendar time; event-study
around adoption. Discriminates "synchronous in calendar time" (any global
shock) from "staggered, tracking adoption" (technology-specific).

**For:** the only strategy that even attempts to separate the 2008 crisis from
connectivity; adoption timing genuinely varies by several years.

**Objections:**
- Adoption is endogenous to development and itself clusters in calendar time —
  identification is weaker than it looks.
- Adoption measurement is heterogeneous across sources and countries.
- The existing 4G-rollout literature found effects concentrated among teens
  only; a global design lacks the age resolution to see this (below).

**Verdict:** include as the *discriminating* module, with honest confidence
bounds; do not hang the paper on it.

## Strategy S4 — the fingerprint: descriptive, visual, house-style

No regression headline. Take one frozen vintage of UN WPP estimates
(~200 populations), compute first and second differences of fertility
trajectories, and visualize the country × year acceleration structure ordered
by development level — the "vertical band" test at global scale: does a
synchronized feature appear across the whole development gradient around
2008–13? Summary statistics (share of countries decelerating per window) and
a verified vital-statistics subsample as robustness.

**For:** plays to the project's comparative advantage and identity (papers 1–2
are exactly this register); fastest to produce; the figure is the argument;
"stylized facts any explanation must match" framing already established.

**Objections:**
- **WPP is partly model output.** For many countries, "estimates" are
  smoothed/interpolated model reconstructions; synchronized features can be
  artefacts of the estimation vintage. Mitigations: single named vintage;
  vital-registration-only subsample shown side by side; sensitivity across
  two vintages.
- Coarse age resolution (5-year groups) limits any age-fingerprint claim.
- Descriptive stance invites "so what caused it?" — answered by the
  established light-not-heat framing, but reviewers may want more.

**Verdict:** the natural Stage 1 and the paper's spine.

## Strategy S5 — common-factor / synchronization econometrics

Dynamic factor model or rolling cross-country correlation: did the global
factor's share of fertility-change variance jump after ~2008?

**Objections:** heavy machinery for the intended venue and audience; fragile
with ~50–200 short series; documents synchrony without discriminating
mechanism (which S4 shows more legibly anyway); highest reviewer-burden per
unit of insight.

**Verdict:** omit, or one appendix robustness figure at most.

## Strategy S6 — age-margin fingerprint (the sharpened version of S1's "margins")

Where age-specific data exist (HFD/HFC panel; WPP 5-year groups elsewhere),
interact the post-break shift with **age**: is the acceleration universally
concentrated at young-adult ages? Connectivity stories (partnering collapse,
delayed union formation) predict a young-skewed fingerprint *everywhere*;
tempo-neutral economic stories predict more variation.

**For:** this is the margin-shift idea done at the margin that matters; links
directly to the corridor paper (young-age loss against a fixed ceiling) and to
the 4G-teens-only finding.

**Objections:** recession effects are *also* young-skewed (last-in labour
market), so the fingerprint discriminates weakly; global age resolution is
coarse; rich-country age detail reintroduces the development bias the paper
exists to escape.

**Verdict:** include as the bridge between the global panel (coarse) and the
HFD panel (fine) — two resolutions, one question.

## Strategy S7 — tempo/quantum decomposition of the acceleration

Decompose post-2008 decline into postponement vs quantum where mean-age data
permit; ask whether the composition is uniform.

**Objections:** only feasible for the rich-data subsample — undermines the
universality logic; tempo adjustment methodology is itself contested; risks
turning paper 3 into paper 2.

**Verdict:** defer; note as future work linking papers 2 and 3.

---

## Prior art to engage first: the 4G-rollout study (Burn-Murdoch's source)

**Hudson & Moscoso Boedo (Univ. of Cincinnati), SSRN working paper, April
2026** (not yet peer-reviewed): exploits *staggered subnational rollout of 4G*
in the US and UK; births fell first and fastest in areas that received
high-speed mobile connectivity earliest. Their own headline caveat: "Whatever
the smartphone shock is doing to fertility, it is doing to teens. The entire
25+ population … exhibits no detrended response in the typical country."
First task of paper 3's literature phase: obtain the working paper and
reconstruct the exact estimator (presumably event-study/staggered
difference-in-differences on area × age-group birth rates).

**Standing methodological objections to log and check against the paper
itself:**
1. **The detrending choice is the result.** "No detrended response" for 25+
   depends entirely on what trend was removed — the same counterfactual
   hazard as our S1/S2. Critics also run this in reverse: against
   pre-smartphone extrapolations, the residual teen dip looks artificially
   sudden.
2. **Rollout endogeneity.** 4G arrived first in denser, richer, younger,
   more urban areas whose fertility was already on different trajectories —
   parallel-trends is the whole game and is doubtful here.
3. **Staggered-DiD pathologies.** Two-way fixed effects with staggered
   adoption and heterogeneous effects produces contaminated estimates
   (Goodman-Bacon decomposition literature); whether they use modern
   estimators matters.
4. **Teen-specific confounds.** The US teen-birth collapse (~2008–2015)
   coincides with LARC expansion and other teen-targeted contraception and
   policy shifts; an area-level rollout design struggles to separate these.
5. **Ecological exposure.** Area-level availability is not individual use.
6. **Two rich anglophone countries** cannot carry a universality claim in
   either direction — which is precisely the gap paper 3 exploits.

Note the strategic asymmetry: their design is *micro/quasi-experimental*
within two countries; ours is *macro/structural* across all countries. The
two are complementary, and the paper should say so rather than compete.

## Strategy S8 — Lexis-field ("pseudospatial") approaches: the acid test

Jon's framing: the age × period ASFR surface is pseudospatial data — a 2-D
field on a lattice — so adoption hypotheses become *geometric* hypotheses
about where and how a change-front crosses the field. If adoption reached
younger people first and spread to older ages, the response should not be a
clean vertical step in period but an **age-graded front**: a step function in
period whose onset year increases with age.

**The model taxonomy** (each adoption pattern implies a distinct, fittable
signature; all are statements about the *sign of change* field
∂ASFR/∂t, not about levels):

| Model | Adoption pattern | Signature on the Lexis surface |
|---|---|---|
| M1 | Simultaneous shock, all ages (crisis-like, or instant saturation) | Vertical front: onset year constant in age |
| M2 | Young-first diffusion up the age scale | Sloped front: onset year rising with age, slope = 1/velocity (years of age per calendar year), between vertical and the cohort diagonal |
| M3 | Cohort-carried (socialized-in-adolescence) | 45° front along cohort diagonals: onset year rises 1-for-1 with age |
| M4 | Cross-population staggering | Same within-country geometry, but front onset varies by country adoption date / development |

**Estimators, simplest first:**
- **Front extraction (the workhorse).** For each country × single age (or
  5-year group), estimate the break year of the ASFR series (S2 machinery,
  one dimension lower). Then regress estimated break year on age within each
  country: slope ≈ 0 → M1; slope ≈ 1 → M3; intermediate stable slope → M2
  with measurable diffusion velocity; no coherent front → none of the above.
  Two parameters per country (onset, velocity) then travel to the
  cross-country stage: regress onset on adoption date/development (M4).
  Communicable, visualizable (fitted front overlaid on the surface — the
  house style), and each competing story is a parameter value, not a vibe.
- **Competing parametric surface fits.** Fit M1/M2/M3 as constrained
  interaction structures on the ASFR (or log-ASFR) field per country and
  compare by information criteria — the formal version of the same test.
- **Image-analysis analogies** (edge detection on the first-difference
  surface, spatial autocorrelation of residuals) as robustness/illustration,
  not headline — reviewers in demography will trust break regressions over
  Sobel filters.

**Objections:**
- **Tempo mimicry (the big one).** A postponement wave moves the ridge of the
  surface upward in age over time and generates *young-age decline followed
  by older-age decline* mechanically — an M2-looking front with no diffusion
  of anything except birth timing. Any front finding must be shown to
  survive, e.g., analysis of cumulative (CPCFR) milestones or comparison
  with mean-age dynamics; this is where the corridor paper's machinery
  re-enters.
- ASFR surfaces are smooth; breaks are blurred and onset dating at single
  ages is noisy, especially at high ages near the wall (low levels → divide
  the field's usable age range honestly).
- Resolution split: the fine test needs single-year Lexis squares (HFD/HFC
  panel — rich-country biased); the global panel (WPP) only supports 5-year
  groups → run the fine test on the 45-country panel, the coarse test
  globally, and report both, mirroring S6's two-resolution design.
- Multiple testing across country × age break searches; pre-specify the
  break-search window (1995–2019) and use the S2 permutation null.

**Verdict:** this is the acid test and the paper's most original module — it
converts the within-country adoption-pattern question into estimable geometry
using exactly the data structure papers 1–2 are built on. Promote to the
analytic core alongside S2.

---

## Recommended architecture (for discussion, not decided)

**Staged, S4 spine with S2+S8 core and S3+S6 as discriminating modules:**

0. **Literature phase:** obtain Hudson & Moscoso Boedo (SSRN 2026),
   reconstruct their estimator, log which of objections 1–6 apply, and
   position paper 3 as the complementary macro/structural test.
1. **Fingerprint first (S4):** one frozen WPP vintage; global acceleration
   surface ordered by development; vital-registration subsample robustness.
2. **Dating second (S2):** estimated break dates, 1990–2019 window,
   permutation null; histogram by development tercile.
3. **Geometry third (S8 — the acid test):** front extraction per country
   (onset + diffusion velocity by age); M1/M2/M3 adjudication on the
   45-country single-year panel, coarse version globally; tempo-mimicry
   check via cumulative milestones.
4. **Discrimination fourth (S3 + S6):** adoption-aligned event time;
   age-margin fingerprint at two resolutions.
5. **S1 appears only as the transparent baseline** in a robustness table; S5
   omitted; S7 deferred.
5. **Pre-specification:** estimation windows, functional forms, vintage,
   subsample definitions, and the reversion-vs-regime test all fixed in this
   file (versioned) before any estimation runs — same discipline as the
   corridor paper, and this time genuinely *before* analysis.

## Data requirements (all new to this repo)

- UN WPP estimates, one named vintage (API or bulk download); document which
  country-years are registration-based vs model-based if the metadata allows.
- Connectivity/adoption series (ITU indicators; platform-penetration sources
  TBD — quality audit needed before committing to S3).
- Recession-exposure classification (for the no-recession-countries contrast:
  did countries that skipped the 2008–09 recession accelerate anyway? —
  cheap, sharp, worth doing inside S4).
- COVID handling rule: primary window ends 2019; 2020–24 shown but not used
  for break estimation.

## Open questions for Jon

1. Venue/register: DR descriptive finding again (fits S4-led design) or a
   longer research article?
2. Is a 2-vintage WPP sensitivity worth the extra pipeline, or is the
   vital-registration subsample enough?
3. Does the no-recession-countries contrast deserve promotion to a headline
   module rather than a robustness aside?
