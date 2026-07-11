# Preregistration protocol — "Did fertility change everywhere at once? A universality test of the post-2008 acceleration"

*DRAFT v0.1 for OSF registration (secondary-data preregistration format).
Prepared 2026-07-11. NOT YET REGISTERED — bracketed [DECISION:] items must be
resolved by JM before freezing. Once frozen, this document is registered on
OSF (immutable) and this file carries the registration DOI; subsequent
changes follow §10 (Deviations policy).*

---

## 1. Study information

**Working title.** Did fertility change everywhere at once? A universality
test of the post-2008 acceleration in fertility decline.

**Authors.** Jon Minton (Independent researcher; jon.will.minton@gmail.com;
ORCID to be added). This work is unaffiliated with the author's employment.

**AI-assistance disclosure.** This protocol was drafted with AI assistance
(Claude, Anthropic) under the direction of JM, consistent with the disclosure
practice of the two companion working papers (SocArXiv 8cb5g, 8jqad). All
analysis code will be public and re-runnable; the division of labour is
documented in the project repository
(<https://github.com/JonMinton/Comparative_Fertility>).

**Background.** Connectivity-based explanations of recent fertility decline
(smartphones/social media) carry a distinctive implication: decline should
have accelerated in essentially all countries at about the same time
(~2008–2013), regardless of development level, and — under diffusion
variants — first at younger ages. Hudson & Moscoso Boedo (2026) document a
synchronized post-2007 break for TEEN fertility across 128 countries but
assert a null for ages 25+ using a within-country detrending (their eq. 6)
that subtracts crude-birth-rate growth — a construction that removes any
effect common to all ages and is near-tautological for the 25+ aggregate
(which produces ~80% of births). Myers & Hooper (2026) attribute 33–52% of
the all-ages US fertility decline to smartphone diffusion. These claims are
in tension unless the 25+ null is the artifact of the detrending. This study
estimates LEVEL responses across the full age range, treating the age ×
period fertility surface as a field, and tests three pre-specified
hypotheses about the timing, geometry, and gradient of the post-2008 change.

**Study type.** Secondary analysis of existing observational data.
Registration-before-download for the primary global dataset (see §3).

## 2. Prior data exposure (disclosure)

1. **HFD/HFC 45-country single-year panel (1850–2025):** EXTENSIVELY
   analysed by the author and AI assistant in two companion papers
   (descriptive update; fertility-ceiling analysis), including post-2008
   patterns for rich countries. All fine-resolution (single-year age)
   analyses in this study use this panel and are therefore *partially
   contaminated*; they are registered here with that status explicit.
2. **UN World Population Prospects 2024 fertility estimates:** NOT
   downloaded, NOT plotted, NOT inspected by the author or assistant as of
   registration. Knowledge is limited to: the published WPP 2024 Summary of
   Results, Methodology Report, Data Sources report (read for design
   purposes), and aggregate claims reproduced in press coverage and the
   Hudson & Moscoso Boedo working paper (e.g., their Figure 1 uses WPP
   ASFRs). No country-level WPP fertility series has been examined.
3. **Adoption and development covariates (ITU/WDI/HDI):** not downloaded;
   known only at the level of published aggregates.
4. **Grey knowledge:** the authors have read the Hudson & Moscoso Boedo and
   Myers & Hooper working papers and surrounding press; their reported
   patterns (e.g., teen-fertility breaks near adoption onset) inform the
   hypotheses and are the object of test, not evidence of it.

## 3. Data

**D1 — Global panel (confirmatory H1/H3 coarse tier; exploratory
fingerprint).** UN World Population Prospects, **2024 revision** (named
vintage), estimates variant: annual age-specific fertility rates (5-year age
groups 15–19 … 45–49; file `WPP2024_Fertility_by_Age5` or the equivalent
field in the `wpp2024` R package) and annual TFR. Access date will be
stamped in the repo at first download (after registration).
*Known limitation, accepted ex ante:* every WPP country-year is Bayesian
model output (WPP 2024 Methodology Report §I.B); smoothing may attenuate or
displace breaks and can manufacture cross-country synchrony. Hence D2.

**D2 — Registration-based subsample (confirmatory primary tier).**
(a) The existing HFD/HFC combined panel (45 populations, single-year ages,
in-repo, prior exposure disclosed); (b) additional countries coded from the
WPP 2024 *Data Sources* inventory as having fertility inputs from vital
registration with ≥98% completeness for all years 1990–2019. Coding is
performed from the Data Sources prose BEFORE any outcome data are examined,
by the AI assistant with an audit by JM; the coded list is committed to the
repo prior to analysis.

**D3 — Adoption series.** ITU mobile-cellular subscriptions per 100 people
(World Bank WDI `IT.CEL.SETS.P2`); iPhone country launch dates compiled
independently from Apple press releases (cross-checked against Hudson &
Moscoso Boedo's appendix). Country adoption year defined, for comparability
with the focal literature, as τ_c = max(iPhone launch year, first year
mobile subscriptions ≥ 80/100).

**D4 — Development covariates.** UNDP HDI (2007 value) primary; World Bank
GDP per capita PPP (2007) alternative.

**D5 — Vintage sensitivity.** `wpp2022` vs `wpp2024` R data packages
(github.com/PPgp). Confirmatory results are reported alongside their
wpp2022 counterparts; conclusions that do not hold in both vintages are
flagged as vintage-sensitive in the abstract.

**Estimation window.** 1990–2019 for ALL confirmatory analyses. Break-search
window 1995–2015 (≥5 years of data on each side of any candidate break).
2020–2024 data are displayed only in exploratory figures, clearly marked,
and never enter break estimation (COVID rule).

**Population exclusions.** Countries with population < [DECISION: proposed
500,000] at 2010; country-series with fewer than 25 observed years within
1990–2019; WPP countries whose fertility inputs are wholly survey/model-based
throughout 1990–2019 are excluded from confirmatory tiers (retained in the
exploratory fingerprint, labelled).

## 4. Design: confirmatory / secondary / exploratory split

- **CONFIRMATORY:** H1 (common break), H2 (front geometry), each with its
  tempo-mimicry co-primary and the uniform-vs-differential decomposition.
- **SECONDARY:** H3 (adoption vs development gradient); the age-margin
  fingerprint (S6); the no-recession-countries contrast [DECISION: promote
  to confirmatory? see §11].
- **EXPLORATORY (registered as such, no inferential claims):** the S4 global
  acceleration fingerprint surfaces (country × year first/second differences
  ordered by development); all 2020–2024 displays; any analysis not listed
  here.

## 5. Hypotheses and numeric decision rules

Throughout: α = 0.05 two-sided; 95% CIs; permutation p-values from ≥ 9,999
draws with fixed seed (seed = 20260711, committed).

### H1 — Universality of timing (common break, all ages)

**Claim.** All-ages period fertility (log TFR) exhibits a structural break
in trend that is COMMON across countries, located in 2008–2013.

**Test.** Panel common-break estimation (Bai 2010) on Δlog TFR, D2
registration tier primary, D1 global panel secondary. The estimated common
break date τ̂ and its 95% CI are reported.

**Decision rule.** H1 is SUPPORTED if (i) the common-break model is
preferred to the no-break model at α = 0.05 against a permutation null that
destroys cross-country synchrony while preserving within-country dynamics
(independent circular time-shifts of each country's series, uniform on
±3–12 years); AND (ii) the 95% CI for τ̂ lies within 2005–2015.
PARTIALLY SUPPORTED if (i) holds but the CI extends outside 2005–2015.
NOT SUPPORTED otherwise.

**Mean-reversion guard (co-primary).** The post-τ̂ trend is compared with
the pre-2000 (1990–1999) within-country trend: H1's interpretation as a NEW
regime (rather than reversion after the 2000s rebound) requires the post-τ̂
slope to be more negative than the 1990s slope in the median country
(sign test across countries, α = 0.05). If not, H1 is reported as
"synchronized reversion," explicitly.

**Uniform-vs-differential decomposition (co-primary).** For each country,
the post-break change in Δlog ASFR at each age group is decomposed into a
common (cross-age mean) component and age-differentials. We test whether
the common component is negative in the typical country (sign test across
D2 countries). This is the direct empirical answer to the Hudson & Moscoso
Boedo eq.-6 construction, which removes this component by design.

### H2 — Geometry of the change (age-graded front) — fine tier (D2/HFD panel; prior exposure disclosed)

**Claim.** Within countries, the onset of accelerated decline is age-graded:
break years rise with age (young-first diffusion, model M2), rather than
constant in age (M1, pure period shock) or rising one-for-one (M3,
cohort-carried).

**Procedure.** For each country in the fine tier and each single year of age
16–39 (ages ≥ 40 excluded a priori: near-ceiling levels make break dating
unstable), estimate the break year of log ASFR within 1995–2015 (Bai–Perron
single-break grid search). For each country, regress estimated break year on
age (WLS, weights = inverse break-date variance). Let β be the slope, with
95% CI.

**Country classification.** M1 if CI includes 0 and excludes 1. M3 if CI
includes 1 and excludes 0. M2 if CI excludes BOTH 0 and 1 and 0 < β̂ < 1
(diffusion velocity = 1/β̂ years of age per calendar year). INDETERMINATE
otherwise, or if no coherent front exists (fewer than [DECISION: proposed
12] of 24 ages yield a dateable break).

**Panel decision rule.** H2 is SUPPORTED if ≥ [DECISION: proposed 60%] of
classifiable countries are M2; NOT SUPPORTED if M1 or M3 pluralities;
reported descriptively otherwise. Country classifications use
Benjamini–Hochberg FDR at q = 0.05 across countries for the CI-based calls.

**Tempo-mimicry co-primary (downgrade rule).** A postponement wave
mechanically mimics M2. For each M2-classified country we therefore test a
quantum component on cumulative cohort trajectories: CPCFR at age 30 (and
35) for cohorts reaching that age post-onset must fall relative to the last
three pre-onset cohorts by ≥ [DECISION: proposed 0.05 children] for at least
three consecutive cohorts. M2 classifications failing this are downgraded to
"M2-tempo" (front reflects timing, not quantum) and H2's support tally
counts only quantum-confirmed M2 countries. [Note: this check runs on
cohorts observable within the 1990–2019 window; its censoring limits are
reported.]

### H3 — Gradient (secondary): onset tracks adoption, not development

**Claim.** Cross-country variation in estimated onset (from H1/H2 machinery)
is predicted by adoption timing τ_c (D3), and NOT by development (D4)
conditional on adoption.

**Test.** Regress country onset on τ_c and HDI-2007 (and GDP alternative).
SUPPORTED if (i) the τ_c coefficient is positive, p < 0.05; and (ii) the
development coefficient conditional on τ_c is equivalent to zero by TOST
with bounds ± [DECISION: proposed 1.5 years of onset per 1 SD of HDI].
Collinearity between adoption and development is reported (VIF); if VIF > 10
the test is reported as non-identified rather than forced.

## 6. Analysis plan — order of operations

1. Freeze this protocol; register on OSF; THEN download D1/D3/D4 (access
   dates stamped by commit).
2. Code the D2 registration subsample from the Data Sources inventory
   (before any outcome data are plotted); commit the list.
3. Run confirmatory H1 (D2 primary, D1 secondary), with mean-reversion
   guard and uniform-vs-differential decomposition.
4. Run confirmatory H2 on the fine tier; apply the tempo downgrade rule.
5. Run secondary H3 and S6 age-margin fingerprint (coarse + fine).
6. Produce exploratory S4 fingerprint surfaces (all countries, ordered by
   development), 2020–2024 marked.
7. Vintage sensitivity (D5) for every confirmatory result.
8. Robustness battery (§7).

## 7. Robustness battery (pre-specified; reported in full)

- S1 fixed-2008 slope-shift baseline (transparent but crude; reported for
  comparability with public debate).
- Alternative break windows (1993–2017); alternative trend forms (quadratic
  pre-trend).
- GFR and 25+ aggregate rate as alternative outcomes to TFR (direct
  engagement with the focal paper's 25+ claim).
- Level vs log outcomes.
- High-income-only vs full-panel replications of H1.
- No-recession-countries contrast: countries without a 2008–09 real-GDP
  contraction (World Bank WDI) — did they break anyway? [DECISION: status]
- Excluding countries whose adoption date is within ±1 year of 2008 (crisis
  de-confounding subset).

## 8. Multiple testing and inference summary

Hypothesis-level tests (H1, H2 panel rule, H3) are three pre-specified
families; no cross-family correction. Within-family country-level calls use
BH-FDR q = 0.05. All CIs 95%. Permutation seeds fixed and committed.
Results language: "supported / partially supported / not supported" per the
rules above; no post-hoc re-labelling.

## 9. Software and reproducibility

R (≥ 4.1); `strucchange`/`mbreaks` for break estimation; `wpp2024`,
`wpp2022` data packages; all code in the public repository; every
confirmatory number generated by scripted pipeline (no interactive
estimation). AI-assisted code development disclosed as in §1.

## 10. Deviations policy and versioning

Any deviation from this protocol is logged in
`manuscript_2026/universality_deviations.md` (date, what, why, and whether
decided before or after seeing results), and material deviations trigger an
OSF registration amendment (new linked registration; original remains
immutable). The repo's commit history provides the fine-grained audit trail.
Preprint versions of the eventual paper will cite the registration DOI and
state conformity/deviations in a dedicated subsection.

## 11. Open decisions to resolve before freezing [all JM]

- [DECISION: §3] Minimum population threshold (proposed 500,000 at 2010).
- [DECISION: §5-H2] Minimum dateable ages for a country to be classifiable
  (proposed 12 of 24); panel support threshold (proposed ≥60% of
  classifiable countries M2).
- [DECISION: §5-H2] Quantum threshold for the tempo downgrade (proposed
  CPCFR-at-30 fall ≥ 0.05 children over ≥3 consecutive cohorts).
- [DECISION: §5-H3] TOST equivalence bound for the development null
  (proposed ±1.5 years onset per 1 SD HDI).
- [DECISION: §7] Status of the no-recession contrast: robustness (current)
  or promoted to secondary hypothesis.
- [DECISION: venue] Demographic Research descriptive-finding vs full
  research article — affects reporting length, not analysis.
