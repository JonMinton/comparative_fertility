# Literature notes — candidate paper 3 (universality test)

*Status: IN PROGRESS 2026-07-11 — being assembled from four parallel research
tracks. Structure below; sections fill as tracks complete. Companion to
`universality_strategy.md` (strategies S1–S8 and objections). All searches
conducted under the standing phrasing rule: human fertility/demography terms
only.*

## 1. The focal study: Hudson & Moscoso Boedo (SSRN, 2026)

*Track complete 2026-07-11 — reconstructed from the FULL PDF (openly hosted
on the corresponding author's UC homepage; no SSRN login needed; local
copies + text extracts in `manuscript_2026/literature_local/`, git-ignored).*

**Hudson, N. & Moscoso Boedo, H.J. (2026). "The Collapse of Teen Fertility in
the Digital Era." WP dated 25 Apr 2026, Univ. of Cincinnati economics; SSRN
abstract 6676839; ~55pp.** PDF: homepages.uc.edu/~moscoshn → papers/Smartphone_web.pdf

**Thesis:** teen fertility "collapsed globally starting around 2007" across
the income/policy spectrum; mechanism is a coordination/tipping model — once
enough teens are on the phone, the peer network IS the phone; in-person time
falls, and with it "the unstructured contact in which most unintended teen
conceptions occur."

**Data:** NCHS natality (ASFRs 1990–2023; county teen rates 2003–2020 — NB
these are Khan–Rossen *Bayesian-smoothed* estimates for 3,136 counties); FCC
Form 477 broadband tiers + county LTE coverage (zero-filled pre-Dec-2010);
Pew; ATUS time diaries; UN WPP 2024 ASFRs for the 128-country panel; ONS
under-18 conceptions for 294 E&W local authorities + Ofcom 4G; CDC WONDER for
the suicide flip-side; iPhone launch dates hand-compiled from Apple press
releases. Cross-country treatment year τ_c = max(iPhone launch year, year
crossing 80 mobile subscriptions/100).

**Estimators — NOT a staggered DiD** (our objection 3 largely misses):
1. Cross-sectional 2SLS: terrain-ruggedness IV (following Akerman, Gaarder &
   Mogstad 2015) on county broadband/LTE; outcome Δlog teen birth rate
   2003–18; income/Gini deliberately excluded as bad controls; first-stage
   F 73–82.
2. Within-county first-difference distributed-lag panel + pooled
   cointegration.
3. E&W replication, same FD design; plus a Chow test on log teen pregnancy
   (single-trend rejected, F=131.5).

**The "detrended" definition (their eq. 6) — the load-bearing detail:**
rel(c,a,t) = 3-year-centered log-linear growth of a country's *age-specific*
rate MINUS the same country's *crude-birth-rate* growth. They present this as
netting out "anything that moves all ages together — recessions, family
policies, contraception-access changes." **⚠ By the same construction, it
also nets out any true UNIFORM effect of the treatment itself.** See §6.

**Magnitudes:** US 2007–24 birth-rate change by age: 15–19 −71%, 20–24 −43%,
25–29 −23%, 30–34 ≈ −1%, 35–39 +9%. Teen-pregnancy annual decline doubles
post-2007 (4.9%→9.8%/yr; UK 6.8×). IV: +10pp broadband → 18.9pp larger
teen-birth decline; within-county panel attributes only ~4–5% of the 70%
aggregate teen decline to broadband rollout narrowly (they stress IV and
panel identify different parameters). Suicide flip-side: +10pp 4G → +2.7–4.0
teen suicides/100k. They run Bongaarts–Feeney themselves: ~half the
partial-TFR fall is tempo, half quantum. Abortion-ratio-stable placebo:
the decline is in conceptions.

**Framing:** deliberately modest — smartphones "accelerated a decline
already underway… rather than caused the decline ab initio"; Kearney–Levine
contraception story accepted for 1991–2005.

**Objections audit (vs the six we logged in `universality_strategy.md`):**
1. Detrending choice — STANDS, and sharper than we knew (see §6).
2. Rollout endogeneity — partly addressed (terrain IV, not raw timing), but
   terrain-IV has its own exclusion worries (ruggedness → many channels;
   they exclude income as a bad control, which is also excluding a
   confounder pathway).
3. Staggered-DiD pathologies — largely MOOT (not their design); retarget the
   critique kit at the FD/distributed-lag and IV designs instead.
4. Teen-specific confounds (LARC etc.) — partially addressed via
   abortion-ratio placebo and by accepting the pre-2007 contraception story;
   still live for the post-2007 window.
5. Ecological exposure — STANDS (coverage ≠ use; Reason's second piece
   presses exactly this against Myers & Hooper).
6. Two rich countries — SUPERSEDED: they DO run 128 countries, but teen-only
   and WPP-based (so our §4d smoothing objection applies to their
   cross-country module too — WPP ASFRs are Bayesian model output).

**Companion:** Moscoso Boedo & Hudson (2026), "Wide and Shallow" (SSRN
6749621; PDF June 2026): calibrated household-production model; freezing the
relative phone price at 2007 "accounts for 43%" of the US children-per-adult
decline; cohort-Bartik: full LTE exposure at ages 18–25 → −0.34 female
ever-married rate (1985–95 cohorts). NB: UC's press page misnames it "The
Fall of Fertility in the Digital Era."

## 2. Connectivity–fertility empirics: where the signs disagree

*Track complete 2026-07-11; citations verified except where flagged.*

### 2a. The sign conflict, in one paragraph

Connectivity's estimated fertility effects are **age- and subgroup-specific,
not universal**: NEGATIVE for teens/young adults and low-resource populations
(information, contraception, displaced in-person interaction), POSITIVE for
older highly educated women in rich countries (telework/work-family
channel). The focal paper's own 25+ null fits this pattern. Any
"universality" claim must therefore be precise about WHICH margin is
universal — this is exactly the S6/S8 age-fingerprint territory.

### 2b. The studies

- **Billari, Giuntella & Stella (2019). Does broadband Internet affect
  fertility? *Population Studies* 73(3): 297–316.** German SOEP; IV from
  historical telephone infrastructure. **POSITIVE**, but only for highly
  educated women 25+; insignificant for men, low-educated, under-25s.
  Mechanism: telework/part-time reconciliation. ("Digital divide in
  fertility.")
- **Guldi & Herbst (2017). *J. Population Economics* 30(1): 69–91.** US
  county broadband rollout 1999–2007. **NEGATIVE** for teen births (≥7% of
  the teen decline; note the WP version said 13% — cite the published 7%).
- **Rotondi, Kashyap, Pesando, Spinelli & Billari (2020). *PNAS* 117(24):
  13413–13420.** DHS microdata, 7 sub-Saharan countries + macro panels.
  Phone ownership → higher modern contraceptive uptake; fertility-REDUCING
  direction, strongest among the most disadvantaged.
- **Billari, Rotondi & Trinitapoli (2020). Mobile phones, digital inequality,
  and fertility. *Demographic Research* 42(37): 1057–1096.** Malawi
  longitudinal; phone ownership → smaller ideal family size, lower parity.
  **NEGATIVE.** (Also: a DR home-journal precedent for this topic.)
- **Kearney & Levine (2015). *AER* 105(12): 3597–3632.** *16 and Pregnant*
  viewership → **−4.3%** teen births. **DISPUTED in print**: Jaeger, Joyce &
  Kaestner (2020), *JBES* 38(2): 317–326 (placebo tests fail the exclusion
  restriction); K&L rejoinder same issue. The only formal published
  replication challenge in this literature so far.
- **Nie, Peng & Luo (2023). *China Economic Review* 77: 101903.** CFPS
  2014–18; internet use → fewer births via marital satisfaction, gender-role
  attitudes, preferences. **NEGATIVE.**
- **Wildeman, Schrijner & Smits (2023). *Population, Space and Place* 29(4),
  e2635 [article no. flagged unverified].** Facebook-trace usage vs crude
  birth rates, 311 subnational regions, 29 SSA countries. **NEGATIVE.**
- **Myers & Hooper (2026). Is the iPhone Birth Control? NBER WP 35310, June
  2026** [working paper]. AT&T's 2007–11 iPhone carrier exclusivity ×
  coverage as natural experiment: births −4.5–8.0% (15–19), −3.2–6.6%
  (20–24), smaller significant declines at older ages; **iPhone diffusion
  explains 33–52% of the US general-fertility-rate decline.** The boldest
  magnitude claim in the literature; not yet peer-reviewed.
- Unpublished pipeline [details unverified]: 3G-rollout studies for Nigeria /
  SSA (PAA 2025 etc.), all fertility-reducing among adolescents.

### 2c. The focal cluster (verified identities)

- **Hudson & Moscoso Boedo (2026). "The Collapse of Teen Fertility in the
  Digital Era." Working paper, 25 April 2026, Univ. of Cincinnati; SSRN
  abstract 6676839.** Verified from the PDF title page. Contents: documents a
  **synchronous post-2007 break in teen (15–19) birth-rate growth across 128
  countries, re-centered on each country's smartphone-shock onset**; US
  identification via terrain-ruggedness IV on broadband/4G coverage; parallel
  England & Wales design (teen conceptions); a teen-suicide sign-flip test on
  the same instrument. Self-caveat: 25+ shows "no detrended response in the
  typical country."
- **Moscoso Boedo & Hudson (2026). "Wide and Shallow: Digital Technology and
  the Post-2007 Fertility Decline." SSRN 6749621, May 2026** [posting date
  unverified]. Companion structural search/matching model ("broad and
  shallow" digital connections) calibrated to US 2007–24; counterfactual
  holding relative phone prices at 2007 explains **43%** of the US fertility
  change; cohort-Bartik design on 4G for partnership formation.
- **Published academic critiques/replications: NONE found as of July 2026.**
  Only media/expert commentary (see section 5). The formal-critique niche is
  open.

### 2d. Consequences for paper 3's positioning

1. **They have already run a teen-only version of our S2/S3** (128-country
   synchronous break, adoption-recentered). Paper 3's niche is therefore NOT
   "first synchronization test" but: (i) the FULL age range, all-ages
   fertility, where their null is asserted but (per their own framing)
   only "detrended" — the detrending audit matters doubly now; (ii) the S8
   front geometry within the age dimension (they treat age groups as
   separate series, not as a field); (iii) demographic-accounting rigor
   (tempo/quantum, cumulative milestones) absent from the economics designs.
2. The Myers & Hooper 33–52% claim vs Hudson & Moscoso Boedo's teen-only
   caveat is a live *within-camp* tension a careful paper can exploit: if
   the iPhone explains a third-to-half of the ALL-ages GFR decline, the 25+
   margin cannot be null — the two working papers cannot both be right as
   stated.
3. No formal replication/comment exists on either — a peer-reviewed,
   preregistered, demographically disciplined test would be first into that
   space.

## 3. The diffusion tradition and synchronization literature

*Track complete 2026-07-11; all citations verified against publisher pages /
Crossref except where flagged.*

### 3a. Diffusion-of-fertility-decline (S8's intellectual ancestry)

- **Coale & Watkins (eds.) (1986). *The Decline of Fertility in Europe*.
  Princeton UP.** Princeton Project synthesis: decline spread along
  cultural/linguistic lines largely independent of local development — the
  canonical evidence that fertility decline behaves as a diffusing innovation.
- **Bongaarts & Watkins (1996). Social interactions and contemporary
  fertility transitions. *PDR* 22(4): 639–682.** Transitions occur at
  progressively lower development thresholds over calendar time; social
  interaction (local networks + global channels) accelerates them — the
  classic mechanism by which a common global signal could produce
  near-simultaneous responses across development levels. *Arguably the single
  most important prior for this paper.*
- **Casterline (ed.) (2001). *Diffusion Processes and Fertility Transition*.
  National Academy Press.** The standard reference for what "diffusion"
  formally means (ideational change transmitted through interaction).
- **Rosero-Bixby & Casterline (1993). Modelling diffusion effects in
  fertility transition. *Population Studies* 47(1): 147–167** (formal
  contagion terms alongside demand/supply) **and (1994) *Social Forces*
  73(2): 435–462** (spatial contagion across 100 Costa Rican counties, net of
  structural effects). The methodological template for separating
  diffusion-driven synchrony from common structural causation — S8's
  front-velocity idea is this tradition transposed to the age × period field.

### 3b. Convergence / synchronization benchmarks

- **Wilson (2001). On the scale of global demographic convergence 1950–2000.
  *PDR* 27(1): 155–171** (+ follow-up *PDR* 37(2), 2011). Convergence as the
  dominant late-20th-century fact.
- **Dorius (2008). Global demographic convergence? *PDR* 34(3): 519–537.**
  Population-weighted intercountry fertility inequality didn't start falling
  until ≥1995 — the immediate pre-2008 dispersion benchmark against which
  post-2008 synchronization gets measured.

### 3c. The development–fertility reversal and its fate (the foil)

- **Myrskylä, Kohler & Billari (2009). Advances in development reverse
  fertility declines. *Nature* 460: 741–743.** The J-shape: above HDI ~0.86,
  fertility rises again (data to 2005). Rich countries looked like they'd
  entered a rebound regime just before 2008.
- **Harttgen & Vollmer (2014). *Demography* 51(1): 173–184.** The reversal is
  fragile — disappears for the 2000s with revised HDI; largely
  education/tempo components.
- **Gaddy (2021). A decade of TFR declines suggests no relationship between
  development and sub-replacement fertility rebounds. *Demographic Research*
  44(5): 125–142.** Direct update to 2017: the rebound is "no longer
  supported." **The cleanest citation that the reversal died exactly over the
  window when decline re-synchronized — the paper's foil in one reference.**

### 3d. Post-2010 decline is near-universal (authoritative documentation)

- **GBD 2021 Fertility Collaborators (2024). *The Lancet* 403(10440):
  2057–2099.** Global TFR ~2.2 by 2021; >half of countries below replacement;
  declines across middle- and most low-income settings.
- **UN DESA Population Division (2024). *World Population Prospects 2024:
  Summary of Results*.** Global TFR 2.25 (2024); 131/237 countries below 2.1;
  WPP2024 itself revised fertility *downward* vs earlier revisions on
  faster-than-projected middle-income declines. [Document symbol unverified;
  PDF at population.un.org/wpp/assets/Files/WPP2024_Summary-of-Results.pdf]

## 4. Methods: breaks, staggered DiD critiques, tempo, WPP construction

*Track complete 2026-07-11; citations verified against publishers/RePEc; the
three UN documents were downloaded and read directly.*

### 4a. Structural breaks (for S2)

- **Bai & Perron (1998). *Econometrica* 66(1): 47–78** (framework: unknown
  number of breaks at unknown dates, sup-F and sequential tests) and
  **(2003). *J. Applied Econometrics* 18(1): 1–22** (computation; what R
  `strucchange`/`mbreaks` implement — cite this for the procedure).
- **Bai (2010). Common breaks in means and variances for panel data.
  *J. Econometrics* 157(1): 78–92.** A break date common to a panel of N
  series is estimable far more precisely than from any single series — the
  formal justification for pooling countries to test whether ~2008–12 is a
  COMMON break vs idiosyncratic. Upgrades S2 from histogram-reading to
  estimation.

### 4b. Staggered DiD critique kit (for auditing the focal study)

All three in the same themed issue, *J. Econometrics* 225(2), 2021:
**Goodman-Bacon** (254–277; TWFE decomposition — already-treated units as
controls can sign-reverse estimates), **Callaway & Sant'Anna** (200–230;
ATT(g,t), the "what should have been done" reference), **Sun & Abraham**
(175–199; event-study lead/lag contamination — the citation if the focal
paper shows pre-trend plots from a staggered design).

### 4c. Tempo/quantum (for the S8 mimicry check)

- **Bongaarts & Feeney (1998). *PDR* 24(2): 271–291** — canonical adjTFR.
- **Ní Bhrolcháin (2011). Tempo and the TFR. *Demography* 48(3): 841–861** —
  strongest modern critique (adjusted TFR lacks coherent interpretation).
- Early comments: **van Imhoff & Keilman (2000)** and **Kim & Schoen (2000)**,
  both *PDR* 26(3) — constant-shape and uniform-shift assumptions fail.

### 4d. WPP construction — the finding that changes the design

- **UN DESA (2024). *WPP 2024: Methodology* (UN DESA/POP/2024/DC/NO.10).**
  Read directly (Section I.B, pp. 6–8): **Bayesian hierarchical models
  produce the annual TFR and ASFR series for ALL countries — every published
  country-year is model output, even where registration is complete.**
  Vital registration enters only at ≥60% completeness; series are treated as
  unbiased only at ≥98% completeness since 1950. ASFRs from a separate
  logit-scale Bayesian model (Chao et al. 2023). Figure I.3 overlays the 2024
  vs 2022 revisions — official evidence of vintage instability.
- **UN DESA (2024). *WPP 2024: Data Sources* (DC/NO.11, ~280 pp.).**
  Country-by-country prose inventory of fertility inputs and years —
  registration vs survey vs indirect IS documented, but not machine-readable;
  a coding pass would be needed to build the registration-based subsample
  flag. Machine-readable partial metadata exists (population.un.org/wpp
  metadata documentation).
- **UN DESA (2024). *WPP 2024 Release Note*** — country-level 2022→2024
  revision explanations (mostly fertility-driven). No single UN document
  tabulates TFR differences across all revisions — cross-vintage comparison
  is feasible via the `wpp2024`/`wpp2022`/`wpp2019` R packages (github/PPgp),
  and this gap is itself worth one sentence in the paper.
- **Alkema, Raftery et al. (2011). *Demography* 48(3): 815–839** — the
  three-phase Bayesian hierarchical TFR model; and **Liu & Raftery (2020).
  *Ann. Applied Statistics* 14(2): 685–705** — the past-TFR
  bias-adjustment/uncertainty model WPP2024 actually cites for estimation
  (the more relevant cite when critiquing WPP-as-observations); software
  `bayesTFR`.

**Design consequence (cross-cutting):** treating WPP country-years as
observations understates uncertainty and induces smoothing that can attenuate
or displace structural breaks — so (i) the S2/S8 fine analysis must run on
registration-based national series (HFD panel + a coded registration
subsample from the Data Sources inventory), with WPP reserved for the coarse
global fingerprint, clearly labelled as model-based; (ii) a two-vintage
sensitivity (wpp2022 vs wpp2024 packages) is cheap and pre-registrable;
(iii) the preregistration must name the vintage AND the smoothing caveat
ex ante.

## 5. The grey/press layer

*Track complete 2026-07-11. FT full text NOT retrievable (blocks crawlers) —
claims below reconstructed from secondary coverage and flagged as such. NBC
piece paywalled after the lede. 🔴 If wanted: a human FT/NBC subscriber can
pull both; neither blocks the phase.*

- **Burn-Murdoch, FT, ~mid-May 2026** (URL known:
  ft.com/content/fba35eca-df3a-4ad6-b42d-eb08eb7c9ad3). Via secondary
  coverage: re-centering each country's births on the year smartphones took
  off makes "every decline look identical" (US/UK 2007; France/Poland 2009;
  Mexico/Morocco/Indonesia 2012; Ghana/Nigeria/Senegal 2013–15); effect
  intensifies at younger ages; the decline is mainly fewer couples forming,
  not smaller families. Note: this re-centering IS our S3 in journalistic
  form — the paper formalizes what the viral chart eyeballs.
- **Burn-Murdoch, FT, Jan 2025, "The relationship recession is going
  global"** — coupling decline from Finland to South Korea to Tunisia;
  singledom rising with mobile-internet use, especially among women; rare
  where fewer women are online. His stated position (via Marginal
  Revolution): phones are "a technological shock that amplifies/accelerates
  the old mechanism (cultural change)."
- **Critiques:** Reason (E.N. Brown, 18 May + 10 Jun 2026; Wolfe 11 Jun):
  centuries-long decline; rollout endogeneity; the 25+-null-quoted-back
  move; vs Myers–Hooper specifically: coverage ≠ ownership (only 23% of
  teens had smartphones by 2011), null for Black women, declines in
  low-coverage areas too. **UnHerd (Cohen & Tennant, 19 May 2026)** — the
  sharpest counterfactual critique in print: the viral chart "strips away
  background decline trends," so what remains is attributed to phones by
  construction; demands individual-level data and peer review. **CNN (Jun
  2026):** Sarah Hayford (Ohio State) — a century-long trend can't be
  explained by a 15-year-old technology; IUD/injectable access maps better
  onto the US teen decline.
- **Defenses/ecosystem:** Brookes, "Yes, It Is The Phones" (Persistent
  Ruminator substack) — post-2007 is a NEW decline period, so "already
  falling" is not a rebuttal; Noah Smith ("it's the phones", missing
  "emotional nutrient" line); Rob Brooks ("Smartphones are contraceptives").
  **Alice Evans** — substack *The Great Gender Divergence*; key essays "Why
  is Fertility Collapsing, Globally?" (~Oct 2024), "The Global Collapse of
  Coupling & Fertility" (~early 2025): coupling collapse driven by
  personalized online entertainment + smartphones raising women's
  expectations fastest in gender-traditional cultures.
- **Disambiguation for citations:** the June 2026 media wave (CNN/NBC/Reason
  #2) is about **Myers & Hooper** (iPhone/AT&T, 33–52% of US GFR decline;
  SSRN 6897299 per track 4 / NBER WP 35310 per track 1 — [reconcile the two
  IDs before citing]). The May wave is about **Hudson & Moscoso Boedo**
  (teen collapse; their analogous headline number is the companion paper's
  43%, and their narrow within-county estimate is ~4–5% of the teen
  decline). Do not conflate the attribution numbers.

## 6. Implications for the preregistration (synthesis)

1. **The detrending catch (new, and central).** H&MB's "no detrended
   response for 25+" is computed as age-specific growth MINUS same-country
   crude-birth-rate growth. Any effect that is UNIFORM across ages is
   removed by construction; the statement "25+ shows no detrended response"
   is therefore consistent with BOTH "no effect on 25+" AND "a large effect
   common to all ages." Their teen-specificity claim rests on an estimator
   that cannot, even in principle, detect a common component. ⇒ Paper 3
   should estimate the LEVEL response at every age (the field, not
   deviations from the field's mean) — precisely what S8 does. This single
   observation justifies the paper.
   **VERIFIED against the PDF (eq. 6, and their fig-1B caption/text):**
   rel(c,a,t) = g^ASFR(c,a,t) − g^CBR(c,t), described by the authors as
   "isolating the age signature… from country-level fertility movements
   common to all ages." Stronger still: women 25+ produce ~80% of births, so
   g^CBR is dominated by the 25+ ages themselves — the 25+ detrended series
   is near-tautologically ≈ 0 (a series minus approximately itself). The
   "no detrended response for 25+" quote, which the entire press debate
   recycles in both directions, is close to true by construction.
2. **The within-camp inconsistency to adjudicate.** Myers & Hooper: iPhone
   diffusion explains 33–52% of the ALL-ages US GFR decline. H&MB: the 25+
   response is null. Both cannot be right as stated — unless the H&MB null
   is the detrending artifact of point 1, which would reconcile them. A
   pre-registered test distinguishing uniform from age-graded responses
   arbitrates a live dispute, not a straw man.
3. **Niche confirmed open.** No published academic critique or replication
   of either working paper exists (July 2026). H&MB already did
   teen-only, WPP-based, adoption-recentered synchronization across 128
   countries — so paper 3 is: all ages; the field geometry (M1/M2/M3 fronts
   — they treat age groups as separate series, never as a surface); tempo/
   quantum discipline (they ran B–F themselves — engage, don't ignore);
   and registration-based data where they used WPP model output (our §4d:
   WPP ASFRs are Bayesian-smoothed for ALL countries — their cross-country
   module inherits that smoothing, and smoothing can manufacture synchrony).
4. **Hypotheses to harden into the prereg** (drafting targets):
   - H1 (universality): a common break in all-ages fertility across
     countries, estimable by panel common-break methods (Bai 2010), located
     in [2008–2013], surviving the mean-reversion counterfactual (Gaddy/MKB
     context).
   - H2 (geometry): within-country fronts are age-graded (M2) with finite
     velocity, not vertical (M1) and not purely cohort (M3) — decision rule
     on the break-year-on-age slope, specified numerically.
   - H3 (gradient): front onset is related to adoption timing, not to
     development level conditional on adoption (S3/M4).
   - Each with the tempo-mimicry check (CPCFR milestones) as a co-primary
     robustness, and the uniform-vs-differential decomposition from point 1.
5. **Data decisions the prereg must fix** (from §4d): registration-based
   series for the fine analysis (HFD + a coded subsample from the WPP Data
   Sources inventory); WPP2024 (named vintage) for the coarse fingerprint
   only, labelled model-based; two-vintage sensitivity via wpp2022/wpp2024 R
   packages; adoption series = ITU subscriptions + iPhone launch dates
   (H&MB hand-compiled theirs from Apple press releases — rebuild or
   request); estimation window 1990–2019, COVID excluded from break search.
6. **Positioning sentence for the eventual paper:** complementary to, not
   competing with, the quasi-experimental literature: they identify local
   causal effects of specific technologies on teens; we characterize the
   global, all-ages, demographically-accounted structure any such mechanism
   must generate.
