# Comparative Fertility

Analysis and visualization codebase behind:

> Pattaro, S., Vanderbloemen, L. and Minton, J. (2020).
> **Visualizing fertility trends for 45 countries using composite lattice plots.**
> *Demographic Research* 42(23): 689–712.
> doi: [10.4054/DemRes.2020.42.23](https://doi.org/10.4054/DemRes.2020.42.23)

…and its in-progress 2026 update (see [`UPDATE_WORKPLAN.md`](UPDATE_WORKPLAN.md)).
Companion interactive app: <https://datascapes.shinyapps.io/cumulative_fertility_app/>.

> **Following the link from the 2020 paper?** The repository exactly as it stood
> at publication is preserved at the tag
> [`demres-2020`](../../releases/tag/demres-2020) — everything after that tag is
> the 2026 update. The published pipeline is also kept unmodified in the current
> tree (see *Repo structure* below).

## Replicating the analyses: what to download, from where

Raw source data are **not** redistributed in this repo (HFD/HFC terms); the
directories below are git-ignored. To rebuild everything from source:

1. **Human Fertility Database (HFD)** — <https://www.humanfertility.org>
   - Register (free) and log in.
   - Go to **Data → Zipped Data Files** and download the **"All types of HFD
     data"** bundle (`HFD.zip`, ~37 MB).
   - Extract into `data/hfd_2026/`. The pipeline needs `asfrRR.txt` (period
     age-specific fertility rates, Lexis squares); the bundle's
     `birthsRR.txt`/`exposRR.txt` support the alternative rates-from-counts
     route, and `cpfrRR.txt`/`ccfrVH.txt` are useful cross-checks on our own
     cumulative calculations.
2. **Human Fertility Collection (HFC)** — <https://www.fertilitydata.org>
   - From **Zipped data files**, download **"ASFR and CPFR, standardized age
     scale → All birth orders combined, females"** (`HFC_ASFRstand_TOT.zip`).
   - Optionally also **"By birth order, females"** (`HFC_ASFRstand_BO.zip`) —
     unused by the current pipeline, held for the birth-order extension (below).
   - Extract into `data/hfc_2026/`.
3. **Run the 2026 pipeline** (R; tested on R 4.1 with readr/dplyr/tidyr/
   lattice/viridis — no latticeExtra required):

   ```sh
   Rscript scripts/ingest_2026.R    # raw files -> data/data_combined_and_standardised_2026.csv
   Rscript scripts/figures_2026.R   # published figure set on the new panel
   Rscript scripts/wall_2026.R      # Phase 3: fertility-ceiling / compression analysis
   ```

To reproduce the **published 2020 figures** no download is needed: the frozen
2016 combined dataset (`data/data_combined_and_standardised.csv`) is committed,
and `scripts/two_contour_version_of_figures.R` is the script that produced the
figures in the paper (kept byte-for-byte as published; the 2026 scripts never
modify it).

## The paper

The paper introduces a refinement of the Lexis surface for fertility: **level
plots** (colour = age-specific fertility rate, ASFR) composed with **contour
lines** marking cumulative pseudo-cohort fertility (CPCFR) milestones — 2.05
(replacement) and 1.50 — so that period, age, cohort and cumulative-milestone
information are readable in a single panel, small-multiplied across 45
countries. It appeared in Demographic Research's Special Issue on Demographic
Data Visualization.

Headline empirical result (2020): once a country's cohorts fall below
replacement (CPCFR < 2.05) they tend not to return; Norway and the USA were the
two exceptions. **The 2026 update reverses the headline**: with data extended
from 2014–15 to 2023–25, both exceptions have collapsed (Norway TFR 1.98 in
2009 → 1.40 in 2023; USA → ~1.6 in 2024), and the updated Norway panel shows
the 2.05 contour returning to the surface for cohorts born 1958–68 and then
escaping vertically — permanently, on current data — for cohorts born after
~1972.

## The app

`cumulative_fertility_app/` is a Shiny app (deployed at the shinyapps.io link
above) that lets users build their own fertility Lexis surfaces per country:
level-plot shading, milestone contours, period gridlines, linked
tooltips/cross-sections, and a 3D surface view. Tooling note: the static
composites use `lattice`/`latticeExtra` (`produce_composite_lattice()`), while
the interactive surfaces are `plotly` — the app never used ggplot2. It still
runs on the 2016 data; refreshing its data and redeploying is a pending step of
the 2026 update (Phase 4).

## The 2026 update

Tracked in [`UPDATE_WORKPLAN.md`](UPDATE_WORKPLAN.md) (the living plan, decision
log, and handoff document). Status at 2026-07-11: data refresh (Phase 1),
regenerated core figures (Phase 2), and the fertility-ceiling analysis
(Phase 3) are done; app refresh and debate-facing outputs (Phase 4) and a
Quarto/BibTeX manuscript modernization are planned. Key new artifacts:

- `reports/validation_2026.md` — new-vs-old build comparison (all discrepancies
  explained: HFD back-revisions, one code-mapping fix that restores France
  post-2008, source switches for Korea)
- `figures/figures_2026/` — the published figure set on the new panel, plus
  ceiling-overlay and reachability figures
- `reports/wall_analysis_2026.md` — the "wall" headline numbers: the effective
  ceiling of the fertility lifecourse (highest age with ASFR ≥ 1/200) fell from
  ~45 (1950s) to ~41 (1980s–90s) and has crept back only to ~43 (2020s), while
  cumulative fertility above age 40 remains ~0.03–0.07 children — so rising
  ages at first birth compress replacement into a narrowing corridor ending in
  the early 40s. 43 of 45 countries have post-war cohorts observed past age 44
  that never reached 2.05.

## Repo structure

```
├── UPDATE_WORKPLAN.md            living plan + decision log — start here
├── reports/                      machine-generated review documents
├── data/
│   ├── hfd_2026/, hfc_2026/      raw 2026 downloads (git-ignored; see above)
│   ├── hfc/                      2016-era HFC remnants + code_definitions.csv (shared lookup)
│   ├── data_combined_and_standardised.csv       2016 build — FROZEN (published paper)
│   ├── data_combined_and_standardised_2026.csv  2026 build (+ hfd/hfc/interp source column)
│   └── derived_2026/             analysis tables (ceiling, late-mass, reachability)
├── scripts/
│   ├── (2016–2020 scripts)       FROZEN — reproduce the published paper
│   ├── ingest_2026.R             Phase 1: raw -> combined CSV
│   ├── functions_2026.R          shared data prep + composite plot function
│   ├── figures_2026.R            Phase 2: published figure set, new data
│   └── wall_2026.R               Phase 3: ceiling / compression / reachability
├── figures/
│   ├── (2016–2020 directories)   FROZEN
│   └── figures_2026/             all update figures
├── manuscript/                   published paper history, incl. peer-review rounds — FROZEN
├── cumulative_fertility_app/     Shiny app (Phase 4: data refresh + redeploy)
└── poster/, presentation/, stl/, support/, script.R, …   2016–19 working archive
```

Architecture rule: the published layer is never edited; every update artifact
carries a `_2026` suffix or lives in a `_2026`/`reports` directory. Data flows
one way: raw (ignored) → ingest → combined CSV → shared prep
(`functions_2026.R`) → figures + derived tables + human-readable reports.

## Has anyone used the method? (uptake since 2020)

Honestly: not much. Semantic Scholar (July 2026) records **2 citations, 0
influential**: the special issue's own
[editorial](https://ideas.repec.org/a/dem/demres/v44y2021i36.html) (2021), and —
the one direct methodological descendant — *"Visualising Age-Specific Fertility
Patterns in the Former German Democratic Republic (GDR) and in East Germany,
1956–2024"* (Comparative Population Studies, 2026), which applies Lexis-surface
visualization to the East German case. (Google Scholar typically indexes a few
more than Semantic Scholar; worth a manual check. Whether the CPS paper is
independent of the original authors has not been verified.) The paper also
appears in the HFD's official [list of publications using HFD/HFC
data](https://www.humanfertility.org/File/GetDocumentFree/Docs/publications.pdf).

## Fertility research and discussion since 2020

The update lands in a very different conversation from the one the paper
entered. Since publication: record-low fertility in Norway (1.40, 2023) and the
Nordics generally, a record-low US TFR (~1.6, 2024), South Korea's descent to
~0.72, a COVID-era fluctuation now visible in the HFD's monthly STFF series,
and a lively public debate about *accelerating* decline — including the
"connectivity/smartphone" hypothesis (Burn-Murdoch's FT columns, Alice Evans's
essays) and its skeptics (a 4G-rollout study finding detectable effects only
among teens). Academic anchors include Kearney, Levine & Pardue's "puzzle of
falling US birth rates," Hellstrand/Nisén/Myrskylä and Comolli et al. on the
Nordic decline, Yoo & Sobotka on tempo in ultra-low-fertility Korea, and
Beaujouan on latest-late fertility. The update's stance (see workplan): the
composite plots contribute *stylized facts any explanation must match* — which
ages, which cohorts, what timing, tempo or quantum — rather than adjudicating
between co-timed candidate causes.

## Birth order: the unexecuted extension

No written plans for a birth-order analysis exist anywhere in the repo or the
manuscript history — but the data has now been downloaded for it twice
(`HFC_ASFRstand_BO` in 2016, again in 2026, plus the HFD bundle's `*bo.txt`
files), which records an intention if not a plan. It is the natural next
analytical step: parity-specific composite surfaces would separate the
postponement of *first* births (where the compression against the fertility
ceiling binds hardest) from progression to second and higher-order births
(where Zeman et al.'s 2018 decomposition locates much of the recent cohort
decline). The Phase 3 machinery (`wall_2026.R`) generalizes to parity-specific
ASFRs without structural change.
