# Restructure blueprint (Jon's spine directive, 2026-07-19; F-025)

Governs the v0.2 restructure of `closing_corridor_merged.qmd`. Supersedes the
v0.1 section order. Read together with F-018..F-024 (Jon's tracked-changes
round, same date).

## The three artifacts and how they coordinate

| # | Artifact | Role | Rule |
|---|----------|------|------|
| 1 | **Repo** (this repository + Shiny app) | The factory: pipeline, derived data, all figures, prereg protocol, agentic update provenance | Everything in (2) and (3) is generated from here; scripts named in captions/S-items |
| 2 | **Main manuscript** | One spine, progressed relentlessly; no asides/eddies | If a passage doesn't advance the current spine beat, it moves to (3) or Discussion |
| 3 | **Supplement** (`supplementary_materials.qmd`, S-numbered) | Additional materials from (1) that (2) cites parenthetically | MS references look like "(Supplement S3)" — never load-bearing for the argument's momentum |

## The spine (i)–(viii) → manuscript outline

Roadmap presents the eight beats as **three phases**, which also resolves the
three-vs-four contributions inconsistency (F-024a):
**A. Method and why it deserves a second look** (i–iii) ·
**B. The embedded forecasts, checked** (iv–v) ·
**C. Formalisation and the squeeze** (vi–viii).

| §  | Spine | Contents | Source material |
|----|-------|----------|-----------------|
| 1 Introduction | (i) | Renewed interest; continuing **and accelerating** decline in high-income nations, with dated sources at first mention (F-018): Ritchie/OWiD 2026, Burn-Murdoch/FT 2026, UN WPP 2024. Then the three-phase roadmap. Debate machinery does NOT live here. | Slimmed from current Introduction |
| 2 The visualisation method | (ii)+(iii) | 2020 codebase + composite lattice plots; how to read them (APC primer + US worked example, F-012). Then the uptake subsection: take-up was limited, likely due to (a) inherent complexity (fifth-encoding material, F-004) and (b) methodologists' suspicion that visualisation alone yields no clear predictions the way formal models do — which sets the stakes for §4 | Current §2 first half + Intro APC primer + F-004 note |
| 3 Data and methods | — | 2026 rebuild; reproduction-vs-replication; the update was possible because code, algorithm, sources were reproducible (agentic support stated factually; elaborated in Discussion). **Formal definitions move here** (F-023): ceiling age / age of no return, peak age & level, squeeze gap, corridor arithmetic — Results then apply, never define | Current §3 + definitions pulled back from Results II |
| 4 The 2020 forecasts, checked | (iv)+(v) | The pedagogic annotations DID contain predictions (verbatim §3.2 quotes). The 2×2 composite figure (F-021): rows Norway/USA, columns 2020-with-projection vs 2026-updated — double duty (re-teaches reading + states the falsifiable claims). Scoreboard table follows. Resolution: **qualitatively correct, quantitatively optimistic, for both** — with the which-part-held precision in prose (USA retention runs against the terminal ages; last cohort to replacement at 40 is 1984). Closing move: this should allay concerns about "reading off graphs" as a legitimate medium-term forecasting practice — answering §2(iii)(b) | Current §2 second half + Results I |
| 5 Beyond period TFR | (vi) | High-attention accounts are period-TFR-based; TFR collapses the age structure the debates need; the full Lexis surface finesses them. Ritchie and Burn-Murdoch engaged HERE (per F-021), not in the Introduction | Current Introduction's TFR argument, relocated |
| 6 Ceiling and peak: the squeeze | (vii)+(viii) | Slight formalisation of two surface features: the **ceiling age** ("age of no return", ANR) and the **peak fertility age & level**, presented on a single chart. Ceiling is behavioural and nearly fixed; the mass behind it is small (~0.05 above 40); grandmothering compressed to 2–3 sentences (F-015 citations + LV credit retained). The squeeze: peak 24→31, ceiling 45→41→43, gap 20→12, peak ASFR 0.19→0.11 — "later, lower, narrower" explains the §5 headline trends in more resolution. MS carries ONE distilled chart; the 45 panels go to Supplement (F-022) | Current Results II incl. squeeze subsection |
| 7 Discussion | — | Stylized facts recap; what the squeeze implies for the debates (light, not heat); compact COVID paragraph (F-009); "from surfaces to models" compressed to 1–2 paragraphs with prereg osf.io/j3tbq as the disciplined next step (detail → S5); limitations; workflow subsection sharpened per F-020: agentic scaffolds on **established codebases** (replication/update) as a safe, defensible first use, contrasted with from-scratch LLM drafting where neither understanding nor verification is assured | Current §6 compressed + §7 |
| 8 Conclusions | — | Rewritten to land the three phases | Current §8 |

## Supplement contents (S-numbered, generated from repo)

- **S1** Full 45-country composite lattice panels (+ Shiny app pointer)
- **S2** Featured-wall gallery: censored vs wall-terminated endings (NOR, USA, others)
- **S3** 45-panel squeeze trajectories + `squeeze_by_country_year.csv` pointer
- **S4** Full last-cohort table + ceiling sensitivity (1/1000 threshold)
- **S5** From surfaces to models: M1/M2/M3 geometries, tempo and mean-reversion
  mimicry guards (current §6 core, moved wholesale)
- **S6** COVID dip-and-rebound detail backing the Discussion paragraph
- **S7** Provenance of the 2020 extrapolations: published Fig 2 annotations,
  the 2018-09-29 working figure with drawn lines, verbatim forecast quotes —
  the honesty layer beneath the §4 composite
- **S8** The vanished US plateau (F-002 analysis + script) — one sentence in
  §4's US prose points here

## Standing style directives

- Hyphens, not em-dashes, throughout (F-019)
- "(TFRs)", "(OWiD)" abbreviations at first use (F-024 insertions)
- Terminology: formal term **ceiling age**, introduced with the informal
  gloss **"age of no return" (ANR)** — per Jon's (vii)

## Open calls for Jon (with recommendations)

1. **Left column of the 2×2 (F-021):** (a) use the 2018 working figure as-is
   (provenance-honest; palette mismatch) vs (b) re-render 2020-vintage data
   in the 2026 house style with the extrapolation lines redrawn, provenance
   in caption + S7. **Recommend (b)** — the columns then differ only by data
   window, so the reader sees the update, not a palette change.
2. **Distilled squeeze chart for §6:** pooled panel medians (peak age +
   ceiling lines, ribbon for spread, colour for peak ASFR) vs a
   selected-countries small multiple. **Recommend pooled-median single
   panel**; 45-country detail in S3.
3. **Title:** revisit after restructure — "The closing corridor" still fits
   the spine's destination.
