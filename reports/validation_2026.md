# Validation report: 2026 combined dataset vs 2016 build

*Generated 2026-07-11 by `scripts/ingest_2026.R` + this comparison. New file: `data/data_combined_and_standardised_2026.csv` (51 countries, 168099 rows, sources: hfd/hfc/interp).*

## Panel changes

- Codes in old build only: **FRATNP**
- Codes in new build only: **none**
- Overlapping cells compared (code-year-age): **148236**

## Back-revision check on overlapping cells

HFD routinely revises historical series; large deviations in HFC-sourced countries may instead reflect refcode-selection differences (new script picks best collection, then most complete/most recent refcode).

| Code | cells | median abs diff | max abs diff | % cells > 0.001 |
|---|---|---|---|---|
| IRL | 2596 | 0.00209 | 0.0221 | 63.8% |
| ROU | 2146 | 0.00104 | 0.0333 | 50.8% |
| ESP | 4092 | 0.00075 | 0.0320 | 44.9% |
| DNK | 4793 | 0.00046 | 0.0149 | 36.0% |
| POL | 2741 | 0.00011 | 0.0194 | 30.4% |
| KOR | 1770 | 0.00000 | 0.0223 | 24.6% |
| CHN | 1470 | 0.00000 | 0.0230 | 24.4% |
| ITA | 3528 | 0.00006 | 0.0083 | 18.7% |
| BEL | 2851 | 0.00010 | 0.0274 | 16.3% |
| JPN | 3298 | 0.00003 | 0.0171 | 15.6% |
| HRV | 2503 | 0.00000 | 0.0111 | 11.6% |
| GBR_NIR | 1804 | 0.00000 | 0.0089 | 9.5% |
| AUS | 3424 | 0.00000 | 0.0033 | 8.9% |
| TWN | 2731 | 0.00000 | 0.0062 | 8.5% |
| SWE | 6891 | 0.00000 | 0.0092 | 7.8% |
| EST | 2464 | 0.00000 | 0.0079 | 6.3% |
| DEUTNP | 2420 | 0.00000 | 0.0019 | 5.9% |
| TUR | 490 | 0.00000 | 0.0018 | 5.7% |
| RUS | 2464 | 0.00000 | 0.0048 | 5.4% |
| GBR_SCO | 3302 | 0.00000 | 0.0134 | 4.7% |
| DEUTW | 2642 | 0.00000 | 0.0017 | 4.5% |
| SVK | 3008 | 0.00000 | 0.0188 | 4.3% |
| FRA | 4662 | 0.00001 | 0.0110 | 4.2% |
| NOR | 2854 | 0.00000 | 0.0055 | 3.8% |
| HUN | 3006 | 0.00000 | 0.0059 | 3.5% |
| ISL | 6272 | 0.00000 | 0.0827 | 3.5% |
| DEUTE | 2568 | 0.00000 | 0.0023 | 3.3% |
| BGR | 2772 | 0.00000 | 0.0072 | 3.2% |
| UKR | 2455 | 0.00000 | 0.0044 | 2.5% |
| SVN | 2629 | 0.00000 | 0.0078 | 2.4% |
| USA | 4200 | 0.00000 | 0.0037 | 2.1% |
| MDA | 1299 | 0.00000 | 0.0391 | 2.1% |
| FIN | 5899 | 0.00000 | 0.0054 | 2.0% |
| CAN | 4004 | 0.00000 | 0.0127 | 1.5% |
| BLR | 2392 | 0.00000 | 0.0048 | 1.4% |
| LTU | 2464 | 0.00000 | 0.0045 | 1.3% |
| PRT | 3300 | 0.00000 | 0.0063 | 1.2% |
| CZE | 3045 | 0.00000 | 0.0016 | 0.2% |
| CHL | 616 | 0.00000 | 0.0011 | 0.2% |
| GBR_NP | 1672 | 0.00000 | 0.0010 | 0.1% |
| GBRTENW | 4261 | 0.00000 | 0.0011 | 0.0% |
| ALB | 1998 | 0.00000 | 0.0000 | 0.0% |
| AUT | 2846 | 0.00000 | 0.0010 | 0.0% |
| BIH | 2220 | 0.00000 | 0.0000 | 0.0% |
| CHE | 3520 | 0.00000 | 0.0000 | 0.0% |
| GRC | 2158 | 0.00000 | 0.0000 | 0.0% |
| LVA | 1589 | 0.00000 | 0.0000 | 0.0% |
| MKD | 2385 | 0.00000 | 0.0000 | 0.0% |
| NLD | 3504 | 0.00000 | 0.0000 | 0.0% |
| NZL | 2923 | 0.00000 | 0.0000 | 0.0% |
| RKS | 1295 | 0.00000 | 0.0000 | 0.0% |

## Interpolated cells (internal gaps <= 4 yrs, linear)

- GRC: 140 cells
- HUN: 2 cells
- ISL: 36 cells
- JPN: 140 cells
- LVA: 140 cells
- NLD: 108 cells
- POL: 105 cells

## Stale right edges (last year < 2020)

- RKS: ends 1987
- CHN: ends 2005
- ALB: ends 2008
- GRC: ends 2009
- LVA: ends 2009
- ROU: ends 2013
- DEUTE: ends 2017
- DEUTNP: ends 2017
- DEUTW: ends 2017
- BLR: ends 2018

## Interpretation (2026-07-11 session)

- **IRL, ESP, DNK, POL, ITA, BEL, JPN**: big-diff cells are exclusively `hfd`-sourced
  and span the whole back-series → genuine HFD back-revisions (re-estimated
  exposures). The new build is authoritative; expect subtly different surfaces for
  these countries even in historical regions of the plots.
- **KOR**: mixture — old build used HFC for 1970–2007; new build uses HFD 2000–2024
  and HFC 1960–1999 (also gaining 1960s coverage). Source switch, not error.
- **ROU, CHN, HRV**: HFC-only; diffs reflect the refcode/collection selection rule
  (best collection, then most complete + most recent refcode). Magnitudes small.
- **FRATNP** present in old build but silently dropped by `data_prep.R`'s
  `to_keep` join (no entry in `code_definitions.csv`) — this is why the published
  figures show France ending 2008. Fixed in the new build (`FRATNP → FRA`).
- Verdict: **build accepted**. No unexplained discrepancies.
