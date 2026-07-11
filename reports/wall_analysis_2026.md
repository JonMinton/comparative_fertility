# Wall / compression analysis — 2026 panel

*Generated 2026-07-11 by `scripts/wall_2026.R`. Data: `data/data_combined_and_standardised_2026.csv` (45-country published panel).*

## Pooled decade medians (context only; per-country CSVs are the primary output)

| Decade | Ceiling (ASFR >= 1/200) | Cumulative ASFR 40+ |
|---|---|---|
| 1950s | 45 | 0.117 |
| 1960s | 44 | 0.079 |
| 1970s | 42 | 0.038 |
| 1980s | 41 | 0.023 |
| 1990s | 41 | 0.023 |
| 2000s | 42 | 0.032 |
| 2010s | 42 | 0.054 |
| 2020s | 43 | 0.065 |

## Replacement reachability (CPCFR >= 2.05, cohorts observed from age <= 16)

- Countries with any cohort born >= 1970 crossing 2.05: **9**
- Countries with at least one post-war cohort observed past age 44 that NEVER crossed: **43**

### Last cohort to reach replacement, by country (top of table = most recent)

| Country | Last cohort crossing 2.05 | Age at crossing | First 'never' cohort |
|---|---|---|---|
| United States of America | 1984 | 40 | 1950 |
| Iceland | 1983 | 40 | — |
| UK, Northern Ireland | 1981 | 42 | 1972 |
| France | 1980 | 43 | 1892 |
| New Zealand | 1980 | 42 | 1981 |
| Ireland | 1975 | 44 | 1976 |
| Macedonia | 1974 | 42 | 1975 |
| Albania | 1973 | 34 | — |
| Norway | 1971 | 43 | 1954 |
| Australia | 1967 | 47 | 1968 |
| Slovakia | 1965 | 42 | 1966 |
| Moldova | 1964 | 41 | 1965 |
| Poland | 1964 | 46 | 1965 |
| Taiwan | 1963 | 37 | 1964 |
| Romania | 1962 | 40 | 1963 |
| Estonia | 1960 | 39 | 1944 |
| Sweden | 1960 | 44 | 1898 |
| Czech Republic | 1958 | 41 | 1943 |
| Republic of Korea | 1958 | 42 | 1959 |
| Portugal | 1956 | 45 | 1953 |
| Spain | 1953 | 43 | 1954 |
| Bosnia and Herzegovina | 1951 | 38 | 1952 |
| Bulgaria | 1950 | 37 | 1933 |
| UK, England and Wales | 1950 | 42 | 1898 |
| UK, Scotland | 1950 | 40 | 1951 |
| Lithuania | 1949 | 40 | 1944 |
| Canada | 1948 | 41 | 1949 |
| Italy | 1947 | 44 | 1948 |
| Denmark | 1945 | 38 | 1946 |
| Netherlands | 1944 | 41 | 1891 |
| Belgium | 1943 | 41 | 1944 |
| Austria | 1942 | 46 | 1943 |
| Switzerland | 1941 | 41 | 1942 |
| Japan | 1941 | 38 | 1910 |
| Finland | 1940 | 41 | 1941 |
| Germany, East | 1938 | 40 | 1939 |
| Germany, West | 1938 | 39 | 1939 |
| Hungary | 1933 | 43 | 1934 |

## Files

- `data/derived_2026/wall_by_country_year.csv` — ceiling per country-year (both thresholds)
- `data/derived_2026/late_fertility_mass.csv` — TFR, 35+, 40+ mass per country-year
- `data/derived_2026/ccfr_crossing_ages.csv` — per-cohort crossing age and status
- `data/derived_2026/last_cohort_replacement.csv` — the table above
- `figures/figures_2026/wall_trajectories.png` — 45-panel ceiling trajectories, both thresholds
- `figures/figures_2026/late_mass_trends.png` — 45-panel late-fertility mass
- `figures/figures_2026/reachability_crossing_ages.png` — age-at-2.05 by cohort, 45 panels, wall reference at 43
- `figures/figures_2026/featured_wall_{nor,usa,kor}.png` — surfaces with ceiling overdrawn
