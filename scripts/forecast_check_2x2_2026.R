# Forecast-check 2x2 composite (F-021 design; F-028 corrections):
#   rows    = Norway, USA
#   columns = 2020-vintage data (with the drawn extrapolations) | 2026 update
# Left column re-renders the data frozen at repository tag `demres-2020`
# (data/data_combined_and_standardised.csv at that tag) in the 2026 house
# style, so the only difference between columns is the data window:
#   https://github.com/JonMinton/comparative_fertility/blob/demres-2020/data/data_combined_and_standardised.csv
#
# F-028 corrections (Jon, 2026-07-19):
#   - CPCFR milestones drawn as TRUE FIELD CONTOURS of cumulative fertility
#     over (birth year, age), not per-cohort crossing-age polylines, so that
#     where cohorts stop reaching a milestone within fully observed data the
#     contour escapes vertically ("goes to infinity"), as in the published
#     figures; censored contours end at the data edge instead.
#   - The magenta extrapolation lines are digitized programmatically from
#     the September 2018 working figure (scripts/extract_2018_extrapolation.py
#     -> data/derived_2026/extrapolation_2018_digitized.csv), replacing
#     hand-eyeballed coordinates. The actual Norway line runs nearly flat at
#     age ~43 for cohorts ~1972-81 and then turns vertical.
#   - Extrapolation lines appear ONLY on the 2020-vintage panels.
#
# Usage: Rscript scripts/forecast_check_2x2_2026.R path/to/data_2020vintage.csv
#        (extract first:  git show demres-2020:data/data_combined_and_standardised.csv > path)
# Output: figures/figures_2026/forecast_check_2x2.png

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

args <- commandArgs(trailingOnly = TRUE)
vintage_path <- if (length(args) >= 1) args[1] else "data/data_2020vintage_from_tag.csv"

prep_two <- function(path, vintage_label) {
  read_csv(path, col_types = cols()) %>%
    select(code, year, age, asfr) %>%
    filter(code %in% c("NOR", "USA")) %>%
    # zero cells are stored as absent rows in some releases; complete the
    # year-age grid within each code's observed span so surfaces have no holes
    group_by(code) %>%
    complete(year = full_seq(year, 1), age = 12:50, fill = list(asfr = 0)) %>%
    ungroup() %>%
    arrange(code, year, age) %>%
    mutate(birth_year = year - age) %>%
    arrange(code, birth_year, age) %>%
    group_by(code, birth_year) %>%
    mutate(my_ccfr = lag(cumsum(asfr), 1)) %>%
    mutate(series_ok = min(age) <= 16) %>%
    ungroup() %>%
    mutate(vintage = vintage_label)
}

old <- prep_two(vintage_path, "2020 vintage (data to 2014-15)")
new <- prep_two("data/data_combined_and_standardised_2026.csv",
                "2026 update (data to 2023-25)")

dta <- bind_rows(old, new) %>%
  filter(year >= 1950, age >= 12, age <= 50) %>%
  mutate(
    country = recode(code, NOR = "Norway", USA = "United States"),
    vintage = factor(vintage, levels = c("2020 vintage (data to 2014-15)",
                                         "2026 update (data to 2023-25)"))
  )

# Complete rectangular grid per panel for field contouring: NA outside the
# observed series_ok region (contours end at data edges = censoring; escape
# through the top within fully observed data = "to infinity").
contour_grid <- dta %>%
  mutate(ccfr_ok = if_else(series_ok, my_ccfr, NA_real_)) %>%
  select(vintage, country, birth_year, age, ccfr_ok) %>%
  group_by(vintage, country) %>%
  complete(birth_year = full_seq(birth_year, 1), age = 12:50) %>%
  ungroup()

# Digitized 2020-era extrapolations (2018 working figure), left panels only
extrap <- read_csv("data/derived_2026/extrapolation_2018_digitized.csv",
                   col_types = cols()) %>%
  mutate(vintage = factor(levels(dta$vintage)[1], levels = levels(dta$vintage)))

# Panel annotations
ann <- tribble(
  ~country,        ~vintage_i, ~birth_year, ~age, ~label,                          ~hjust,
  "Norway",         1,          1951,        49,  "replacement lost\n1953 cohort..", 1,
  "Norway",         1,          1959,        49,  "..re-established\n1956 cohort",   0,
  "Norway",         1,          1969,        40,  "replacement age 43",              1,
  "Norway",         1,          2008,        47,  "speculative extrapolation\n(drawn 2018; published verbally)", 1,
  "Norway",         2,          2008,        48,  "realized: contour escapes -\nno cohort after 1971\nreaches replacement",  1,
  "United States",  1,          1948,        49,  "replacement lost\n1950 cohort..", 1,
  "United States",  1,          1966,        49,  "..re-established\n1963 cohort",   0,
  "United States",  1,          1975,        34,  "replacement age 37",              1,
  "United States",  1,          2008,        42,  "speculative extrapolation:\nsustained near age 37",  1,
  "United States",  2,          2008,        45,  "realized: crossings continue\nto the 1984 cohort - but\nat ages 39-40, not 37",  1
) %>%
  mutate(vintage = factor(levels(dta$vintage)[vintage_i], levels = levels(dta$vintage)))

p <- ggplot(dta, aes(x = birth_year, y = age)) +
  geom_tile(aes(fill = asfr)) +
  scale_fill_viridis(direction = -1, name = "ASFR",
                     limits = c(0, max(dta$asfr, na.rm = TRUE)),
                     breaks = c(0, 0.1, 0.2)) +
  geom_contour(data = contour_grid, aes(z = ccfr_ok),
               breaks = 1.50, linewidth = 0.35, colour = "black",
               na.rm = TRUE) +
  geom_contour(data = contour_grid, aes(z = ccfr_ok),
               breaks = 2.05, linewidth = 0.9, colour = "black",
               na.rm = TRUE) +
  geom_line(data = extrap, aes(group = country), colour = "magenta",
            linewidth = 0.8, linetype = "dotted") +
  geom_text(data = ann, aes(label = label, hjust = hjust),
            size = 2.6, lineheight = 0.95, vjust = 1) +
  facet_grid(country ~ vintage) +
  coord_cartesian(xlim = c(1900, 2012), ylim = c(12, 50), expand = FALSE) +
  labs(
    x = "Birth year", y = "Age in years",
    title = "The 2020 extrapolations and what the new data showed",
    subtitle = paste0(
      "Shading: ASFR (darker = higher). Heavy contour: cumulative cohort fertility (CPCFR) = 2.05 (replacement); thin: 1.50. ",
      "Contours escaping\nthrough the top mark cohorts that never reach the milestone; contours cut at the data edge are censored. ",
      "Magenta dotted: the 2020-era\nspeculative extrapolations (September 2018 working figure, digitized), on the 2020-vintage panels only."
    )
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.subtitle = element_text(size = 7.5)
  )

dir.create("figures/figures_2026", showWarnings = FALSE, recursive = TRUE)
ggsave("figures/figures_2026/forecast_check_2x2.png", p,
       width = 27, height = 21, units = "cm", dpi = 300)

# Console check: last crossing cohorts (for the caption/scoreboard, computed
# from the same field the contours are drawn on)
dta %>%
  filter(series_ok, !is.na(my_ccfr)) %>%
  group_by(vintage, country, birth_year) %>%
  summarise(crossed = any(my_ccfr >= 2.05),
            age_at = if (any(my_ccfr >= 2.05)) min(age[my_ccfr >= 2.05]) else NA_real_,
            .groups = "drop") %>%
  filter(crossed) %>%
  group_by(vintage, country) %>%
  summarise(last_cohort = max(birth_year),
            age_at_last = age_at[birth_year == max(birth_year)],
            .groups = "drop") %>%
  as.data.frame() %>%
  print()

cat("forecast_check_2x2_2026.R complete\n")
