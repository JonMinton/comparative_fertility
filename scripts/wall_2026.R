# Phase 3: the fertility "wall" / compression analysis.
#
# Quantifies and visualizes the effective upper boundary of the fertility
# lifecourse and its interaction with the CPCFR replacement contour:
#   1. per-country effective-ceiling trajectories (highest age with
#      ASFR >= 1/200, sensitivity 1/1000)
#   2. late-fertility mass: cumulative ASFR above ages 40 and 35
#   3. reachability: per cohort, the age at which CPCFR crosses 2.05, and a
#      three-way status — crossed / never (observed past the wall) / censored
#   4. featured single-country surfaces (NOR, USA, KOR) with the ceiling
#      overdrawn, distinguishing wall-terminated from censored contours
# Outputs: data/derived_2026/*.csv, figures/figures_2026/*, reports/wall_analysis_2026.md
#
# Analytic choices (fixed in UPDATE_WORKPLAN.md before this ran on new data;
# prototypes on the OLD data during planning are disclosed there):
#   - primary ceiling threshold ASFR >= 0.005 (1-in-200), sensitivity 0.001
#   - per-country trajectories are the headline; pooled medians only for context
#   - "never" requires observation to at least age 44 without crossing

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lattice)
library(RColorBrewer)
library(viridis)

source("scripts/functions_2026.R")

dir.create("data/derived_2026", showWarnings = FALSE, recursive = TRUE)
dir.create("figures/figures_2026", showWarnings = FALSE, recursive = TRUE)
dir.create("reports", showWarnings = FALSE)

prep <- prepare_dta_2026()
dta <- prep$dta
country_codes <- prep$country_codes

# --- 1. Effective ceiling per country-year ----------------------------------

walls <- dta %>%
  group_by(code, country, year) %>%
  summarise(
    wall_200  = ifelse(any(asfr >= 0.005), max(age[asfr >= 0.005]), NA_integer_),
    wall_1000 = ifelse(any(asfr >= 0.001), max(age[asfr >= 0.001]), NA_integer_),
    .groups = "drop"
  )

write_csv(walls, "data/derived_2026/wall_by_country_year.csv")

png("figures/figures_2026/wall_trajectories.png",
    res = 300, width = 40, height = 40, units = "cm")
print(
  xyplot(wall_200 + wall_1000 ~ year | country,
         data = walls %>% filter(year >= 1950),
         type = "l", lwd = c(2, 1), lty = c(1, 2), col = c("firebrick", "grey40"),
         as.table = TRUE,
         ylab = "Highest age with ASFR above threshold",
         xlab = "Year",
         ylim = c(35, 52),
         scales = list(x = list(rot = 90, cex = 0.7), y = list(cex = 0.7)),
         par.strip.text = list(cex = 0.7),
         key = list(
           space = "top", columns = 2,
           lines = list(lwd = c(2, 1), lty = c(1, 2), col = c("firebrick", "grey40")),
           text = list(c("ASFR >= 1/200 (primary)", "ASFR >= 1/1000 (sensitivity)"))
         ))
)
dev.off()

# --- 2. Late-fertility mass --------------------------------------------------

late_mass <- dta %>%
  group_by(code, country, year) %>%
  summarise(
    tfr     = sum(asfr),
    mass_35 = sum(asfr[age >= 35]),
    mass_40 = sum(asfr[age >= 40]),
    .groups = "drop"
  )

write_csv(late_mass, "data/derived_2026/late_fertility_mass.csv")

png("figures/figures_2026/late_mass_trends.png",
    res = 300, width = 40, height = 40, units = "cm")
print(
  xyplot(mass_40 + mass_35 ~ year | country,
         data = late_mass %>% filter(year >= 1950),
         type = "l", lwd = c(2, 1), lty = c(1, 2), col = c("firebrick", "grey40"),
         as.table = TRUE,
         ylab = "Cumulative ASFR at and above age threshold (children)",
         xlab = "Year",
         scales = list(x = list(rot = 90, cex = 0.7), y = list(cex = 0.7)),
         par.strip.text = list(cex = 0.7),
         key = list(
           space = "top", columns = 2,
           lines = list(lwd = c(2, 1), lty = c(1, 2), col = c("firebrick", "grey40")),
           text = list(c("Ages 40+", "Ages 35+"))
         ))
)
dev.off()

# --- 3. Reachability: cohort crossing of the 2.05 contour --------------------

crossings <- dta %>%
  filter(series_ok %in% TRUE, !is.na(my_ccfr)) %>%
  group_by(code, country, birth_year) %>%
  summarise(
    crossed     = any(my_ccfr >= 2.05),
    cross_age   = ifelse(any(my_ccfr >= 2.05), min(age[my_ccfr >= 2.05]), NA_integer_),
    max_obs_age = max(age),
    final_ccfr  = max(my_ccfr),
    .groups = "drop"
  ) %>%
  mutate(status = case_when(
    crossed           ~ "crossed",
    max_obs_age >= 44 ~ "never",
    TRUE              ~ "censored"
  ))

write_csv(crossings, "data/derived_2026/ccfr_crossing_ages.csv")

last_crossing <- crossings %>%
  filter(status == "crossed") %>%
  group_by(code, country) %>%
  summarise(
    last_cohort_crossing = max(birth_year),
    age_at_last_crossing = cross_age[birth_year == max(birth_year)][1],
    .groups = "drop"
  ) %>%
  left_join(
    crossings %>%
      filter(status == "never") %>%
      group_by(code) %>%
      summarise(first_never_cohort = min(birth_year), .groups = "drop"),
    by = "code"
  ) %>%
  arrange(desc(last_cohort_crossing))

write_csv(last_crossing, "data/derived_2026/last_cohort_replacement.csv")

png("figures/figures_2026/reachability_crossing_ages.png",
    res = 300, width = 40, height = 40, units = "cm")
print(
  xyplot(cross_age ~ birth_year | country,
         data = crossings %>% filter(status == "crossed"),
         type = "b", pch = 16, cex = 0.3, col = "black",
         as.table = TRUE,
         ylab = "Age at which cohort CPCFR reaches 2.05",
         xlab = "Cohort (birth year)",
         panel = function(x, y, ...) {
           panel.abline(h = 43, lty = 3, col = "firebrick", lwd = 1.5)
           panel.xyplot(x, y, ...)
         },
         scales = list(x = list(rot = 90, cex = 0.7), y = list(cex = 0.7)),
         par.strip.text = list(cex = 0.7))
)
dev.off()

# --- 4. Featured surfaces with the ceiling overdrawn -------------------------

dta_wall <- dta %>%
  left_join(walls %>% select(code, year, wall_200), by = c("code", "year")) %>%
  mutate(is_wall = age == wall_200)

for (cc in c("NOR", "USA", "KOR")) {
  png(sprintf("figures/figures_2026/featured_wall_%s.png", tolower(cc)),
      res = 300, width = 16, height = 14, units = "cm")
  print(produce_composite_lattice(
    dta_wall %>% filter(code == cc) %>% mutate(country = droplevels(country)),
    country_codes, add_gridlines = FALSE
  ))
  dev.off()
}

# --- 5. Summary report --------------------------------------------------------

dec_med <- function(df, var) {
  df %>%
    filter(year >= 1950) %>%
    mutate(decade = (year %/% 10) * 10) %>%
    group_by(decade) %>%
    summarise(v = median({{ var }}, na.rm = TRUE), n = n(), .groups = "drop")
}

w5 <- dec_med(walls, wall_200)
m40 <- dec_med(late_mass, mass_40)

n_crossing_recent <- crossings %>%
  filter(status == "crossed", birth_year >= 1970) %>%
  distinct(code) %>% nrow()
n_never <- crossings %>%
  filter(status == "never") %>%
  distinct(code) %>% nrow()

lines_out <- c(
  "# Wall / compression analysis — 2026 panel",
  "",
  sprintf("*Generated %s by `scripts/wall_2026.R`. Data: `data/data_combined_and_standardised_2026.csv` (45-country published panel).*", "2026-07-11"),
  "",
  "## Pooled decade medians (context only; per-country CSVs are the primary output)",
  "",
  "| Decade | Ceiling (ASFR >= 1/200) | Cumulative ASFR 40+ |",
  "|---|---|---|",
  sprintf("| %ds | %.0f | %.3f |", w5$decade, w5$v, m40$v[match(w5$decade, m40$decade)]),
  "",
  "## Replacement reachability (CPCFR >= 2.05, cohorts observed from age <= 16)",
  "",
  sprintf("- Countries with any cohort born >= 1970 crossing 2.05: **%d**", n_crossing_recent),
  sprintf("- Countries with at least one post-war cohort observed past age 44 that NEVER crossed: **%d**", n_never),
  "",
  "### Last cohort to reach replacement, by country (top of table = most recent)",
  "",
  "| Country | Last cohort crossing 2.05 | Age at crossing | First 'never' cohort |",
  "|---|---|---|---|",
  sprintf("| %s | %d | %d | %s |",
          last_crossing$country, last_crossing$last_cohort_crossing,
          last_crossing$age_at_last_crossing,
          ifelse(is.na(last_crossing$first_never_cohort), "—",
                 as.character(last_crossing$first_never_cohort))),
  "",
  "## Files",
  "",
  "- `data/derived_2026/wall_by_country_year.csv` — ceiling per country-year (both thresholds)",
  "- `data/derived_2026/late_fertility_mass.csv` — TFR, 35+, 40+ mass per country-year",
  "- `data/derived_2026/ccfr_crossing_ages.csv` — per-cohort crossing age and status",
  "- `data/derived_2026/last_cohort_replacement.csv` — the table above",
  "- `figures/figures_2026/wall_trajectories.png` — 45-panel ceiling trajectories, both thresholds",
  "- `figures/figures_2026/late_mass_trends.png` — 45-panel late-fertility mass",
  "- `figures/figures_2026/reachability_crossing_ages.png` — age-at-2.05 by cohort, 45 panels, wall reference at 43",
  "- `figures/figures_2026/featured_wall_{nor,usa,kor}.png` — surfaces with ceiling overdrawn"
)

writeLines(lines_out, "reports/wall_analysis_2026.md")
cat("Phase 3 outputs written.\n")
