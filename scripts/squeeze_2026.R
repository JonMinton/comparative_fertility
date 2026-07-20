# The "squeeze": age of peak fertility against the effective ceiling.
#
# Distils the Lexis surfaces into two per-country trajectories (F-015 round,
# Jon's direction 2026-07-17):
#   - modal (peak) age of the ASFR schedule per country-year, colour-coded by
#     the ASFR value at the peak (darker = higher, matching the surfaces)
#   - the effective ceiling (highest age with ASFR >= 1/200) from wall_2026.R
# The vertical gap between the two is the room left between where childbearing
# is concentrated and where it effectively ends; its narrowing is the
# "squeeze". Companion stylized fact to the TFR convergence-down noted by
# Ritchie (OWID Data Insight, 2026-07-16): the AGE-structure of the levelling.
#
# F-034 (Jon, 2026-07-20): the 45-panel figure moves from Supplement S3 into
# the main paper, with panels ordered by period TFR in each country's latest
# observed year (highest first), echoing the original paper's ordered
# 45-country presentation. Labels updated to AAPF/ANR vocabulary (F-026).
#
# Outputs:
#   data/derived_2026/squeeze_by_country_year.csv
#   data/derived_2026/latest_tfr_by_country.csv (panel ordering, F-034)
#   figures/figures_2026/squeeze_trajectories.png (45 panels)
#   pooled decade medians printed to console (context only, as elsewhere)

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

source("scripts/functions_2026.R")

dir.create("data/derived_2026", showWarnings = FALSE, recursive = TRUE)
dir.create("figures/figures_2026", showWarnings = FALSE, recursive = TRUE)

prep <- prepare_dta_2026()
dta <- prep$dta

# --- peak age of the fertility schedule per country-year --------------------
# Ties broken to the youngest tied age; flat/bimodal schedules are therefore
# read conservatively (peak placed early). See F-002 (US plateau) for why
# plateau shapes deserve their own treatment.

peaks <- dta %>%
  filter(!is.na(asfr)) %>%
  group_by(code, country, year) %>%
  summarise(
    peak_age  = min(age[asfr == max(asfr)]),
    peak_asfr = max(asfr),
    .groups = "drop"
  )

walls <- read_csv("data/derived_2026/wall_by_country_year.csv",
                  show_col_types = FALSE)

squeeze <- peaks %>%
  left_join(walls %>% select(code, year, wall_200, wall_1000),
            by = c("code", "year")) %>%
  mutate(gap_200 = wall_200 - peak_age)

write_csv(squeeze, "data/derived_2026/squeeze_by_country_year.csv")

# --- pooled decade medians (context only) ------------------------------------

decades <- squeeze %>%
  filter(year >= 1950) %>%
  mutate(decade = paste0(floor(year / 10) * 10, "s")) %>%
  group_by(decade) %>%
  summarise(
    peak_age_med  = median(peak_age, na.rm = TRUE),
    peak_asfr_med = round(median(peak_asfr, na.rm = TRUE), 3),
    wall_med      = median(wall_200, na.rm = TRUE),
    gap_med       = median(gap_200, na.rm = TRUE),
    .groups = "drop"
  )
print(as.data.frame(decades))

# --- panel ordering: latest TFR, vintage-adjusted (F-034, F-035) -------------
# TFR = sum of single-year ASFRs; years with fewer than 30 observed ages are
# skipped so a partial final year cannot understate a country's TFR.
#
# Mixed-vintage problem (F-035): nine series end in 2021 or earlier (Albania
# 2008 .. Bulgaria 2021), and TFRs have fallen almost everywhere since, so
# ordering raw last-observed TFRs ranks stale series too high. The last year
# common to all 45 countries is 2008 (pre-acceleration), so a common-year
# ordering is no fix. Instead, series ending before 2022 are vintage-adjusted
# from the panel itself: last observed TFR times the panel-median ratio of
# current TFR to same-year TFR among fully observed countries. Self-contained
# (no external source), and the correction is the panel-wide decline the paper
# itself documents. Ukraine's adjusted value is still likely optimistic (war).

CURRENT_FROM <- 2022

tfr_cy <- dta %>%
  filter(!is.na(asfr)) %>%
  group_by(code, country, year) %>%
  summarise(tfr = sum(asfr), n_ages = n(), .groups = "drop") %>%
  filter(n_ages >= 30)

last_obs <- tfr_cy %>%
  group_by(code, country) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(code, country, latest_year = year, latest_tfr = tfr)

current_codes <- last_obs %>% filter(latest_year >= CURRENT_FROM) %>% pull(code)

adjust_factor <- function(t_stale) {
  ratios <- tfr_cy %>%
    filter(code %in% current_codes, year == t_stale) %>%
    inner_join(last_obs %>% select(code, latest_tfr), by = "code") %>%
    mutate(ratio = latest_tfr / tfr)
  median(ratios$ratio)
}

latest_tfr <- last_obs %>%
  mutate(
    vintage_adjusted = latest_year < CURRENT_FROM,
    adj_factor = ifelse(vintage_adjusted,
                        vapply(latest_year, adjust_factor, numeric(1)), 1),
    tfr_for_ordering = latest_tfr * adj_factor
  ) %>%
  arrange(desc(tfr_for_ordering)) %>%
  mutate(rank = row_number())

write_csv(latest_tfr, "data/derived_2026/latest_tfr_by_country.csv")

# --- 45-panel squeeze figure --------------------------------------------------

panel_labels <- latest_tfr %>%
  mutate(label = ifelse(
    vintage_adjusted,
    sprintf("%s (%d: %.2f → %.2f)", country, latest_year,
            latest_tfr, tfr_for_ordering),
    sprintf("%s (%.2f)", country, latest_tfr)
  ))

p <- squeeze %>%
  filter(year >= 1950) %>%
  left_join(panel_labels %>% select(code, label), by = "code") %>%
  mutate(label = factor(label, levels = panel_labels$label)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = wall_200), colour = "grey25", linewidth = 0.5) +
  geom_point(aes(y = peak_age, colour = peak_asfr), size = 0.45) +
  scale_colour_viridis(direction = -1, name = "ASFR at AAPF age") +
  facet_wrap(~label, ncol = 7) +
  coord_cartesian(ylim = c(15, 52)) +
  labs(
    x = "Year", y = "Age in years",
    title = "The squeeze by country: the AAPF (points, shaded by the rate at the peak) rising toward the ANR (line)",
    subtitle = "Panels ordered by latest period TFR, highest first; series ending before 2022 are vintage-adjusted by the panel-median decline since their\nlast observed year (year: observed → adjusted). ANR: highest age with ASFR ≥ 1/200. AAPF age: modal age of the schedule, ties to youngest."
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 7)
  )

ggsave("figures/figures_2026/squeeze_trajectories.png", p,
       width = 40, height = 42, units = "cm", dpi = 300)

cat("squeeze_2026.R complete\n")
