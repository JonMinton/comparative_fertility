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
# Outputs:
#   data/derived_2026/squeeze_by_country_year.csv
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

# --- 45-panel squeeze figure --------------------------------------------------

p <- squeeze %>%
  filter(year >= 1950) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = wall_200), colour = "grey25", linewidth = 0.5) +
  geom_point(aes(y = peak_age, colour = peak_asfr), size = 0.45) +
  scale_colour_viridis(direction = -1, name = "ASFR at peak age") +
  facet_wrap(~country, ncol = 7) +
  coord_cartesian(ylim = c(15, 52)) +
  labs(
    x = "Year", y = "Age in years",
    title = "The squeeze: age of peak fertility (points, shaded by ASFR at the peak) rising toward the effective ceiling (line)",
    subtitle = "Ceiling: highest age with ASFR ≥ 1/200. Peak: modal age of the fertility schedule, ties to youngest."
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
