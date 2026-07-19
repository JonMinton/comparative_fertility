# Pooled squeeze chart for the main manuscript (F-025 restructure; the
# 45-panel version from squeeze_2026.R moves to the supplement, S3).
#
# One panel: cross-country medians per year of
#   - the ANR (age of no return; operationalised as the highest age with
#     ASFR >= 1/200; wall_200 in squeeze_by_country_year.csv), and
#   - the AAPF age (modal age of the fertility schedule), with the AAPF
#     amount (ASFR at the peak) carried by point shading,
# with interquartile ribbons for spread. Runway reading: AAPF = where speed
# is highest; ANR = where the runway ends.
#
# Input:  data/derived_2026/squeeze_by_country_year.csv  (from squeeze_2026.R)
# Output: figures/figures_2026/squeeze_pooled.png

library(readr)
library(dplyr)
library(ggplot2)
library(viridis)

squeeze <- read_csv("data/derived_2026/squeeze_by_country_year.csv",
                    show_col_types = FALSE)

pooled <- squeeze %>%
  filter(year >= 1950, !is.na(wall_200)) %>%
  group_by(year) %>%
  summarise(
    n            = n(),
    peak_age_med = median(peak_age),
    peak_age_lo  = quantile(peak_age, 0.25),
    peak_age_hi  = quantile(peak_age, 0.75),
    peak_asfr_med = median(peak_asfr),
    anr_med      = median(wall_200),
    anr_lo       = quantile(wall_200, 0.25),
    anr_hi       = quantile(wall_200, 0.75),
    .groups = "drop"
  )

gap_first <- with(pooled[pooled$year == 1955, ], anr_med - peak_age_med)
gap_last  <- with(pooled[pooled$year == 2023, ], anr_med - peak_age_med)

p <- ggplot(pooled, aes(x = year)) +
  geom_ribbon(aes(ymin = anr_lo, ymax = anr_hi), fill = "grey40", alpha = 0.18) +
  geom_ribbon(aes(ymin = peak_age_lo, ymax = peak_age_hi),
              fill = "grey40", alpha = 0.18) +
  geom_line(aes(y = anr_med), colour = "grey15", linewidth = 0.8) +
  geom_point(aes(y = peak_age_med, colour = peak_asfr_med), size = 1.6) +
  scale_colour_viridis(direction = -1, name = "ASFR at the peak age\n(median)",
                       breaks = c(0.10, 0.15, 0.20)) +
  annotate("segment", x = 1955, xend = 1955,
           y = pooled$peak_age_med[pooled$year == 1955] + 0.6,
           yend = pooled$anr_med[pooled$year == 1955] - 0.6,
           arrow = arrow(ends = "both", length = unit(0.18, "cm")),
           colour = "grey30", linewidth = 0.4) +
  annotate("text", x = 1957, y = 34.5, hjust = 0, size = 3.1, colour = "grey15",
           label = sprintf("usable runway:\n%.0f years", gap_first)) +
  annotate("segment", x = 2023, xend = 2023,
           y = pooled$peak_age_med[pooled$year == 2023] + 0.6,
           yend = pooled$anr_med[pooled$year == 2023] - 0.6,
           arrow = arrow(ends = "both", length = unit(0.18, "cm")),
           colour = "grey30", linewidth = 0.4) +
  annotate("text", x = 2021, y = 37.5, hjust = 1, size = 3.1, colour = "grey15",
           label = sprintf("%.0f years", gap_last)) +
  annotate("text", x = 1951, y = 46.3, hjust = 0, size = 3.2, colour = "grey15",
           label = "ANR: age of no return (highest age with ASFR ≥ 1/200)") +
  annotate("text", x = 1951, y = 22.3, hjust = 0, size = 3.2, colour = "grey15",
           label = "AAPF: age of peak fertility (shaded by the rate at the peak)") +
  labs(
    x = "Year", y = "Age in years",
    title = "The squeeze: cross-country medians, 45 countries, 1950-2025",
    subtitle = "Medians across countries; ribbons: interquartile ranges."
  ) +
  coord_cartesian(ylim = c(20, 48)) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

dir.create("figures/figures_2026", showWarnings = FALSE, recursive = TRUE)
ggsave("figures/figures_2026/squeeze_pooled.png", p,
       width = 20, height = 14, units = "cm", dpi = 300)

print(as.data.frame(pooled %>% filter(year %in% c(1955, 1975, 1995, 2015, 2023))))
cat(sprintf("gap 1955: %.1f | gap 2023: %.1f\n", gap_first, gap_last))
cat("squeeze_pooled_2026.R complete\n")
