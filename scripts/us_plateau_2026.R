# The vanished US plateau (F-002; supplement S8).
#
# The USA was historically unusual for a flat-peaked (potentially bimodal,
# subgroup-mixture) ASFR age profile. Operationalisation: plateau width of a
# country-year = number of single-year ages whose ASFR is within 90% of that
# year's maximum. A "plateau year" has width >= 9.
#
# Outputs:
#   data/derived_2026/plateau_width_by_country_year.csv
#   figures/figures_2026/us_plateau_width.png
#   console: plateau-year runs by country; US profile summary

library(readr)
library(dplyr)
library(ggplot2)

source("scripts/functions_2026.R")
library(tidyr)

prep <- prepare_dta_2026()
dta <- prep$dta

widths <- dta %>%
  filter(!is.na(asfr), year >= 1950, age <= 50) %>%
  group_by(code, country, year) %>%
  summarise(width = sum(asfr >= 0.9 * max(asfr)), .groups = "drop")

dir.create("data/derived_2026", showWarnings = FALSE, recursive = TRUE)
write_csv(widths, "data/derived_2026/plateau_width_by_country_year.csv")

# plateau-year runs (width >= 9), by country
runs <- widths %>%
  filter(width >= 9) %>%
  group_by(code, country) %>%
  summarise(n_years = n(), first = min(year), last = max(year), .groups = "drop") %>%
  arrange(desc(n_years))
cat("Country-years with plateau width >= 9 (ASFR within 90% of max):\n")
print(as.data.frame(runs))

panel_med <- widths %>%
  group_by(year) %>%
  summarise(panel_median = median(width), .groups = "drop")

usa <- widths %>% filter(code == "USA")

p <- ggplot() +
  geom_line(data = panel_med, aes(x = year, y = panel_median),
            colour = "grey55", linewidth = 0.6) +
  geom_line(data = usa, aes(x = year, y = width),
            colour = "grey10", linewidth = 0.9) +
  annotate("text", x = 1999, y = max(usa$width) + 0.6, label = "United States",
           size = 3.4, colour = "grey10") +
  annotate("text", x = 1962, y = 3.2, label = "panel median (45 countries)",
           size = 3.1, colour = "grey45", hjust = 0) +
  geom_hline(yintercept = 9, linetype = "dashed", colour = "grey70",
             linewidth = 0.4) +
  annotate("text", x = 1951, y = 9.5, label = "plateau criterion (9 ages)",
           size = 2.9, colour = "grey55", hjust = 0) +
  labs(
    x = "Year", y = "Ages within 90% of the year's peak ASFR",
    title = "The vanished US plateau",
    subtitle = "Width of the near-peak age range, USA vs the 45-country median, 1950-2025."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

dir.create("figures/figures_2026", showWarnings = FALSE, recursive = TRUE)
ggsave("figures/figures_2026/us_plateau_width.png", p,
       width = 18, height = 12, units = "cm", dpi = 300)

cat("\nUSA plateau summary:\n")
usa %>%
  mutate(era = case_when(year <= 1990 ~ "1950-1990",
                         year <= 2008 ~ "1991-2008",
                         TRUE ~ "2009-")) %>%
  group_by(era) %>%
  summarise(median_width = median(width), max_width = max(width),
            .groups = "drop") %>%
  as.data.frame() %>%
  print()

cat("us_plateau_2026.R complete\n")
