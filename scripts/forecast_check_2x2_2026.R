# Forecast-check 2x2 composite (F-021, Jon's design 2026-07-19):
#   rows    = Norway, USA
#   columns = 2020-vintage data (with the drawn extrapolations) | 2026 update
# Left column re-renders the data frozen at repository tag `demres-2020`
# (data/data_combined_and_standardised.csv at that tag) in the 2026 house
# style, so the only difference between columns is the data window:
#   https://github.com/JonMinton/comparative_fertility/blob/demres-2020/data/data_combined_and_standardised.csv
# The magenta dotted extrapolation lines are redrawn from the September 2018
# working figure (figures/usanor_annotated_two_contour.png); the published
# paper carried their verbal equivalents (Pattaro et al. 2020, pp. 699-700).
# Digitized coordinates are approximate to the eye; the original figure is
# reproduced in the supplement (S7).
#
# CPCFR milestone "contours" are drawn as per-cohort crossing-age lines
# (first age at which cumulative pseudo-cohort fertility reaches the
# milestone) - equivalent to levelplot contours because CPCFR is monotone in
# age, and cleaner to annotate.
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

# Per-cohort milestone crossing ages (only series observed from age <= 16)
crossings <- dta %>%
  filter(series_ok, !is.na(my_ccfr)) %>%
  group_by(vintage, country, code, birth_year) %>%
  summarise(
    age_205 = if (any(my_ccfr >= 2.05)) min(age[my_ccfr >= 2.05]) else NA_real_,
    age_150 = if (any(my_ccfr >= 1.50)) min(age[my_ccfr >= 1.50]) else NA_real_,
    .groups = "drop"
  )

# Redrawn "speculative extrapolation" lines (2018 working figure, digitized)
extrap <- bind_rows(
  tibble(country = "Norway",
         birth_year = c(1972, 1976, 1979, 1982, 1984),
         age        = c(43, 44, 46, 48.5, 50)),
  tibble(country = "United States",
         birth_year = c(1977, 1985, 1995, 2004),
         age        = c(37, 37.3, 37.8, 38.2))
)
extrap_both <- bind_rows(
  extrap %>% mutate(vintage = levels(dta$vintage)[1]),
  extrap %>% mutate(vintage = levels(dta$vintage)[2])
) %>% mutate(vintage = factor(vintage, levels = levels(dta$vintage)))

# Panel annotations
ann <- tribble(
  ~country,        ~vintage_i, ~birth_year, ~age, ~label,                          ~hjust,
  "Norway",         1,          1951,        49,  "replacement lost\n1953 cohort..", 1,
  "Norway",         1,          1959,        49,  "..re-established\n1956 cohort",   0,
  "Norway",         1,          1969,        40,  "replacement age 43",              1,
  "Norway",         1,          2008,        47,  "speculative extrapolation\n(drawn 2018; published verbally)", 1,
  "Norway",         2,          2008,        48,  "realized: contour terminates -\nno cohort after 1971\nreaches replacement",  1,
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
  geom_line(data = crossings %>% filter(!is.na(age_150)),
            aes(y = age_150), linewidth = 0.35, colour = "black") +
  geom_line(data = crossings %>% filter(!is.na(age_205)),
            aes(y = age_205), linewidth = 0.9, colour = "black") +
  geom_line(data = extrap_both %>% filter(as.integer(vintage) == 1),
            aes(group = country), colour = "magenta", linewidth = 0.8,
            linetype = "dotted") +
  geom_line(data = extrap_both %>% filter(as.integer(vintage) == 2),
            aes(group = country), colour = "magenta", linewidth = 0.5,
            linetype = "dotted", alpha = 0.55) +
  geom_text(data = ann, aes(label = label, hjust = hjust),
            size = 2.6, lineheight = 0.95, vjust = 1) +
  facet_grid(country ~ vintage) +
  coord_cartesian(xlim = c(1900, 2012), ylim = c(12, 50), expand = FALSE) +
  labs(
    x = "Birth year", y = "Age in years",
    title = "The 2020 extrapolations and what the new data showed",
    subtitle = paste0(
      "Shading: ASFR (darker = higher). Heavy line: age at which each cohort's cumulative fertility (CPCFR) reaches 2.05 (replacement); ",
      "thin line: 1.50.\nMagenta dotted: the 2020-era speculative extrapolations, redrawn from the September 2018 working figure ",
      "(ghosted on the updated panels for comparison)."
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

# Console check: crossing endpoints per vintage
crossings %>%
  filter(!is.na(age_205)) %>%
  group_by(vintage, country) %>%
  summarise(last_cohort = max(birth_year),
            age_at_last = age_205[birth_year == max(birth_year)],
            .groups = "drop") %>%
  as.data.frame() %>%
  print()

cat("forecast_check_2x2_2026.R complete\n")
