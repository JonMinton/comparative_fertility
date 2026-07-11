# Regenerate the published composite lattice figures on the 2026 data panel.
#
# Faithful adaptation of scripts/two_contour_version_of_figures.R (the script
# behind the published figures), reading the 2026 combined dataset instead.
# Deliberate choices, for comparability with the 2020 paper:
#   - same exclusions (CHL, CHN, TUR, GBR_NP, DEUTNP); RKS drops out via the
#     ranking mechanism as before -> same 45-country panel
#   - countries ranked by CCFR at 2007 as published (the new panel's last
#     common year is 2008 — Albania — but 2007 keeps the ordering directly
#     comparable with the published figures)
#   - same visual spec: viridis reversed, contours at 2.05 (lwd 2) and 1.50
#     (lwd 1), year >= 1950, age <= 50
# Changes:
#   - no latticeExtra: fill + contours drawn in one panel function (the CRAN
#     source build of latticeExtra's `interp` dependency fails on this
#     machine's toolchain; composition in-panel is visually identical)
#   - period gridlines extended beyond birth-year 2000 to cover the new decade
#   - Norway-only figure produced first (the paper's featured exception,
#     now reversed)
#   - outputs under figures/figures_2026/

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lattice)
library(RColorBrewer)
library(viridis)

dir.create("figures/figures_2026", showWarnings = FALSE, recursive = TRUE)

country_codes <- read_csv("data/hfc/code_definitions.csv", col_types = cols())

dta_simplified <- read_csv(
  "data/data_combined_and_standardised_2026.csv", col_types = cols()
) %>%
  select(code, year, age, asfr) %>%
  arrange(code, year, age)

dta <- dta_simplified %>%
  group_by(code, year) %>%
  arrange(age) %>%
  mutate(cpfr = lag(cumsum(asfr), 1, default = 0)) %>%
  mutate(birth_year = year - age) %>%
  arrange(code, birth_year, age) %>%
  group_by(code, birth_year) %>%
  mutate(my_ccfr = lag(cumsum(asfr), 1)) %>%
  ungroup()

selector <- dta %>%
  arrange(code, birth_year) %>%
  filter(!is.na(my_ccfr)) %>%
  group_by(code, birth_year) %>%
  summarise(min_age = min(age), max_age = max(age), .groups = "drop") %>%
  mutate(series_ok = min_age <= 16) %>%
  select(code, birth_year, series_ok)

dta <- dta %>% left_join(selector, by = c("code", "birth_year"))

dta <- dta %>% filter(!code %in% c("CHL", "CHN", "TUR", "GBR_NP", "DEUTNP"))
names(country_codes) <- tolower(names(country_codes))

dta <- dta %>%
  left_join(country_codes, by = "code") %>%
  filter(to_keep == 1) %>%
  select(-to_keep)

ordered_codes <- dta %>%
  filter(year == 2007) %>%
  filter(age <= 49) %>%
  group_by(code) %>%
  mutate(last_ccfr = max(my_ccfr, na.rm = TRUE)) %>%
  ungroup() %>%
  select(code, year, last_ccfr) %>%
  distinct() %>%
  mutate(fert_rank = dense_rank(last_ccfr)) %>%
  arrange(fert_rank)

write_csv(ordered_codes, "figures/figures_2026/ccfr_in_2007_from_2026_data.csv")
ordered_codes <- ordered_codes$code

tmp <- country_codes %>% select(country, code)
ordered_country_labels <- tmp %>%
  mutate(code = factor(code, levels = ordered_codes)) %>%
  filter(!is.na(code)) %>%
  arrange(code) %>%
  .$country
rm(tmp)

dta <- dta %>%
  mutate(country = factor(country, levels = rev(ordered_country_labels))) %>%
  filter(!is.na(country)) %>%
  arrange(country)

# Contour variable masked to cohorts observed from age <= 16, as published
dta <- dta %>% mutate(ccfr_ok = if_else(series_ok %in% TRUE, my_ccfr, NA_real_))

produce_composite_lattice <- function(DTA, add_gridlines = T,
                                      colscheme = viridis_pal(direction = -1)(200)) {
  colour_values <- c(
    "#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4",
    "#fed9a6", "#ffffcc", "#e5d8bd", "#fddaec"
  )
  country_codes %>% .$geography %>% unique() -> lbls
  lbls[!is.na(lbls)] -> lbls
  names(colour_values) <- lbls
  rm(lbls)

  DTA %>% group_by(country) %>% select(geography) %>% slice(1) %>% .$geography -> tmp
  colour_values_selection <- colour_values[tmp]
  rm(tmp)

  my_strip_style <- function(which.panel, factor.levels, ...) {
    panel.rect(0, 0, 1, 1,
               col = colour_values_selection[which.panel],
               border = 1)
    panel.text(x = 0.5, y = 0.5, cex = 0.7,
               lab = factor.levels[which.panel])
  }

  DTA_SS <- DTA %>%
    filter(year >= 1950) %>%
    filter(age <= 50) %>%
    as.data.frame()

  combined_panel <- function(x, y, z, subscripts, ...) {
    panel.levelplot(x, y, z, subscripts = subscripts, ...)
    if (add_gridlines) {
      panel.abline(h = seq(15, 45, by = 5), lty = "dashed", col = "grey")
      panel.abline(v = seq(1900, 2010, by = 5), lty = "dashed", col = "grey")
    }
    ccfr <- DTA_SS$ccfr_ok
    panel.contourplot(x, y, ccfr, subscripts = subscripts,
                      at = 2.05, lwd = 2, col = "black",
                      region = FALSE, contour = TRUE, labels = FALSE)
    panel.contourplot(x, y, ccfr, subscripts = subscripts,
                      at = 1.50, lwd = 1, col = "black",
                      region = FALSE, contour = TRUE, labels = FALSE)
  }

  levelplot(
    asfr ~ birth_year * age | country,
    data = DTA_SS,
    par.strip.text = list(cex = 0.80, fontface = "bold"),
    ylab = list(label = "Age in years", cex = 1.0),
    xlab = list(label = "Birth year", cex = 1.0),
    cex = 1.0,
    cuts = 30,
    aspect = "iso",
    col.regions = colscheme,
    labels = list(cex = 1.0),
    colorkey = list(
      space = "top",
      labels = list(cex = 1.0)
    ),
    col = "black",
    as.table = TRUE,
    strip = my_strip_style,
    scales = list(
      x = list(cex = 0.9, rot = 90),
      y = list(cex = 0.9),
      alternating = 3
    ),
    panel = combined_panel,
    par.settings = list(strip.background = list(col = "lightgrey"))
  )
}

# --- Norway first: the paper's featured exception, now reversed -------------

png("figures/figures_2026/norway_only.png",
    res = 300, width = 16, height = 14, units = "cm")
print(produce_composite_lattice(dta %>% filter(code == "NOR"), add_gridlines = F))
dev.off()

# --- Full 45-country composite ----------------------------------------------

png("figures/figures_2026/overall_gridded.png",
    res = 300, width = 40, height = 40, units = "cm")
print(produce_composite_lattice(dta, add_gridlines = F))
dev.off()

# --- Three-way split, 15 per page, as in the manuscript ----------------------

lv <- levels(dta$country)[levels(dta$country) %in% unique(as.character(dta$country))]
splits <- split(lv, ceiling(seq_along(lv) / 15))
for (i in seq_along(splits)) {
  png(sprintf("figures/figures_2026/overall_split_%d.png", i),
      res = 300, width = 18, height = 15, units = "cm")
  print(produce_composite_lattice(
    dta %>% filter(country %in% splits[[i]]) %>% mutate(country = droplevels(country)),
    add_gridlines = F
  ))
  dev.off()
}

cat("Countries in panel:", length(lv), "\n")
cat("Max year in figures:", max(dta$year), " Max birth year:", max(dta$birth_year), "\n")
