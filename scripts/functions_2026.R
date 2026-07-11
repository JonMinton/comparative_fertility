# Shared functions for the 2026 update pipeline.
# Sourced by scripts/figures_2026.R (Phase 2) and scripts/wall_2026.R (Phase 3).
# The 2016-2020 scripts behind the published paper are untouched.

# Build the analysis dataset from the 2026 combined CSV, replicating the
# published prep: CPFR/CCFR computation, series_ok selector (cohort observed
# from age <= 16), published exclusions, country ordering by CCFR at 2007.
# Returns list(dta, country_codes).
prepare_dta_2026 <- function(path = "data/data_combined_and_standardised_2026.csv") {
  country_codes <- read_csv("data/hfc/code_definitions.csv", col_types = cols())

  dta <- read_csv(path, col_types = cols()) %>%
    select(code, year, age, asfr) %>%
    arrange(code, year, age) %>%
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

  dta <- dta %>%
    left_join(selector, by = c("code", "birth_year")) %>%
    filter(!code %in% c("CHL", "CHN", "TUR", "GBR_NP", "DEUTNP"))

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

  dta <- dta %>%
    mutate(country = factor(country, levels = rev(ordered_country_labels))) %>%
    filter(!is.na(country)) %>%
    arrange(country) %>%
    mutate(ccfr_ok = if_else(series_ok %in% TRUE, my_ccfr, NA_real_))

  list(dta = dta, country_codes = country_codes)
}

# Composite Lexis surface: ASFR fill + CPCFR milestone contours (2.05, 1.50),
# visually matching the published figures (drawn in one panel function; no
# latticeExtra). If DTA carries an `is_wall` logical column, the effective
# fertility ceiling is overdrawn as a dotted red line.
produce_composite_lattice <- function(DTA, country_codes, add_gridlines = TRUE,
                                      colscheme = viridis_pal(direction = -1)(200)) {
  colour_values <- c(
    "#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4",
    "#fed9a6", "#ffffcc", "#e5d8bd", "#fddaec"
  )
  lbls <- unique(country_codes$geography)
  lbls <- lbls[!is.na(lbls)]
  names(colour_values) <- lbls

  tmp <- DTA %>% group_by(country) %>% select(geography) %>% slice(1) %>% .$geography
  colour_values_selection <- colour_values[tmp]

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

  has_wall <- "is_wall" %in% names(DTA_SS)

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
    if (has_wall) {
      sub <- DTA_SS[subscripts, ]
      w <- sub[sub$is_wall %in% TRUE, ]
      if (nrow(w) > 1) {
        w <- w[order(w$birth_year + w$age), ]
        panel.lines(w$birth_year, w$age, col = "firebrick", lwd = 2, lty = 3)
      }
    }
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
