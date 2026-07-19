# 3D Lexis surfaces with the AAPF ridge cells highlighted, beside the AAPF
# trajectory each surface distils to (Jon's design, 2026-07-19; F-027 + this
# round's refinements):
#   - 3 rows x 2 cols: per country, 3D terrain (left) and AAPF chart (right)
#   - countries: France, Norway, South Korea (USA excluded: plateau schedules
#     make the ridge ill-defined - see supplement S8)
#   - viewing angle theta = +30 (age axis low-to-high visible on the right
#     edge) and phi raised (more overhead) so the correspondence with the 2D
#     bird's-eye lattice plots is easier to parse
#   - the ridge is shown by COLOURING the peak cells (bright magenta), not by
#     a line hovering above the surface: the marked cells ARE the AAPF.
#     (Bright magenta, not dark purple: the viridis-reversed scale already
#     ends in dark purple at high rates, exactly where the ridge sits.)
#
# Output: figures/figures_2026/lexis3d_aapf_panels.png

library(readr)
library(dplyr)
library(tidyr)
library(viridis)

COUNTRIES <- c(FRA = "France", NOR = "Norway", KOR = "South Korea")
RIDGE_COL <- "magenta"

dta_all <- read_csv("data/data_combined_and_standardised_2026.csv",
                    show_col_types = FALSE) %>%
  filter(code %in% names(COUNTRIES), year >= 1950, age >= 12, age <= 50) %>%
  select(code, year, age, asfr) %>%
  group_by(code) %>%
  complete(year = full_seq(year, 1), age = 12:50, fill = list(asfr = 0)) %>%
  ungroup()

pal <- viridis(120, direction = -1)
asfr_max <- max(dta_all$asfr, na.rm = TRUE)

png("figures/figures_2026/lexis3d_aapf_panels.png",
    width = 24, height = 30, units = "cm", res = 300)
layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 7), ncol = 2, byrow = TRUE),
       widths = c(1.15, 1), heights = c(1, 1, 1, 0.22))

for (cc in names(COUNTRIES)) {
  d <- dta_all %>% filter(code == cc)
  years <- sort(unique(d$year))
  ages  <- sort(unique(d$age))
  z <- matrix(d$asfr[order(d$year, d$age)],
              nrow = length(years), ncol = length(ages), byrow = TRUE)

  ridge <- d %>%
    group_by(year) %>%
    summarise(peak_age = min(age[asfr == max(asfr)]),
              peak_asfr = max(asfr), .groups = "drop")

  # facet colours: shared viridis-reversed scale; ridge cells overpainted
  nr <- nrow(z); nc <- ncol(z)
  zfacet <- (z[-1, -1] + z[-1, -nc] + z[-nr, -1] + z[-nr, -nc]) / 4
  cols <- matrix(pal[pmax(1, ceiling(120 * zfacet / asfr_max))],
                 nrow = nr - 1, ncol = nc - 1)
  for (i in seq_len(nr - 1)) {
    pk <- ridge$peak_age[ridge$year == years[i]]
    j <- match(pk, ages)
    if (!is.na(j) && j <= nc - 1) cols[i, j] <- RIDGE_COL
  }

  # --- left: terrain -----------------------------------------------------
  par(mar = c(0.4, 1.2, 1.6, 0.2))
  persp(x = years, y = ages, z = z,
        theta = 30, phi = 42, expand = 0.5,
        col = as.vector(cols), border = NA, shade = NA,
        xlab = "Year", ylab = "Age", zlab = "ASFR",
        ticktype = "detailed", cex.axis = 0.55, cex.lab = 0.7,
        zlim = c(0, asfr_max),
        main = sprintf("%s: the surface as terrain", COUNTRIES[cc]),
        cex.main = 0.9)

  # --- right: the AAPF trajectory the ridge distils to -------------------
  par(mar = c(2.6, 3.2, 1.6, 0.8))
  plot(ridge$year, ridge$peak_age, type = "n",
       xlim = range(dta_all$year), ylim = c(18, 36),
       xlab = "", ylab = "", axes = FALSE,
       main = sprintf("%s: AAPF", COUNTRIES[cc]), cex.main = 0.9)
  axis(1, cex.axis = 0.7, padj = -1.2)
  axis(2, cex.axis = 0.7, las = 1, hadj = 0.7)
  mtext("Age of peak fertility", side = 2, line = 2.0, cex = 0.55)
  points(ridge$year, ridge$peak_age, pch = 16, cex = 0.75,
         col = pal[pmax(1, ceiling(120 * ridge$peak_asfr / asfr_max))])
  box(col = "grey60")
}

# --- shared colour key --------------------------------------------------
par(mar = c(2.2, 8, 1.4, 8))
key_vals <- seq(0, asfr_max, length.out = 120)
image(x = key_vals, y = 1, z = matrix(seq_along(key_vals), ncol = 1),
      col = pal, axes = FALSE, xlab = "", ylab = "")
axis(1, at = pretty(key_vals), cex.axis = 0.7, padj = -1)
mtext("ASFR (shading of surface and AAPF points); magenta cells: the AAPF ridge",
      side = 3, line = 0.2, cex = 0.6)
box(col = "grey60")

dev.off()
cat("lexis_3d_ridge_2026.R complete\n")
