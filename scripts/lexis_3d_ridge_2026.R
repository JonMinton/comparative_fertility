# 3D rendering of an example Lexis surface with the AAPF ridge highlighted
# (Jon's direction, 2026-07-19). Norway: large population, clean single-peak
# schedules (the US plateau would make the ridge ill-defined - see F-002).
#
# The composite lattice plots in the paper are the bird's-eye view of this
# terrain; the AAPF (age and amount of peak fertility) is its crest line.
# Drawn on the year x age grid (complete rectangle; the squeeze quantities
# are year-indexed), base-graphics persp() with viridis facet shading and
# the ridge overlaid via trans3d().
#
# Output: figures/figures_2026/lexis3d_ridge_norway.png

library(readr)
library(dplyr)
library(tidyr)
library(viridis)

dta <- read_csv("data/data_combined_and_standardised_2026.csv",
                show_col_types = FALSE) %>%
  filter(code == "NOR", year >= 1950, age >= 12, age <= 50) %>%
  select(year, age, asfr) %>%
  complete(year = full_seq(year, 1), age = 12:50, fill = list(asfr = 0))

years <- sort(unique(dta$year))
ages  <- sort(unique(dta$age))
z <- matrix(dta$asfr[order(dta$year, dta$age)],
            nrow = length(years), ncol = length(ages), byrow = TRUE)

ridge <- dta %>%
  group_by(year) %>%
  summarise(peak_age = min(age[asfr == max(asfr)]),
            peak_asfr = max(asfr), .groups = "drop")

# facet colours: viridis reversed (darker = higher), matching the surfaces
nr <- nrow(z); nc <- ncol(z)
zfacet <- (z[-1, -1] + z[-1, -nc] + z[-nr, -1] + z[-nr, -nc]) / 4
pal <- viridis(120, direction = -1)
cols <- pal[cut(zfacet, 120, labels = FALSE)]

png("figures/figures_2026/lexis3d_ridge_norway.png",
    width = 24, height = 17, units = "cm", res = 300)
par(mar = c(1.2, 1.8, 2.2, 0.6))
pm <- persp(x = years, y = ages, z = z,
            theta = -38, phi = 27, expand = 0.55,
            col = cols, border = NA, shade = NA,
            xlab = "Year", ylab = "Age in years", zlab = "ASFR",
            ticktype = "detailed", cex.axis = 0.72, cex.lab = 0.85,
            main = "The fertility surface as terrain: Norway, 1950-2024, with the AAPF ridge",
            cex.main = 0.95)

lines(trans3d(ridge$year, ridge$peak_age, ridge$peak_asfr + 0.004, pm),
      col = "magenta", lwd = 2.6)

# label in the empty upper-left region, with a leader to the ridge start
p_lab <- trans3d(1953, 50, 0.185, pm)
p_tip <- trans3d(ridge$year[4], ridge$peak_age[4], ridge$peak_asfr[4] + 0.005, pm)
text(p_lab$x, p_lab$y, "AAPF ridge:\nage & amount of peak fertility",
     cex = 0.85, adj = c(0, 0.5), col = "magenta4")
segments(p_lab$x - 0.008, p_lab$y - 0.02, p_tip$x, p_tip$y,
         col = "magenta4", lwd = 0.8)

dev.off()

print(ridge[ridge$year %in% c(1950, 1975, 2000, 2024), ], n = 4)
cat("lexis_3d_ridge_2026.R complete\n")
