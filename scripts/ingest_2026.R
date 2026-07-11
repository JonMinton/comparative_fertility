# Rebuild the combined ASFR dataset from the 2026 HFD/HFC downloads.
#
# Reimplements the combination rules of the published pipeline
# (scripts/hfc_hfd_data_combine.R) against the new raw files, leaving the
# original script untouched so the 2020 paper remains reproducible.
# Rules (Pattaro, Vanderbloemen & Minton 2020, Demographic Research 42(23)):
#   1. HFD preferred over HFC for any country-year
#   2. Within HFC: single-year periods only (Year1 == Year2); AgeDef == "ACY";
#      collection preference STAT > ODE > RE
#   3. Linear interpolation across small internal year gaps
# Differences from the 2016 build, by design:
#   - HFD code FRATNP mapped to FRA (old build lost France post-2008 to this mismatch)
#   - a `source` column (hfd / hfc / interp) is kept for validation and for the
#     paper's "HFD-only" sensitivity option
#   - internal gaps longer than MAX_GAP years are left missing and reported,
#     not silently interpolated

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

MAX_GAP <- 4

# --- HFD ------------------------------------------------------------------

hfd <- read_table("data/hfd_2026/asfrRR.txt", skip = 2,
                  col_types = cols(.default = col_character()))

hfd <- hfd %>%
  transmute(
    code = if_else(Code == "FRATNP", "FRA", Code),
    year = as.integer(Year),
    age  = as.integer(str_remove(Age, "[-+]$")),
    asfr = suppressWarnings(as.numeric(ASFR))
  ) %>%
  filter(!is.na(asfr)) %>%
  mutate(source = "hfd")

# --- HFC ------------------------------------------------------------------

country_codes <- read_csv("data/hfc/code_definitions.csv", col_types = cols())

hfc <- read_csv("data/hfc_2026/HFC_ASFRstand_TOT.txt",
                col_types = cols(.default = col_character()))

hfc <- hfc %>%
  rename(Code = Country) %>%
  semi_join(filter(country_codes, to_keep == 1), by = "Code") %>%
  filter(Year1 == Year2, AgeDef == "ACY",
         Collection %in% c("STAT", "ODE", "RE")) %>%
  transmute(
    code = Code,
    year = as.integer(Year1),
    age  = suppressWarnings(as.integer(str_remove(Age, "[-+]$"))),
    asfr = suppressWarnings(as.numeric(ASFR)),
    collection = Collection,
    refcode = RefCode
  ) %>%
  filter(!is.na(asfr), !is.na(age))

# One series per country-year: best collection, then most complete refcode
# (tie-break: alphabetically last refcode, which tends to be the more recent
# source). Report-level consequences of this choice are checked in validation.
hfc_best <- hfc %>%
  mutate(pref = match(collection, c("STAT", "ODE", "RE"))) %>%
  group_by(code, year) %>%
  filter(pref == min(pref)) %>%
  group_by(code, year, refcode) %>%
  mutate(n_ages = n()) %>%
  group_by(code, year) %>%
  filter(n_ages == max(n_ages)) %>%
  filter(refcode == max(refcode)) %>%
  ungroup() %>%
  distinct(code, year, age, .keep_all = TRUE) %>%
  select(code, year, age, asfr) %>%
  mutate(source = "hfc")

# --- Combine: HFD wins per country-year ------------------------------------

hfc_only <- hfc_best %>% anti_join(hfd, by = c("code", "year"))
combined <- bind_rows(hfd, hfc_only)

# --- Interpolate small internal year gaps, per country-age series ----------

interpolated <- combined %>%
  group_by(code, age) %>%
  complete(year = full_seq(year, 1)) %>%
  arrange(year) %>%
  mutate(
    gap_id  = cumsum(!is.na(asfr)),
    gap_len = ave(is.na(asfr), gap_id, FUN = sum)
  ) %>%
  mutate(
    asfr_i = approx(year[!is.na(asfr)], asfr[!is.na(asfr)], xout = year)$y,
    source = case_when(
      !is.na(asfr)                        ~ source,
      gap_len <= MAX_GAP & !is.na(asfr_i) ~ "interp",
      TRUE                                ~ NA_character_
    ),
    asfr = if_else(is.na(asfr) & source %in% "interp", asfr_i, asfr)
  ) %>%
  ungroup() %>%
  filter(!is.na(asfr), !is.na(source)) %>%
  select(code, year, age, asfr, source)

# --- Write ------------------------------------------------------------------

out <- interpolated %>% arrange(code, year, age)
write_csv(out, "data/data_combined_and_standardised_2026.csv")

# Console summary for the validation step
out %>%
  group_by(code) %>%
  summarise(first = min(year), last = max(year),
            n_hfd = sum(source == "hfd"),
            n_hfc = sum(source == "hfc"),
            n_interp = sum(source == "interp"), .groups = "drop") %>%
  print(n = 60)

cat("\nRows:", nrow(out), " Countries:", n_distinct(out$code), "\n")
