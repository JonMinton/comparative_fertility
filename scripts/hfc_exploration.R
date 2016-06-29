
rm(list=ls())


require(pacman)

pacman::p_load(
  readr, car,
  tidyr,  stringr, dplyr,
  purrr, r2stl, 
  ggplot2, lattice, latticeExtra,
  RColorBrewer
)


# Load data ---------------------------------------------------------------


dta_hfc <- read_csv("data/hfc/HFC_ASFRstand_TOT/HFC_ASFRstand_TOT.txt")

dta_hfd <- read_csv("data/lexis_square_combined.csv")

country_codes <- read_csv("data/hfc/code_definitions.csv")
# work on tidying/filtering hfc -------------------------------------------


# Simple illustration of asfr and age relationship

dta_hfd  %>% 
  filter(code %in% c("GBRTENW", "GBR_SCO"))  %>% 
  filter(year %in% c(1960, 1980, 2000))  %>% 
  mutate(
    year = factor(year), 
    country = recode(code, "'GBRTENW' = 'England & Wales'; 'GBR_SCO' = 'Scotland'")
    ) %>% 
  ggplot(.) + 
  geom_line(
    aes(x = age, y = asfr, group = year, colour = year, linetype = year)) + 
  facet_wrap(~country) + 
  labs(x = "Age in years", y = "Age-specific fertility rate") + 
  theme_minimal()

ggsave(filename = "figures/asfr_example.png", height = 20, width = 30, units = "cm", dpi = 300)
# So, the rule is 
# 1 ) See if the country is in the countries to include 
# 2 ) See if year1 and year2 match. Discard if they do not
# 3 ) Select AgeDef == ACY
# 4 ) Use following collection preference: i) STAT; ii) ODE; iii) RE
# 5 ) Use HFD in preference to HFC

dta_hfc_simple <- dta_hfc  %>% 
  rename(Code = Country)  %>% 
  left_join(country_codes)  %>% 
  filter(to_keep == 1)  %>% 
  mutate(yeardif = Year1 == Year2)  %>% 
  filter(yeardif == T)  %>% 
  filter(AgeDef == "ACY") %>% 
  filter(Collection %in% c("STAT", "ODE")) %>% 
  select(code = Code, year = Year1, age = Age, asfr = ASFR, collection = Collection, refcode = RefCode)

dta_hfd_simple <- dta_hfd %>% 
  select(code, year, age, asfr) %>% 
  mutate(collection = "HFD") %>% 
  mutate(refcode = NA)


dta_both_simple <- bind_rows(dta_hfc_simple, dta_hfd_simple)

select_best_asfr <- function(x){
  best_asfr <- NA
  
  collections <- unique(x$collection) 
  
  if("HFD" %in% collections){
    best_asfr <- x %>% filter(collection == "HFD") %>% .$asfr
  } else if ("STAT" %in% collections) {
    asfrs <- x %>% filter(collection == "STAT") %>% .$asfr
    if(length(asfrs)==1) {
      best_asfr <- asfrs
    } else {
      best_asfr <- mean(asfrs, na.rm = T)
    }
  } else if ("ODE" %in% collections) {
    asfrs <- x %>% filter(collection == "ODE") %>% .$asfr
    if(length(asfrs)==1) {
      best_asfr <- asfrs
    } else {
      best_asfr <- mean(asfrs, na.rm = T)
    }
    
  }
  return(best_asfr)
}

dta_both_simple %>% 
  group_by(code, year, age) %>% 
  nest %>% 
  mutate(best_asfr = map_dbl(data, select_best_asfr)) %>% 
  select(code, year, age, asfr = best_asfr) -> dta_simplified

# Values for BIH (Bosnia and Hertzegovina) are missing 
# for the years 1991 to 1995. However, the rest of the series 
# seems relatively complete. 
# Because of this, I will produce a linear imputation of the ASFRs 
# based on the 1990 to 1996 years, weighted towards the nearest year


tmp <- dta_simplified %>% 
  filter(code == "BIH") %>% 
  filter(year %in% c(1990, 1996)) %>% 
  select(year, age, asfr)  %>% 
  arrange(age)  %>% 
  spread(year, asfr)

tmp2 <- expand.grid(age = 14:50, year = 1991:1995) %>% tbl_df

tmp3 <- tmp2 %>% left_join(tmp) 

tmp4 <- tmp3 %>% mutate(
  dif = 1996 - 1990, 
  x1 = (1996 - year) / dif, 
  x2 = 1 - x1, 
  weighted_asfr = x1 * `1990`  + x2 * `1996`) %>% 
  mutate(code = "BIH") %>% 
  select(code, year, age, asfr = weighted_asfr)

dta_simplified <- bind_rows(dta_simplified, tmp4)

rm(tmp, tmp2, tmp3, tmp4)

# Finished imputation of BIH missing years

# Now to do something similar for Belarus - though just one year (2013) missing!
tmp <- dta_simplified %>% 
  filter(code == "BLR") %>% 
  filter(year %in% c(2012, 2014)) %>% 
  select(year, age, asfr)  %>% 
  arrange(age)  %>% 
  spread(year, asfr)

tmp2 <- expand.grid(age = 14:50, year = 2013) %>% tbl_df

tmp3 <- tmp2 %>% left_join(tmp) 

tmp4 <- tmp3 %>% mutate(
  dif = 2014 - 2012, 
  x1 = (2014 - year) / dif, 
  x2 = 1 - x1, 
  weighted_asfr = x1 * `2012`  + x2 * `2014`) %>% 
  mutate(code = "BLR") %>% 
  select(code, year, age, asfr = weighted_asfr)

dta_simplified <- bind_rows(dta_simplified, tmp4)

rm(tmp, tmp2, tmp3, tmp4)



#####


dta <- dta_simplified %>% 
  mutate(birth_year = year - age) %>% 
  group_by(code, year) %>% 
  mutate(cpfr = lag(cumsum(asfr), 1)) %>% 
  ungroup 

#germanies <- dta %>% filter(code %in% c("DEUTE", "DEUTW"))


dta <- dta  %>% 
  arrange(code, birth_year, age) %>%  
  group_by(code, birth_year) %>%  
  mutate(my_ccfr = lag(cumsum(asfr), 1)) %>% 
  ungroup

selector <- dta  %>% 
  arrange(code, birth_year)  %>% 
  filter(!is.na(my_ccfr))  %>% 
  group_by(code, birth_year)  %>% 
  summarise(min_age = min(age), max_age = max(age))  %>% 
  mutate(series_ok = min_age <= 16)  %>% 
  select(code, birth_year, series_ok)


dta <- dta %>% left_join(selector)

dta <- dta %>% filter(!code %in% c("CHL", "CHN", "TUR", "GBR_NP", "DEUTNP"))
names(country_codes) <- tolower(names(country_codes))

dta <- dta %>% 
  left_join(country_codes) %>% 
  filter(to_keep == 1) %>% 
  select(-to_keep)



# Order the plots by fertility level in last year 

ordered_codes <- dta  %>% 
  group_by(code)  %>% 
  filter(year == max(year))  %>% 
  mutate(last_ccfr = max(my_ccfr, na.rm= T))  %>% 
  ungroup  %>% 
  select(code, year, last_ccfr)  %>% 
  distinct  %>% 
  mutate(fert_rank = dense_rank(last_ccfr))  %>% 
  arrange(fert_rank)  %>% 
  .$code

country_codes  %>% select(country, code)   -> tmp

tmp  %>% 
  mutate(code = factor(code, levels = ordered_codes))  %>% 
  filter(!is.na(code))  %>% 
  arrange(code)  %>% 
  .$country -> ordered_country_labels

rm(tmp)

dta <- dta  %>% mutate(country = factor(country, levels = rev(ordered_country_labels)))

dta <- dta %>% arrange(country)
# Plot new lattice --------------------------------------------------------

# The important and slightly challenging new tasks are: 
# 1) To replace each of the codes with the country names - DONE
# 2) To have the colours of the strips dependent on the 
# country region
# 3) To have the order of the strips dependent on the highest 
# fertility rate in the last year  - DONE
# 4) To remove CHL and CHN, TUR and GBR_NP - DONE


# Strip function
# http://stackoverflow.com/questions/8536239/change-background-and-text-of-strips-associated-to-muliple-panels-in-r-lattice/8537752#8537752

colour_values <- c(
  "#fbb4ae",
  "#b3cde3",
  "#ccebc5",
  "#decbe4",
  "#fed9a6",
  "#ffffcc",
  "#e5d8bd",
  "#fddaec"
) # Picked using colorbrewer2 website - qualitative, 7 class
country_codes  %>% .$geography  %>% unique()  -> lbls
lbls[!is.na(lbls)] -> lbls

names(colour_values) <- lbls
rm(lbls)

dta  %>% group_by(country)  %>% select(geography)  %>% slice(1)  %>% .$geography -> tmp
colour_values_selection <- colour_values[tmp]
rm(tmp)

my_strip_style <- function(which.panel, factor.levels, ...) {
  panel.rect(0, 0, 1, 1,
             col = colour_values_selection[which.panel],
             border = 1)
  panel.text(x = 0.5, y = 0.5,
             lab = factor.levels[which.panel]
             )
}    



shading <- dta %>% 
  filter(year >= 1950) %>% 
  filter( age <= 50 ) %>% 
  levelplot(
    asfr ~ birth_year * age | country, 
    data=. , 
    par.strip.text=list(cex=1.1, fontface="bold"),
    ylab=list(label="Age in years", cex=1.2),
    xlab=list(label="Birth Year", cex=1.2),
    cex=1.4,
    cuts=20,
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    labels=list(cex=1.2),
    col="black", 
    as.table = TRUE,
    strip = my_strip_style,
    scales=list(
      x=list(cex=1.2), 
      y=list(cex=1.2),
      alternating=3
    ),
    par.settings=list(strip.background=list(col="lightgrey"))
  ) 

line_2_05 <- dta %>% 
  filter(year >= 1950) %>% 
  filter( age <= 50 ) %>% 
  filter(series_ok == TRUE) %>% 
  contourplot(
    my_ccfr ~ birth_year * age | country, 
    data=. , 
    region = F,
    as.table = TRUE,
    ylab = "",
    xlab = "", 
    scales = list(NULL),
    at = 2.05, 
    lwd = 2, 
    labels = F
  )

line_1_80 <- dta %>% 
  filter(year >= 1950) %>% 
  filter( age <= 50 ) %>% 
  filter(series_ok == TRUE) %>% 
  contourplot(
    my_ccfr ~ birth_year * age | country, 
    data=. , 
    region = F,
    as.table = TRUE,
    ylab = "",
    xlab = "", 
    scales = list(NULL),
    at = 1.80, 
    lwd = 2,
    lty= "dashed",
    labels = F
  )

line_1_50 <- dta %>% 
  filter(year >= 1950) %>% 
  filter( age <= 50 ) %>% 
  filter(series_ok == TRUE) %>% 
  contourplot(
    my_ccfr ~ birth_year * age | country, 
    data=. , 
    region = F,
    as.table = TRUE,
    ylab = "",
    xlab = "", 
    scales = list(NULL),
    at = 1.50, 
    lwd = 1, 
    labels = F
  )

line_1_30 <- dta %>% 
  filter(year >= 1950) %>% 
  filter( age <= 50 ) %>% 
  filter(series_ok == TRUE) %>% 
  contourplot(
    my_ccfr ~ birth_year * age | country, 
    data=. , 
    region = F,
    as.table = TRUE,
    ylab = "",
    xlab = "", 
    scales = list(NULL),
    at = 1.30, 
    lwd = 1,
    lty= "dashed",
    labels = F
  )


png("figures/ccfr/hfd_hfc_combined_latticeplot.png",
    res=600, width=60, height=40, units = "cm"
)
print(shading + line_2_05 + line_1_80 + line_1_50 + line_1_30)

dev.off()



# Separate complex figures for all countries  -----------------------------

# ASFR, some contours

fn <- function(x) {
  this_title = paste0(x$country[1], " (", x$geography[1], ")")
  x %>% 
    mutate(asfr = 100 * asfr) %>% 
    contourplot(
      asfr ~ year * age , 
      data=. , 
      region = T,
      xlim = c(1950, 2015),
      ylim = c(15, 50),
      ylab = "Age of woman",
      xlab = "Year", 
      scales = list(NULL),
      lwd = 1,
      labels = list(col = "darkgreen", cex = 0.7),
      aspect = "iso",
      col = "grey",
      at = c(0, seq(5, 40, 5)),
      col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
      main = this_title
    ) -> output
  
  png(
    paste0("figures/asfr/", this_title, ".png"),
    width = 15, height = 12, units = "cm", res = 300
  )  
  print(output)
  dev.off()
  return(output)
}



dta %>% 
  group_by(code) %>% 
  nest() %>% 
  mutate(figure = map(data, fn))

# ASFR, heatmap 

fn <- function(x) {
  this_title = paste0(x$country[1], " (", x$geography[1], ")")
  x %>% 
    mutate(asfr = 100 * asfr) %>% 
    levelplot(
      asfr ~ year * age , 
      data=. , 
      xlim = c(1950, 2015),
      ylim = c(15, 50),
      ylab = "Age of woman",
      xlab = "Year", 
      scales = list(NULL),
      lwd = 1,
      aspect = "iso",
      col = "grey",
      at = c(0:40),
      col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
      main = this_title
    ) -> output
  
  png(
    paste0("figures/asfr_heat/", this_title, ".png"),
    width = 15, height = 12, units = "cm", res = 300
  )  
  print(output)
  dev.off()
  return(output)
}


dta %>% 
  group_by(code) %>% 
  nest() %>% 
  mutate(figure = map(data, fn))

# Now CCFR with heat only

fn <- function(x){
  this_title = paste0(x$country[1], " (", x$geography[1], ")")
  x %>% 

    filter( age <= 50 ) %>%
    mutate(asfr = 100 * asfr) %>% 
    levelplot(
      asfr ~ birth_year * age, 
      data=. , 
      ylab="Age in years",
      xlab="Birth Year",
      cuts=20,
      xlim = c(1900, 2000),
      ylim = c(15, 50),
      at = c(0:40),
      col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
      labels=list(cex=1.2),
      col="black", 
      main = this_title
    )   -> output
  

  png(
    paste0("figures/asfr_cohort_heat/", this_title, ".png"),
    width = 15, height = 12, units = "cm", res = 300
  )  
  print(output)
  dev.off()
  return(output)
  
}

dta %>% 
  group_by(code) %>% 
  nest() %>% 
  mutate(figure = map(data, fn))


# Now finally CCFR with selected contours added 


fn <- function(x){
  this_title = paste0(x$country[1], " (", x$geography[1], ")")
  x %>% 
    filter( age <= 50 ) %>%
    mutate(asfr = 100 * asfr) %>% 
    levelplot(
      asfr ~ birth_year * age, 
      data=. , 
      ylab="Age in years",
      xlab="Birth Year",
      cuts=20,
      xlim = c(1900, 2000),
      ylim = c(15, 50),
      at = c(0:40),
      col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
      labels=list(cex=1.2),
      col="black", 
      main = this_title
    )   -> part1
  
  x %>% 
    filter( age <= 50 ) %>% 
    filter(series_ok == TRUE) %>% 
    contourplot(
      my_ccfr ~ birth_year * age, 
      data=. , 
      region = F,
      as.table = TRUE,
      ylab = "",
      xlab = "", 
      scales = list(NULL),
      at = c(1.30, 1.50, 1.80, 2.05), 
      lwd = 1,
      labels = list(col = "darkgreen")
    ) -> part2
  
  output <- part1 + part2
  
  png(
    paste0("figures/ccfr/", this_title, ".png"),
    width = 15, height = 12, units = "cm", res = 300
  )  
  print(output)
  dev.off()
  return(output)
  
}

dta %>% 
  group_by(code) %>% 
  nest() %>% 
  mutate(figure = map(data, fn))








# Stl file for East and West Germany --------------------------------------
max_val <- max(germanies$asfr)
min_val <- 0.03 * max_val
east_matrix <- germanies %>% 
  select(code, year, age, asfr) %>% 
  mutate(asfr = ifelse(asfr < min_val, min_val, asfr))
east_matrix$asfr[is.na(east_matrix$asfr)] <- min_val
east_matrix <-   east_matrix %>%  
  filter(code == "DEUTE") %>% 
  select(-code) %>% 
  spread(age, asfr, fill = min_val) %>% as.matrix()
rownames(east_matrix) <- east_matrix[,1]
east_matrix <- east_matrix[,-1]

west_matrix <- germanies %>% 
  select(code, year, age, asfr) 
west_matrix$asfr[is.na(west_matrix$asfr)] <- min_val
west_matrix <-   west_matrix %>%  
  mutate(asfr = asfr + min_val) %>% 
  filter(year >= 1952) %>% 
  filter(code == "DEUTW") %>% 
  select(-code) %>% 
  spread(age, asfr, fill = min_val) %>% as.matrix()
rownames(west_matrix) <- west_matrix[,1]
west_matrix <- west_matrix[,-1]


both_matrix <- cbind(east_matrix, west_matrix)

dimnames(both_matrix) <- list(1:dim(both_matrix)[1], 1:dim(both_matrix)[2])

r2stl(
  x = as.numeric(rownames(both_matrix)), 
  y = as.numeric(colnames(both_matrix)),
  z = both_matrix, 
  show.persp = T,
  filename = "stl/both_germanies.stl"
)


