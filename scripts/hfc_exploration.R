
rm(list=ls())


require(readr)

require(tidyr)
require(stringr)
require(dplyr)
require(purrr)


require(ggplot2)
require(lattice)
require(latticeExtra)


# Load data ---------------------------------------------------------------


dta_hfc <- read_csv("data/hfc/HFC_ASFRstand_TOT/HFC_ASFRstand_TOT.txt")

dta_hfd <- read_csv("data/lexis_square_combined.csv")

country_codes <- read_csv("data/hfc/code_definitions.csv")
# work on tidying/filtering hfc -------------------------------------------

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




# Plot new lattice --------------------------------------------------------



shading <- dta_both_simple %>% 
  filter(year >= 1900) %>% 
  filter( age <= 50 ) %>% 
  filter(!(code %in% c("DEUTNP", "GBR_NP"))) %>% 
  levelplot(
    asfr ~ year * age | code, 
    data=. , 
    par.strip.text=list(cex=1.2, fontface="bold"),
    ylab=list(label="Age in years", cex=1.2),
    xlab=list(label="Year", cex=1.2),
    cex=1.4,
    cuts=20,
    col.regions=colorRampPalette(brewer.pal(6, "Purples"))(200),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.2), 
      y=list(cex=1.2),
      alternating=3
    ),
    par.settings=list(strip.background=list(col="lightgrey"))
  ) 

replace_line <- dta %>% 
  filter(year >= 1950) %>% 
  filter( age <= 50 ) %>% 
  filter(!(code %in% c("DEUTNP", "GBR_NP"))) %>% 
  filter(series_ok == TRUE) %>% 
  
  contourplot(
    my_ccfr ~ birth_year * age | code, 
    data=. , 
    region = F,
    ylab = "",
    xlab = "", 
    scales = list(NULL),
    at = c(1.5, 2.05), 
    lwd = 2, 
    labels = F
  )

near_line <- dta %>% 
  filter(year >= 1950) %>% 
  filter( age <= 50 ) %>% 
  filter(!(code %in% c("DEUTNP", "GBR_NP"))) %>% 
  filter(series_ok == TRUE) %>% 
  contourplot(
    my_ccfr ~ birth_year * age | code, 
    data=. , 
    region = F,
    ylab = "",
    xlab = "", 
    scales = list(NULL),
    at = c(1.30, 1.80), 
    lwd = 1,
    lty= "dashed",
    labels = F
  )


png("figures/ccfr/latticeplot.png",
    res=300, width=30, height=30, units = "cm"
)
print(shading + replace_line + near_line)

dev.off()



