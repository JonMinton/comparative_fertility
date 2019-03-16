# 28/9/2018 

# two contour versions of figures 

rm(list=ls())


require(pacman)

pacman::p_load(
  readr, car,
  tidyr,  stringr, dplyr,
  purrr, r2stl, 
  ggplot2, lattice, latticeExtra,
  RColorBrewer
)

# Data 
country_codes <- read_csv("data/hfc/code_definitions.csv")

if(file.exists("data/data_combined_and_standardised.csv")){
  dta_simplified <- read.csv(
    "data/data_combined_and_standardised.csv"
  ) %>% tbl_df %>% 
    arrange(code, year, age) 
} else {
  source("scripts/hfc_hfd_data_combine.R")
}





dta <- dta_simplified %>% 
  group_by(code, year) %>% 
  arrange(age) %>% 
  mutate(cpfr = lag(cumsum(asfr), 1, default = 0)) %>% 
  mutate(birth_year = year - age) %>% 
  arrange(code, birth_year, age) %>%  
  group_by(code, birth_year) %>%  
  mutate(my_ccfr = lag(cumsum(asfr), 1)) %>% 
  ungroup()

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

# Want to know what is the last common year

dta %>% 
  group_by(code) %>% 
  summarise(max_year = max(year)) %>% 
  arrange(max_year)

# Last common is 2007 

ordered_codes <- dta  %>% 
  filter(year == 2007)  %T>% print(sample_n(10)) %>% 
  group_by(code) %>% 
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

adjusted_paired <- brewer.pal(12, "Paired")[c(1,2,3,4,9,10,7,8,5,6,11,12)]


# Now only 2.05 and 1.50
produce_composite_lattice <- function(DTA, add_gridlines = T,
                                      colscheme = colorRampPalette(adjusted_paired)(200),
                                      return = "all"
                                      
){
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
  
  DTA  %>% group_by(country)  %>% select(geography)  %>% slice(1)  %>% .$geography -> tmp
  colour_values_selection <- colour_values[tmp]
  rm(tmp)
  
  my_strip_style <- function(which.panel, factor.levels, ...) {
    panel.rect(0, 0, 1, 1,
               col = colour_values_selection[which.panel],
               border = 1)
    panel.text(x = 0.5, y = 0.5, cex = 0.7,
               lab = factor.levels[which.panel]
    )
  }    
  
  panel_select <- function(add_gridlines){
    if(!add_gridlines){
      return(
        function(...){
          panel.levelplot(...)
        }
      )
    } else {
      return(
        function(...){
          panel.levelplot(...)
          panel.abline(h = seq(15, 45, by = 5), lty = "dashed", col = "grey")
          panel.abline(v = seq(1900, 2000, by = 5), lty = "dashed", col = "grey")
        }
      )
    }
    NULL
  }
  
  
  panel_select2 <- function(add_gridlines){
    if(!add_gridlines){
      return(
        function(...){
          panel.contourplot(...)
        }
      )
    } else {
      return(
        function(...){
          panel.contourplot(...)
          panel.abline(h = seq(15, 45, by = 5), lty = "dashed", col = "grey")
          panel.abline(v = seq(1900, 2000, by = 5), lty = "dashed", col = "grey")
        }
      )
    }
    NULL
  }
  
  DTA_SS <- DTA %>% 
    filter(year >= 1950) %>% 
    filter(age <= 50)
  
  shading <- DTA_SS %>% 
    levelplot(
      asfr ~ birth_year * age | country, 
      data= . , 
      par.strip.text=list(cex=0.80, fontface="bold"),
      ylab=list(label="Age in years", cex=1.0),
      xlab=list(label="Birth year", cex=1.0),
      cex=1.0,
      cuts=30,
      aspect = "iso",
      col.regions=colscheme,
      labels=list(cex=1.0),
      colorkey = list(
        space = "top",
        labels = list(cex = 1.0)
      ),
      col="black", 
      as.table = TRUE,
      strip = my_strip_style,
      scales=list(
        x=list(cex=0.9, rot = 90), 
        y=list(cex=0.9),
        alternating=3
      ),
      panel = panel_select(add_gridlines),
      par.settings=list(strip.background=list(col="lightgrey"))
    ) 
  
  if (return == "all"){
    line_2_05 <- DTA_SS %>% 
      filter(series_ok == TRUE) %>% 
      contourplot(
        my_ccfr ~ birth_year * age | country, 
        data= . , 
        region = F,
        aspect = "iso",
        as.table = TRUE,
        ylab = "",
        xlab = "", 
        scales = list(NULL),
        at = 2.05, 
        lwd = 2, 
        labels = F
      )
  } else if (return == "contours"){
    line_2_05 <- DTA_SS %>% 
      filter(series_ok == TRUE) %>% 
      contourplot(
        my_ccfr ~ birth_year * age | country, 
        data= . , 
        par.strip.text=list(cex=0.80, fontface="bold"),
        ylab=list(label="Age in years", cex=1.0),
        xlab=list(label="Birth year", cex=1.0),
        region = F,
        cex=1.0,
        aspect = "iso",
        as.table = TRUE,
        labels = F,
        
        at = 2.05, 
        lwd = 2, 
        strip = my_strip_style,
        scales=list(
          x=list(cex=0.9, rot = 90),
          y=list(cex=0.9),
          alternating=3
        ),
        xlim = c(1900, 2000),
        panel = panel_select2(add_gridlines),
        par.settings=list(strip.background=list(col="lightgrey"))
        
      )
    
  }
  
  line_1_80 <- DTA_SS %>% 
    filter(year >= 1950) %>% 
    filter( age <= 50 ) %>% 
    filter(series_ok == TRUE) %>% 
    contourplot(
      my_ccfr ~ birth_year * age | country, 
      data= . , 
      region = F,
      aspect = "iso",
      as.table = TRUE,
      ylab = "",
      xlab = "", 
      scales = list(NULL),
      at = 1.80, 
      lwd = 2,
      lty= "dashed",
      labels = F
    )
  
  line_1_50 <- DTA_SS %>% 
    filter(series_ok == TRUE) %>% 
    contourplot(
      my_ccfr ~ birth_year * age | country, 
      data= . , 
      region = F,
      aspect = "iso",
      as.table = TRUE,
      ylab = "",
      xlab = "", 
      scales = list(NULL),
      at = 1.50, 
      lwd = 1, 
      labels = F
    )
  
  line_1_30 <- DTA_SS %>% 
    filter(series_ok == TRUE) %>% 
    contourplot(
      my_ccfr ~ birth_year * age | country, 
      data= . , 
      region = F,
      aspect = "iso",
      as.table = TRUE,
      ylab = "",
      xlab = "", 
      scales = list(NULL),
      at = 1.30, 
      lwd = 1,
      lty= "dashed",
      labels = F
    )
  
  if (return == "all"){
    output <- shading + line_2_05 + line_1_50 
  } else if (return == "shade") {
    output <- shading
  } else if (return == "contours") {
    output <- line_2_05 + line_1_50  
  }
  
  
  return(output)
  
}

# 
# png("figures/for_ms/overall_poster_gridded.png",
#     res=300, width=40, height=40, units = "cm"
# )
# print(produce_composite_lattice(dta, add_gridlines = F))
# dev.off()
# 
# Split into three pages, each of 16

png("figures/for_ms/overall_split_1.png", 
    res = 300, width = 18, height = 15, units = "cm")
produce_composite_lattice(dta %>% filter(country %in% levels(dta$country)[1:15]), add_gridlines = F)
dev.off()

png("figures/for_ms/overall_split_2.png", res = 300, width = 18, height = 15, units = "cm")
produce_composite_lattice(dta %>% filter(country %in% levels(dta$country)[16:30]), add_gridlines = F)
dev.off()

png("figures/for_ms/overall_split_3.png", res = 300, width = 18, height = 15, units = "cm")
produce_composite_lattice(dta %>% filter(country %in% levels(dta$country)[31:45]), add_gridlines = F)
dev.off()

