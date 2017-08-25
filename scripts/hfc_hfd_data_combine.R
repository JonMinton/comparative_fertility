# Harvest, tidy, and combine HFD and HFC data 



# Load data ---------------------------------------------------------------


dta_hfc <- read_csv("data/hfc/HFC_ASFRstand_TOT/HFC_ASFRstand_TOT.txt")

dta_hfd <- read_csv("data/lexis_square_combined.csv")

country_codes <- read_csv("data/hfc/code_definitions.csv")
# work on tidying/filtering hfc -------------------------------------------


# # Simple illustration of asfr and age relationship
# 
# dta_hfd  %>% 
#   filter(code %in% c("GBRTENW", "GBR_SCO"))  %>% 
#   filter(year %in% c(1960, 1980, 2000))  %>% 
#   mutate(
#     year = factor(year), 
#     country = recode(code, "'GBRTENW' = 'England & Wales'; 'GBR_SCO' = 'Scotland'")
#     ) %>% 
#   ggplot(.) + 
#   geom_line(
#     aes(x = age, y = asfr, group = year, colour = year, linetype = year)) + 
#   facet_wrap(~country) + 
#   labs(x = "Age in years", y = "Age-specific fertility rate") + 
#   theme_minimal()
# 
# ggsave(filename = "figures/asfr_example.png", height = 20, width = 30, units = "cm", dpi = 300)
# # So, the rule is 
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
  filter(Collection %in% c("STAT", "ODE", "RE")) %>% 
  select(code = Code, year = Year1, age = Age, asfr = ASFR, collection = Collection, refcode = RefCode) %>% 
  mutate(asfr = as.numeric(asfr)) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(age = as.numeric(age))


# Some analyis of above before conversion to numeric

# For each code, report each combination of collection and refcode, along with year
  
# dta_hfc_simple %>% 
#   mutate(colrefcode = paste(refcode, collection, sep = "__")) %>% 
#   group_by(colrefcode) %>% 
#   summarise(
#     min_year = min(year),
#     max_year = max(year),
#     year_cont = length(unique(year)) == max_year - min_year + 1 # find non-contiguous datasets
#   ) %>% View

# Workflow should be: 

# For each country, year, and age: 
# i ) find if any Collection == Stat
#  If number of collections > 1, get average (na.rm = T)
# ii ) if any collection == ODE
#  Repeat
# iii) if any collection == RE
# Repeat 

dta_hfc_simple %>% 
  group_by(code, year, age, collection) %>% 
  filter(!is.na(year), !is.na(age)) %>% 
  summarise(
    n_vals = length(.),
    mean_asfr = mean(asfr, na.rm = T),
    sd_asfr = sd(asfr, na.rm = T) # Returns NA which is correct when vector is of length 1
    ) %T>% print(sample_n(10)) %>%
  ungroup() %>% 
  select(code, year, age, collection, mean_asfr) %T>% print(sample_n(10)) %>% 
  spread(collection, mean_asfr) %>% 
  mutate(
    final_asfr = case_when(
      !is.na(STAT) ~ STAT,
      !is.na(ODE) ~ ODE,
      !is.na(RE) ~ RE
      
    )
  ) -> dta_hfc_simple_best

dta_hfc_simple_best %>% sample_n(10)

dta_hfc_simple <- dta_hfc_simple_best %>% 
  select(code, year, age, asfr = final_asfr)

rm(dta_hfc_simple_best)

dta_simplified <- bind_rows(
  dta_hfc_simple %>% mutate(database = "hfc"),
  dta_hfd %>% 
    select(code, year, age, asfr) %>% 
    mutate(database = "hfd") 
) %T>% print(sample_n(10)) %>% 
  spread(database, asfr) %T>% print(sample_n(10)) %>% 
  mutate(asfr = case_when(
    !is.na(hfd) ~ hfd,
    !is.na(hfc) ~ hfc
    )
  ) %T>% print(sample_n(10)) %>% 
  select(code, year, age, asfr) 
         
dta_simplified %>% sample_n(10)

# Values for BIH (Bosnia and Hertzegovina) are missing 
# for the years 1991 to 1995. However, the rest of the series 
# seems relatively complete. 
# Because of this, I will produce a linear imputation of the ASFRs 
# based on the 1990 to 1996 years, weighted towards the nearest year

# Produce a function for automating the filling in of ASFRs for multiple cases

fill_in_asfr <- function(DTA){

  # find number of contiguous missing pieces 
  find_breaks <- function(x){
    x_lag <- x[-1]
    x_2   <- x[-length(x)]
    
    breaks <- x[!which(x_2 == x_lag - 1)]
    browser()
    
    # THIS IS WHAT NEEDS RE-DOING
    output <- data_frame(
      block = 1:(length(breaks + 1))
    ) %>% 
      mutate(
        start_missing = case_when(
          block == 1 ~ x[1], 
          TRUE ~ x[lag(breaks)]
        ),
        end_missing = x[breaks]
      )
    
    return(output)
  }
  
  impute_between <- function(START, END){
    
    tmp1 <- DTA %>% 
      filter(year %in% c(START - 1, END + 1)) %>% 
      select(year, age, asfr)  %>% 
      arrange(age)  
    
    
    tmp2 <- expand.grid(
        age = 14:50, 
        year = START:END
      ) %>% 
      tbl_df
    
    output <- tmp2 %>% 
      left_join(tmp1) %>%
      group_by(age) %>% 
      mutate(
        dif = max(year) - min(year),
        x1 = (max(year) - year) / dif,
        x2 = 1 - x1,
        weighted_asfr = x1 * asfr[year == min(year)] + x2 * asfr[year == max(year)]
        ) %>% 
      select(year, age, asfr = weighted_asfr)

    return(output)
  }
  
  impute_asfrs <- function(d2){
    max_year <- max(d2$year)
    min_year <- min(d2$year)
    
    num_years <- length(unique(d2$year))
    
    if (num_years == (max_year - min_year + 1)){
      # No problems - return input
      return(d2)
    } else {
      # at least one missing year
      all_possible_years <- min_year:max_year
      all_available_years <- sort(unique(d2$year))
      
      missing_years <- setdiff(
        all_possible_years, 
        all_available_years
        )
      
      missing_year_indicators_df <- find_breaks(missing_years) 
      
      output <- missing_year_indicators_df %>% 
        mutate(imputed_data = map2(start_missing, end_missing, impute_between)) %>% 
        unnest() %>% 
        select(year, age, asfr) %>% 
        bind_row(d2) 
      
      return(output)
    }
    
    return(NULL) # Error if this happens
  }  

  
  output <- DTA %>% 
    group_by(code) %>% 
    nest() %>% 
    mutate(all_data = map(data, impute_asfrs)) %>% 
    unnest()
  
  return(output)

}


fill_in_asfr(dta_simplified)

# 
# 
# tmp <- dta_simplified %>% 
#   filter(code == "BIH") %>% 
#   filter(year %in% c(1990, 1996)) %>% 
#   select(year, age, asfr)  %>% 
#   arrange(age)  %>% 
#   spread(year, asfr)
# 
# tmp2 <- expand.grid(age = 14:50, year = 1991:1995) %>% tbl_df
# 
# tmp3 <- tmp2 %>% left_join(tmp) 
# 
# tmp4 <- tmp3 %>% mutate(
#   dif = 1996 - 1990, 
#   x1 = (1996 - year) / dif, 
#   x2 = 1 - x1, 
#   weighted_asfr = x1 * `1990`  + x2 * `1996`) %>% 
#   mutate(code = "BIH") %>% 
#   select(code, year, age, asfr = weighted_asfr)
# 
# dta_simplified <- bind_rows(dta_simplified, tmp4)
# 
# rm(tmp, tmp2, tmp3, tmp4)
# 
# # Finished imputation of BIH missing years
# 
# # Now to do something similar for Belarus - though just one year (2013) missing!
# tmp <- dta_simplified %>% 
#   filter(code == "BLR") %>% 
#   filter(year %in% c(2012, 2014)) %>% 
#   select(year, age, asfr)  %>% 
#   arrange(age)  %>% 
#   spread(year, asfr)
# 
# tmp2 <- expand.grid(age = 14:50, year = 2013) %>% tbl_df
# 
# tmp3 <- tmp2 %>% left_join(tmp) 
# 
# tmp4 <- tmp3 %>% mutate(
#   dif = 2014 - 2012, 
#   x1 = (2014 - year) / dif, 
#   x2 = 1 - x1, 
#   weighted_asfr = x1 * `2012`  + x2 * `2014`) %>% 
#   mutate(code = "BLR") %>% 
#   select(code, year, age, asfr = weighted_asfr)
# 
# dta_simplified <- dta_simplified %>% 
#   bind_rows(tmp4) 
# rm(tmp, tmp2, tmp3, tmp4)

dta_simplified %>% 
  group_by(code) %>% 
  summarise(
    min_year = min(year), max_year = max(year), 
    min_age = min(age), max_age = max(age),
    n_missing = (max_year - min_year + 1) - length(unique(year)),
    n = n()
    )


write_csv(dta_simplified, "data/data_combined_and_standardised.csv")

rm(list = ls())
