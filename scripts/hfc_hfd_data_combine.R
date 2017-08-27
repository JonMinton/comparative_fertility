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
k <- 0

fill_in_asfr <- function(DTA){

  impute_between <- function(START, END, DTA_){
    
    tmp1 <- DTA_ %>% 
      filter(year %in% c(START - 1, END + 1)) %>% 
      select(year, age, asfr)  %>% 
      arrange(year, age)  
    
    # Need to find the max and min age range for 
    # the last and next year available 
    
    age_ranges <- tmp1 %>% 
      group_by(age) %>% 
      summarise(vals_available = !is.na(sum(asfr))) %>% 
      ungroup() %>% 
      filter(vals_available) %>% 
      summarise(min_age_available = min(age), max_age_available = max(age))
    
    tmp2 <- expand.grid(
        age = age_ranges[[1]]:age_ranges[[2]], 
        year = START:END,
        asfr = NA
      ) %>% 
      tbl_df
    
    output <- tmp2 %>% 
      bind_rows(tmp1) %>%
      arrange(year, age) %>% 
      group_by(age) %>% 
      mutate(
        dif = max(year) - min(year),
        x1 = (max(year) - year) / dif,
        x2 = 1 - x1,
        weighted_asfr = x1 * asfr[year == min(year)] + x2 * asfr[year == max(year)]
        ) %>% ungroup() %>% 
      filter(year %in% START:END) %>% 
      select(year, age, asfr = weighted_asfr)

    return(output)
  }
  
  impute_asfrs <- function(d2){
    k <<- k + 1
    max_year <- max(d2$year)
    min_year <- min(d2$year)
    
    num_years <- length(unique(d2$year))
    
    if (num_years == (max_year - min_year + 1)){
      print(paste(k, ": No missing years"))
      # No problems - return input
      return(d2)
    } else {
      print(paste(k, ": At least one missing year"))
      # at least one missing year
      all_possible_years <- min_year:max_year
      all_available_years <- sort(unique(d2$year))
      
      missing_year_indicators_df <- data_frame(
        all_possible_years = all_possible_years, 
        missing = !(all_possible_years %in% all_available_years),
        lag_missing = lag(missing, default = F)
      ) %>% 
        mutate(
          new_missing_block = missing - lag_missing == 1,
          block_num = cumsum(new_missing_block),
          block_num = ifelse(!missing, NA, block_num)
        ) %>% 
        group_by(block_num) %>% 
        filter(!is.na(block_num)) %>% 
        summarise(
          start_missing = min(all_possible_years),
          end_missing = max(all_possible_years)
        )
      

      output <- missing_year_indicators_df %>% 
        mutate(imputed_data = map2(start_missing, end_missing, impute_between, DTA_ = d2)) %>% 
        unnest() %>% 
        select(year, age, asfr) %>% 
        bind_rows(d2) %>% 
        arrange(year, age)

      return(output)
    }
    
    return(NULL) # Error if this happens
  }  

  output <- DTA %>% 
    group_by(code) %>% 
    nest() %>% 
    mutate(data = map(data, impute_asfrs)) %>% 
    unnest()

  return(output)

}

dta_including_imputations <- fill_in_asfr(dta_simplified) %>% 
  filter(!is.na(asfr))

# Fill in younger and older ages 

fill_in_ages <- function(DTA){
  OUT <- data_frame(
    age = 12:55
  ) %>% 
    full_join(DTA) %>% 
    mutate(asfr = ifelse(is.na(asfr), 0, asfr))
  OUT
}

filled_data <- dta_including_imputations %>% 
  group_by(code, year) %>% 
  arrange(age) %>% 
  nest() %>% 
  mutate(data = map(data, fill_in_ages)) %>% 
  unnest



write_csv(filled_data, "data/data_combined_and_standardised.csv")

rm(list = ls())
