

# Data 
country_codes <- read_csv("data/code_definitions.csv")

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

# adjust the paired colour scheme so reds and greens are not next to each other
adjusted_paired <- brewer.pal(12, "Paired")[c(1,2,3,4,9,10,7,8,5,6,11,12)]


divlist  <- c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral")
names(divlist) <- divlist
quallist <- c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")
names(quallist) <- quallist
seqlist  <- c("Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges","OrRd",
              "PuBu","PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd")
names(seqlist) <- seqlist		

brew_pals <-c(divlist,quallist,seqlist)


