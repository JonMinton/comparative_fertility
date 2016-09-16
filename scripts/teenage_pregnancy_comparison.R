
rm(list=ls())

require(readr)

require(plyr)
require(tidyr)
require(stringr)
require(dplyr)

require(ggplot2)
require(lattice)
require(latticeExtra)


require(scales)


dta <- read_csv("data/lexis_square_combined.csv")


# Look at teenage pregnancy trends in different countries 


# Total teenage pregnancies 
dta %>% 
  filter(age < 20) %>% 
  group_by(code, year) %>% 
  summarise(total = sum(total)) %>% 
  ggplot(., aes(x = year, y = total)) + 
  geom_line() + 
  facet_wrap(~code, scales = "free_y")

dta %>% 
  filter(age < 20) %>% 
  group_by(code, year) %>% 
  summarise(
    total = sum(total),
    exposure = sum(exposure)
    ) %>% 
  mutate(tfr = total / exposure) %>% 
  ggplot(., aes(x = year, y = tfr)) + 
  geom_line() + 
  facet_wrap(~code, scales = "free_y")


dta %>% 
  filter(age < 20) %>% 
  group_by(code, year) %>%   
  mutate(fr = total / exposure) %>% 
  summarise(
    tfr = mean(fr)
  ) %>% 
  ggplot(., aes(x = year, y = tfr)) + 
  geom_line() + 
  facet_wrap(~code, scales = "free_y")



# Some countries on same figure

dta %>% 
  filter(age < 20) %>% 
  filter(code %in% c("GBR_SCO", "GBRTENW", "GBR_NIR", "USA", "NOR", "USA", "RUS")) %>% 
  group_by(code, year) %>%   
  mutate(fr = total / exposure) %>% 
  summarise(
    tfr = mean(fr)
  ) %>% 
  ggplot(., aes(x = year, y = tfr, colour = code, group = code)) + 
  geom_line() 


dta %>% 
  filter(age < 25) %>% 
  filter(code %in% c("GBR_SCO", "GBRTENW", "GBR_NIR", "USA", "NOR", "USA", "RUS")) %>% 
  group_by(code, year) %>%   
  mutate(fr = total / exposure) %>% 
  summarise(
    tfr = mean(fr)
  ) %>% 
  ggplot(., aes(x = year, y = tfr, colour = code, group = code)) + 
  geom_line() 



# GGTILE of fert rate at younger ages 

dta %>% 
  filter(age <= 35) %>% 
  filter(code %in% c("GBR_SCO", "GBRTENW", "GBR_NIR", "USA", "NOR", "USA", "RUS")) %>% 
  mutate(fr = total / exposure) %>% 
  ggplot(., aes(x = year, y = age, fill = fr)) + 
  geom_tile() + 
  facet_wrap( ~ code)

  


  

  