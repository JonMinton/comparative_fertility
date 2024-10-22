
rm(list=ls())


require(pacman)

pacman::p_load(
  readr, car,
  tidyr,  stringr, dplyr,
  purrr, r2stl, 
  ggplot2, lattice, latticeExtra,
  RColorBrewer,
  viridis
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


# Create illustrative figure

dta_simplified %>% 
  filter(code == "DEUTW") %>% 
  mutate(cohort = year - age) %>% 
  filter(cohort %in% c(1935, 1938)) %>%
  mutate(cohort = factor(cohort)) %>%
  group_by(cohort) %>% 
  arrange(age) %>% 
  mutate(sum_asfr = cumsum(asfr)) %>% 
  ungroup() -> dta


find_age_of_hurdle <- function(dta, crate){

  xvals <- pull(dta, age)
  yvals <- pull(dta, sum_asfr)
  curvefun <- splinefun(xvals, yvals)
  fn <- function(x, CRATE = crate){
    (curvefun(x) - CRATE) ^ 2
  }
  out <- optimize(fn, c(10, 100))$`minimum`
}


crossing(
  cohort = c(1935, 1938), crate = c(1.3, 1.5,1.8,2.05)
  ) %>% 
  left_join(
    dta %>% 
      mutate(cohort = as.double(as.character(cohort))) %>% 
      group_by(cohort) %>% 
      nest()
  ) %>% 
  mutate(crossing_age = 
    map2_dbl(
      data, crate,
      find_age_of_hurdle
    )
  ) %>% 
  select(cohort, cumulative_fertility = crate, age_cf_achieved = crossing_age) -> 
  ages_fertility_achieved

ages_fertility_achieved <- ages_fertility_achieved %>%
  mutate(nearest_age = round(age_cf_achieved, 0)) %>% 
  left_join(
    dta %>% mutate(cohort = as.numeric(as.character(cohort))),
    by = c("cohort"= "cohort", "nearest_age" = "age")
  ) %>% 
  select(cohort, cumulative_fertility, age_cf_achieved, asfr)


            

dta %>% 
  ggplot(aes(x = sum_asfr, y = age, group = cohort, colour = cohort)) +
  geom_line() +
  geom_vline(xintercept = 1.3, linetype = "dashed") + 
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 1.8, size = 1.2, linetype = "dashed") + 
  geom_vline(xintercept = 2.05, size = 1.2) +
  scale_color_manual(values = c("#a6cee3", "#b2df8a")) +
  theme_minimal() + 
  labs(x = "Cumulative fertility of cohorts (live births / woman)",
       y = "Age in years") +
  geom_hline(
    aes(yintercept = age_cf_achieved, 
        size = factor(cumulative_fertility),
        linetype = factor(cumulative_fertility),
        colour = factor(cohort)),
    data = ages_fertility_achieved, 
    inherit.aes = F
  ) +
  scale_size_manual(values = c(1, 1, 1.2, 1.2)) + 
  scale_linetype_manual(values = c("dashed", "solid", "dashed", "solid")) + 
  scale_color_manual(values = c("#a6cee3", "#b2df8a")) +
  coord_cartesian(xlim = c(0, 2.5), ylim = c(12, 50))


dta %>% 
  ggplot(aes(x = sum_asfr, y = age, group = cohort, colour = cohort)) +
  geom_line() +
  geom_vline(xintercept = 1.3, linetype = "dashed") + 
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 1.8, size = 1.2, linetype = "dashed") + 
  geom_vline(xintercept = 2.05, size = 1.2) +
  scale_color_manual(values = c("#a6cee3", "#b2df8a"), guide = F) +
  theme_minimal() + 
  labs(x = "Cumulative fertility of cohorts (live births / woman)",
       y = "Age in years") +
  geom_segment(
    aes(y = age_cf_achieved, 
        yend = age_cf_achieved,
        x = cumulative_fertility,
        xend = asfr + 0.01, 
        size = factor(cumulative_fertility),
        linetype = factor(cumulative_fertility),
        colour = factor(cohort)),
    arrow = arrow(length = unit(0.01, "npc")),
    data = ages_fertility_achieved, 
    inherit.aes = F
  ) +
  scale_size_manual(values = c(1, 1, 1.2, 1.2), guide = FALSE) + 
  scale_linetype_manual(values = c("dashed", "solid", "dashed", "solid"), guide = FALSE) + 
  scale_color_manual(values = c("#a6cee3", "#b2df8a"), guide = F) +
  coord_cartesian(xlim = c(0, 2.5), ylim = c(12, 50)) + 
  annotate(
    "text", x = 1.32, y = 20,
    label = "1.3 babies/woman", fontface = "italic",
    angle = 90,
    size = 2
  ) + 
  annotate(
    "text", x = 1.52, y = 20,
    label = "1.5 babies/woman", fontface = "italic",
    angle = 90,
    size = 2
  ) +
  annotate(
    "text", x = 1.82, y = 20,
    label = "1.8 babies/woman", fontface = "italic",
    angle = 90,
    size = 2
  ) + 
  annotate(
  "text", x = 2.07, y = 20,
  label = "2.05 babies/woman", fontface = "italic",
  angle = 90,
  size = 2
  ) + 
  annotate(
    "text", x = 0.5, y = 23,
    label = "Cumulative fertility, 1935 cohort", fontface = "bold",
    angle = 15, colour = "#a6cee3"
  ) + 
  annotate(
    "text", x = 0.5, y = 21.5,
    label = "Cumulative fertility, 1938 cohort", fontface = "bold",
    angle = 15, colour = "#b2df8a"
  ) +
  geom_polygon(
    aes(x = asfr, y = age),
    data = dta %>% filter(cohort == "1935"),
    fill = "#a6cee3", alpha = 0.5
  ) + 
  geom_polygon(
    aes(x = asfr, y = age), 
    data = dta %>% filter(cohort == "1938"),
    fill = "#b2df8a", alpha = 0.5
  ) + 
  annotate(
    "text", x = 0.065, y = 25,
    label = "Fertility Schedules\n(1935 and 1938)", fontface = "bold",
    angle = 270
  ) + 
  annotate(
    "text", x = 0.60, y = 37.5,
    label = "Placement of replacement fertility contour for 1938 cohort",
    size = 1.8
  ) +
  annotate(
    "text", x = 0.62, y = 35,
    label = "Placement of replacement fertility contour for 1935 cohort",
    size = 1.8
  ) +
  annotate(
    "text", x = 0.60, y = 31.5,
    label = "Placement of 1.8 babies/woman contours",
    size = 1.8
  ) +
  annotate(
    "text", x = 0.60, y = 28.50,
    label = "Placement of 1.5 babies/woman contours",
    size = 1.8
  ) +
  annotate(
    "text", x = 0.60, y = 26.75,
    label = "Placement of 1.3 babies/woman contours",
    size = 1.8
  ) 

ggsave("figures/annotation_fig.svg")


# Produce figure for West Germany 
# contours only 
# colours only (paired colourscheme)



# ages_fertility_achieved


#####
# dta_simplified %>%
#   ggplot(., aes(x = year, y = age, fill = asfr)) +
#   facet_wrap(~code) +
#   geom_tile()


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
  filter(age <= 49) %>% # Added as per Sebastian's suggestion
  group_by(code) %>% 
  mutate(last_ccfr = max(my_ccfr, na.rm= T))  %>% 
  ungroup()  %>% 
  select(code, year, last_ccfr)  %>% 
  distinct()  %>% 
  mutate(fert_rank = dense_rank(last_ccfr))  %>% 
  arrange(fert_rank)  %T>% write_csv("ccfr_in_2007.csv") %>%  
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
paired <- brewer.pal(12, "Paired")

col_df <- tibble(
  type = rep(c("unadjusted", "adjusted"), each = 12),
  col = c(adjusted_paired, paired),
  x = rep(1:12, 2)
)

svg("figures/paired_pal.svg")
plot(1:12, rep(2, 12), pch = 15, cex = 5, col = col_df$col[1:12], ylim = c(0, 2.5), axes = F,
     xlab = "", ylab = "")
points(1:12, rep(1, 12), pch = 15, cex = 5, col = col_df$col[13:24])
dev.off()


brewer.pal(12, "Paired")

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
    panel.text(x = 0.5, y = 0.5,
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
      par.strip.text=list(cex=1.1, fontface="bold"),
      ylab=list(label="Age in years", cex=1.2),
      xlab=list(label="Birth Year", cex=1.2),
      cex=1.4,
      cuts=20,
      aspect = "iso",
      col.regions=colscheme,
      labels=list(cex=1.2),
      colorkey = list(
        space = "top",
        labels = list(cex = 1.2)
      ),
      col="black", 
      as.table = TRUE,
      strip = my_strip_style,
      scales=list(
        x=list(cex=1.2, rot = 90), 
        y=list(cex=1.2),
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
        par.strip.text=list(cex=1.1, fontface="bold"),
        ylab=list(label="Age in years", cex=1.2),
        xlab=list(label="Birth Year", cex=1.2),
        region = F,
        cex=1.4,
        aspect = "iso",
        as.table = TRUE,
        labels = F,

       at = 2.05, 
       lwd = 2, 
       strip = my_strip_style,
       scales=list(
         x=list(cex=1.2, rot = 90),
         y=list(cex=1.2),
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
    output <- shading + line_2_05 + line_1_80 + line_1_50 + line_1_30 
  } else if (return == "shade") {
    output <- shading
  } else if (return == "contours") {
    output <- line_2_05 + line_1_80 + line_1_50 + line_1_30 
  }
  
  
  return(output)

}


png("figures/for_ms/overall_poster_gridded.png",
    res=300, width=40, height=40, units = "cm"
)
print(produce_composite_lattice(dta, add_gridlines = F, colscheme = viridis_pal(direction = -1)(200)))
dev.off()



# How about a version with code instead of country in label? 

dta %>% 
  mutate(country = code) %>% 
  mutate(country = factor(country, levels = rev(ordered_codes), ordered = T)) %>% 
  produce_composite_lattice(add_gridlines = F,colscheme = viridis_pal(direction = -1)(200)) -> p1

png("figures/for_ms/overall_paper_coded.png",
    res = 300, width = 26, height = 26, units = "cm"
    )
print(p1)
dev.off()


# Illustration for fig 1: West Germany with Shade only
# Illustration for fig 1: West Germany with Contour lines only 
# Illustration for fig 1: East and West Germany combined

dta %>% 
  filter(code %in% c("DEUTW")) %>% 
  produce_composite_lattice(add_gridlines = F, return = "contours",
                            colscheme = viridis_pal(direction=-1)(200)) -> f1b


dta %>% 
  filter(code %in% c("DEUTW")) %>% 
  produce_composite_lattice(add_gridlines = F, return = "shade",
                            colscheme = viridis_pal(direction=-1)(200)) -> f1c

dta %>% 
  filter(code %in% c("DEUTW")) %>% 
  produce_composite_lattice(add_gridlines = F, return = "all",
                            colscheme = viridis_pal(direction=-1 )(200)) -> f1d


dta %>% 
  filter(code %in% c("DEUTW", "DEUTE")) %>% 
  produce_composite_lattice(add_gridlines = F, return = "all",
                            colscheme = viridis_pal(direction=-1)(200)) -> f1e

svg("figures/fig1_b.svg"); print(f1b); dev.off()
svg("figures/fig1_c.svg"); print(f1c); dev.off()
svg("figures/fig1_d.svg"); print(f1d); dev.off()
svg("figures/fig1_e.svg"); print(f1e); dev.off()


# USA Norway comparison

dta %>% 
  filter(code %in% c("USA", "NOR")) %>% 
  produce_composite_lattice(add_gridlines = F,
                            colscheme = viridis_pal(direction = -1)(200)
                            ) -> fig_usanor

svg("figures/usanor.svg"); print(fig_usanor); dev.off()


# Britain
dta %>% 
  filter(code %in% c("GBRTENW", "GBR_SCO", "GBR_NIR", "IRL")) %>% 
  produce_composite_lattice(add_gridlines = F,                             colscheme = viridis_pal(direction = -1)(200)) -> p2

png("figures/for_ms/Britain.png",
    res = 300, width= 20, height = 20, units = "cm")
print(p2)
dev.off()


# USA, Norway, New Zealand, Australia, Canada

dta %>% 
  filter(code %in% c("USA", "NOR", "NZL", "AUS", "CAN")) %>% 
  produce_composite_lattice(add_gridlines = F,                             colscheme = viridis_pal(direction = -1)(200)) -> p3

png("figures/for_ms/anglonor.png",
    res = 300, width = 20, height = 18, units = "cm"
    )
print(p3)
dev.off()


# Germany, Italy and Spain 

dta %>% 
  filter(code %in% c("DEUTE", "DEUTW", "ITA", "ESP")) %>% 
  produce_composite_lattice(add_gridlines = F,                             colscheme = viridis_pal(direction = -1)(200)) -> p4

png("figures/for_ms/lowfert_westsouth.png",
    res = 300, width = 20, height = 15, units = "cm"
    )
print(p4)
dev.off()



# Asian countries 

dta %>% 
  filter(code %in% c("TWN", "JPN", "KOR")) %>% 
  produce_composite_lattice(add_gridlines = F,                            colscheme = viridis_pal(direction = -1)(200)) -> p5

png("figures/for_ms/asian.png",
    res = 300, width = 20, height = 15, 
    units = "cm"
    )
print(p5)
dev.off()

# 

# East/Central European 

dta %>% 
  filter(code %in% c("ALB", "ROU", "POL", "HUN", "RUS", "MDA")) %>% 
  produce_composite_lattice(add_gridlines = F,                            colscheme = viridis_pal(direction = -1)(200)) -> p6

png("figures/for_ms/eastcent.png",
    res = 300, width = 20, height = 20,
    units = "cm"
    )
print(p6)
dev.off()








# Older material ----------------------------------------------------------



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


