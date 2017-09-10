
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

produce_composite_lattice <- function(DTA, add_gridlines = T){
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

  DTA_SS <- DTA %>% 
    filter(year >= 1950) %>% 
    filter(age <= 50)
  
  shading <- DTA_SS %>% 
    levelplot(
      asfr ~ birth_year * age | country, 
      data=. , 
      par.strip.text=list(cex=1.1, fontface="bold"),
      ylab=list(label="Age in years", cex=1.2),
      xlab=list(label="Birth Year", cex=1.2),
      cex=1.4,
      cuts=20,
      aspect = "iso",
      col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
      labels=list(cex=1.2),
      colorkey = list(
        space = "top"
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
  
  line_2_05 <- DTA_SS %>% 
    filter(series_ok == TRUE) %>% 
    contourplot(
      my_ccfr ~ birth_year * age | country, 
      data=. , 
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
  
  line_1_80 <- DTA_SS %>% 
    filter(year >= 1950) %>% 
    filter( age <= 50 ) %>% 
    filter(series_ok == TRUE) %>% 
    contourplot(
      my_ccfr ~ birth_year * age | country, 
      data=. , 
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
      data=. , 
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
      data=. , 
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
  

  output <- shading + line_2_05 + line_1_80 + line_1_50 + line_1_30 
  
  
  return(output)

}


png("figures/for_ms/overall_poster_gridded.png",
    res=300, width=40, height=40, units = "cm"
)
print(produce_composite_lattice(dta, add_gridlines = T))
dev.off()

# How about a version with code instead of country in label? 

dta %>% 
  mutate(country = code) %>% 
  mutate(country = factor(country, levels = rev(ordered_codes), ordered = T)) %>% 
  produce_composite_lattice(add_gridlines = F) -> p1

png("figures/for_ms/overall_paper_coded.png",
    res = 300, width = 25, height = 25, units = "cm"
    )
print(p1)
dev.off()


# Britain
dta %>% 
  filter(code %in% c("GBRTENW", "GBR_SCO", "GBR_NIR", "IRL")) %>% 
  produce_composite_lattice() -> p2

png("figures/for_ms/Britain.png",
    res = 300, width= 20, height = 20, units = "cm")
print(p2)
dev.off()


# USA, Norway, New Zealand, Australia, Canada

dta %>% 
  filter(code %in% c("USA", "NOR", "NZL", "AUS", "CAN")) %>% 
  produce_composite_lattice() -> p3

png("figures/for_ms/anglonor.png",
    res = 300, width = 20, height = 18, units = "cm"
    )
print(p3)
dev.off()


# Germany, Italy and Spain 

dta %>% 
  filter(code %in% c("DEUTE", "DEUTW", "ITA", "ESP")) %>% 
  produce_composite_lattice() -> p4

png("figures/for_ms/lowfert_westsouth.png",
    res = 300, width = 20, height = 15, units = "cm"
    )
print(p4)
dev.off()



# Asian countries 

dta %>% 
  filter(code %in% c("TWN", "JPN", "KOR")) %>% 
  produce_composite_lattice() -> p5

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
  produce_composite_lattice() -> p6

png("figures/for_ms/eastcent.png",
    res = 300, width = 20, height = 20,
    units = "cm"
    )
print(p6)
dev.off()



# Attempt to produce CCFR legend in grid ----------------------------------
# Examples from here:
# https://stat.ethz.ch/R-manual/R-devel/library/grid/doc/grid.pdf

require(grid)
# # First example
# grid.rect(gp = gpar(lty = "dashed"))
# vp1 <- viewport(x = 0, y = 0.5, w = 0.5, h = 0.5,
#                   just = c("left", "bottom"), name = "vp1")
# vp2 <- viewport(x = 0.5, y = 0, w = 0.5, h = 0.5,
#                    just = c("left", "bottom"))
# pushViewport(vp1)
# grid.rect(gp = gpar(col = "grey"))
# grid.text("Some drawing in graphics region 1", y = 0.8)
# upViewport()
# pushViewport(vp2)
# grid.rect(gp = gpar(col = "grey"))
# grid.text("Some drawing in graphics region 2", y = 0.8)
# upViewport()
# downViewport("vp1")
# grid.text("MORE drawing in graphics region 1", y = 0.2)
# popViewport()
# 
# # Second example:
# grid.rect(gp = gpar(lty = "dashed"))
# vp <- viewport(width = 0.5, height = 0.5)
# pushViewport(vp)
# grid.rect(gp = gpar(col = "grey"))
# grid.text("quarter of the page", y = 0.85)
# pushViewport(vp)
# grid.rect()
# grid.text("quarter of the\nprevious viewport")
# popViewport(2)
# 
# dev.off()
# 
# # Third example
# pushViewport(viewport(layout = grid.layout(4, 5)))
# grid.rect (gp = gpar(col = "grey"))
# grid.segments( 
#   c(1:4/5, rep(0, 3)), c(rep(0, 4), 1:3 / 4),
#   c(1:4/5, rep(1, 3)), c(rep(1, 4), 1:3 / 4),
#   gp = gpar(col = "grey")
#   )
# pushViewport(viewport(layout.pos.col = 2:3, layout.pos.row = 3))
# grid.rect(gp = gpar(lwd = 3))
# popViewport(2)

grid.newpage()

pushViewport(viewport(layout = grid.layout(5, 2)))
# grid.rect(gp = gpar(col = "red"))  

pushViewport(viewport(layout.pos.col = 1:2, layout.pos.row = 1))
# grid.rect(gp = gpar(col = "blue", fill = "grey"))
grid.text("CCFR", gp = gpar(fontface = "bold", cex = 1.5 ))
popViewport()

pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2:5))
# grid.rect(gp = gpar(fill = "lightgreen"))
# grid.text("lines go here")
grid.move.to(x = 0.3, y = 0.8)
grid.line.to(x = 0.7, y = 0.8, gp = gpar(lwd = 2))
grid.move.to(x = 0.3, y = 0.6)
grid.line.to(x = 0.7, y = 0.6, gp = gpar(lwd = 2, lty = "dashed"))
grid.move.to(x = 0.3, y = 0.4)
grid.line.to(x = 0.7, y = 0.4)
grid.move.to(x = 0.3, y = 0.2)
grid.line.to(x = 0.7, y = 0.2, gp = gpar(lty = "dashed"))
popViewport()

pushViewport(viewport(layout.pos.col= 2, layout.pos.row = 2:5))
# grid.rect(gp = gpar(fill = "lightblue"))
grid.text("2.05", x = 0.05, y = 0.8, just = "left")
grid.text("1.80", x = 0.05, y = 0.6, just = "left")
grid.text("1.50", x = 0.05, y = 0.4, just = "left")
grid.text("1.30", x = 0.05, y = 0.2, just = "left")
#grid.text("Labels go here")
popViewport(2)



# First division: 
# Top 
# The challenges 






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


