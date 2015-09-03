
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



dta <- dta %>% 
  mutate(birth_year = year - age) %>% 
  mutate(my_asfr = total / exposure) %>% 
  group_by(code, year) %>% 
  mutate(my_cpfr = lag(cumsum(total/exposure), 1)) %>% 
  ungroup 

dta <- dta  %>% 
  arrange(code, birth_year, age) %>%  
  group_by(code, birth_year) %>%  
  mutate(my_ccfr = lag(cumsum(total/exposure), 1)) %>% 
  ungroup



dir.create("figures/asfr/", recursive = TRUE)
dir.create("figures/cpfr/", recursive = TRUE)
dir.create("figures/ccfr/", recursive = TRUE)


spool_asfr_figs <- function(x){
  this_country <- x$code[1]
  min_year <- min(x$year)
  max_year <- max(x$year)
  
  title_label <- paste0(
    this_country, " (", min_year, " - ", max_year, ")"
  )
  
  file_label <- paste0(
    "asfr_", this_country, "_(", min_year, "_", max_year, ")" 
  )
  
  png(paste0("figures/asfr/", file_label, ".png"),
      res=300, width=25, height=25, units = "cm"
  )
  
  
  
  p <- x %>% filter( age <= 50 ) %>% 
    contourplot(
      my_asfr ~ year * age , 
      data=. , 
      region=T, 
      par.strip.text=list(cex=1.4, fontface="bold"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      cex=1.4,
      cuts=20,
      col.regions=colorRampPalette(brewer.pal(6, "Purples"))(200),
      main=NULL,
      labels=list(cex=1.2),
      col="black",
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      )
    )
  
  print(p)
  dev.off()
  
  return(NULL)
}


d_ply(dta, .(code), spool_asfr_figs, .progress="text")


spool_cpfr_figs <- function(x){
  this_country <- x$code[1]
  min_year <- min(x$year)
  max_year <- max(x$year)
  
  title_label <- paste0(
    this_country, " (", min_year, " - ", max_year, ")"
  )
  
  file_label <- paste0(
    "cpfr_", this_country, "_(", min_year, "_", max_year, ")" 
  )
  
  png(paste0("figures/cpfr/", file_label, ".png"),
      res=300, width=25, height=25, units = "cm"
  )
  
  
  
  p <- x %>% filter( age <= 50 ) %>% 
    contourplot(
      my_cpfr ~ year * age , 
      data=. , 
      region=T, 
      par.strip.text=list(cex=1.4, fontface="bold"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      cex=1.4,
      cuts=20,
      col.regions=colorRampPalette(brewer.pal(6, "Purples"))(200),
      main=NULL,
      labels=list(cex=1.2),
      col="black",
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      )
    )
  
  print(p)
  dev.off()
  
  return(NULL)
}


d_ply(dta, .(code), spool_cpfr_figs, .progress="text")



spool_ccfr_figs <- function(x){
  this_country <- x$code[1]
  min_year <- min(x$year)
  max_year <- max(x$year)
  
  title_label <- paste0(
    this_country, " (", min_year, " - ", max_year, ")"
  )
  
  file_label <- paste0(
    "cpfr_", this_country, "_(", min_year, "_", max_year, ")" 
  )
  
  png(paste0("figures/ccfr/", file_label, ".png"),
      res=300, width=25, height=25, units = "cm"
  )
  
  
  
  p <- x %>% filter( age <= 50 ) %>% 
    contourplot(
      my_ccfr ~ birth_year * age , 
      data=. , 
      region=T, 
      par.strip.text=list(cex=1.4, fontface="bold"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Birth year", cex=1.4),
      main = x$code[1],
      cex=1.4,
      cuts=20,
      col.regions=colorRampPalette(brewer.pal(6, "Purples"))(200),
      labels=list(cex=1.2),
      col="black",
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      )
    )
  
  print(p)
  dev.off()
  
  return(NULL)
}


d_ply(dta, .(code), spool_ccfr_figs, .progress="text")



# Contour plot for many countries, from 1950 onwards, with 2.1 contour highlighted


shading <- dta %>% 
  filter(year >= 1950) %>% 
  filter( age <= 50 ) %>% 
  filter(code != "DEUTNP") %>% 
  levelplot(
    my_ccfr ~ birth_year * age | code, 
    data=. , 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Birth year", cex=1.4),
    cex=1.4,
    cuts=20,
    col.regions=colorRampPalette(brewer.pal(6, "Purples"))(200),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    ),
    par.settings=list(strip.background=list(col="lightgrey"))
  ) 

replace_line <- dta %>% 
  filter(year >= 1950) %>% 
  filter( age <= 50 ) %>% 
  filter(code != "DEUTNP") %>% 
  
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
  filter(code != "DEUTNP") %>% 
  contourplot(
    my_ccfr ~ birth_year * age | code, 
    data=. , 
    region = F,
    ylab = "",
    xlab = "", 
    scales = list(NULL),
    at = 1.80, 
    lwd = 1,
    lty= "dashed",
    labels = F
  )


png("figures/ccfr/latticeplot.png",
    res=300, width=30, height=30, units = "cm"
)
print(shading + replace_line + near_line)

dev.off()
