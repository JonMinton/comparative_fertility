


brewer.pal(12, "Paired")

produce_composite_lattice <- function(DTA, add_gridlines = TRUE, add_periodgridlines = FALSE,
                                      colscheme = colorRampPalette(adjusted_paired)(200),
                                      return = "all",
                                      contour_vals = c(1.30, 1.50, 1.80, 2.05),
                                      cohort = NULL,
                                      shading_cuts = 20,
                                      shading_limits = NULL
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
  
  panel_select <- function(add_gridlines, add_periodgridlines){
    if(!add_gridlines){
      return(
        function(...){
          panel.levelplot(...)
          panel.abline(v = cohort, col = "blue", size = 2, lty = "dashed")
        }
      )
    } else {
      if (!add_periodgridlines){
        return(
          function(...){
            panel.levelplot(...)
            panel.abline(h = seq(15, 45, by = 5), lty = "dashed", col = "grey")
            panel.abline(v = seq(1900, 2000, by = 5), lty = "dashed", col = "grey")
            panel.abline(v = cohort, col = "blue", size = 2, lty = "dashed")
          }
        )
      } else {
        return(
          function(...){
            panel.levelplot(...)
            panel.abline(h = seq(15, 45, by = 5), lty = "dashed", col = "grey")
            A <- seq(1800, 2200, by = 5)
            for (i in seq_along(A)){
              panel.abline(a = A[i], b = -1, lty = "dashed", col = "grey")
            }
            panel.abline(v = seq(1900, 2000, by = 5), lty = "dashed", col = "grey")
            panel.abline(v = cohort, col = "blue", size = 2, lty = "dashed")
          }
        )
      }
    }
    NULL
  }
  
  
  panel_select2 <- function(add_gridlines, add_periodgridlines){
    if(!add_gridlines){
      return(
        function(...){
          panel.contourplot(...)
          panel.abline(v = cohort, col = "blue", size = 2, lty = "dashed")
        }
      )
    } else {
      if (!add_periodgridlines){
        return(
          function(...){
            panel.levelplot(...)
            panel.abline(h = seq(15, 45, by = 5), lty = "dashed", col = "grey")
            panel.abline(v = seq(1900, 2000, by = 5), lty = "dashed", col = "grey")
            panel.abline(v = cohort, col = "blue", size = 2, lty = "dashed")
          }
        )
      } else {
        return(
          function(...){
            panel.levelplot(...)
            panel.abline(h = seq(15, 45, by = 5), lty = "dashed", col = "grey")
            A <- seq(1800, 2200, by = 5)
            for (i in seq_along(A)){
              panel.abline(a = A[i], b = -1, lty = "dashed", col = "grey")
            }
            panel.abline(v = seq(1900, 2000, by = 5), lty = "dashed", col = "grey")
            panel.abline(v = cohort, col = "blue", size = 2, lty = "dashed")
          }
        )
      }
    }
    NULL
  }
  
  DTA_SS <- DTA %>% 
    filter(year >= 1950) %>% 
    filter(age <= 50)
  
  if(is.null(shading_limits)){
    shading <- DTA_SS %>% 
      levelplot(
        asfr ~ birth_year * age | country, 
        data= . , 
        par.strip.text=list(cex=1.1, fontface="bold"),
        ylab=list(label="Age in years", cex=1.2),
        xlab=list(label="Birth Year", cex=1.2),
        cex=1.4,
        cuts=shading_cuts,
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
        panel = panel_select(add_gridlines, add_periodgridlines),
        par.settings=list(strip.background=list(col="lightgrey"))
      ) 
  } else {
    shading <- DTA_SS %>% 
      levelplot(
        asfr ~ birth_year * age | country, 
        data= . , 
        par.strip.text=list(cex=1.1, fontface="bold"),
        ylab=list(label="Age in years", cex=1.2),
        xlab=list(label="Birth Year", cex=1.2),
        cex=1.4,
        at = seq(from = shading_limits[1], to = shading_limits[2], length.out = shading_cuts),
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
        panel = panel_select(add_gridlines, add_periodgridlines),
        par.settings=list(strip.background=list(col="lightgrey"))
      ) 
    
    
  }
  
  if (return == "all"){
    line_4 <- DTA_SS %>% 
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
        at = contour_vals[4], 
        lwd = 2, 
        labels = F
      )
  } else if (return == "contours"){
    line_4 <- DTA_SS %>% 
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
        
        at = contour_vals[4], 
        lwd = 2, 
        strip = my_strip_style,
        scales=list(
          x=list(cex=1.2, rot = 90),
          y=list(cex=1.2),
          alternating=3
        ),
        xlim = c(1900, 2000),
        panel = panel_select2(add_gridlines, add_periodgridlines),
        par.settings=list(strip.background=list(col="lightgrey"))
        
      )
    
  }
  
  line_3 <- DTA_SS %>% 
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
      at = contour_vals[3], 
      lwd = 2,
      lty= "dashed",
      labels = F
    )
  
  line_2 <- DTA_SS %>% 
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
      at = contour_vals[2], 
      lwd = 1, 
      labels = F
    )
  
  line_1 <- DTA_SS %>% 
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
      at = contour_vals[1], 
      lwd = 1,
      lty= "dashed",
      labels = F
    )
  
  if (return == "all"){
    output <- shading + line_4 + line_3 + line_2 + line_1 
  } else if (return == "shade") {
    output <- shading
  } else if (return == "contours") {
    output <- line_4 + line_3 + line_2 + line_1 
  }
  
  
  return(output)
  
}



