surfaceplot_function <- function(input, output, pal_set) {

  
  turn_asfr_to_matrix <- function(DTA, CODE){
    DTA %>%
      filter(code == CODE) %>%
      select(birth_year, age, asfr) %>%
      spread(age, asfr) ->  tmp
    birth_years <- pull(tmp, birth_year)
    tmp <- tmp[,-1]
    ages <- colnames(tmp)
    asfr <- as.matrix(tmp)
    return(list(asfr = asfr, birth_years = birth_years, ages = ages))
  }
  
  turn_asfr_period_to_matrix <- function(DTA, CODE){
    DTA %>%
      filter(code == CODE) %>%
      select(year, age, asfr) %>%
      spread(age, asfr) ->  tmp
    years <- pull(tmp, year)
    tmp <- tmp[,-1]
    ages <- colnames(tmp)
    asfr <- as.matrix(tmp)
    return(list(asfr = asfr, years = years, ages = ages))
  }
  
  turn_ccfr_to_matrix <- function(DTA, CODE){
    DTA %>%
      filter(code == CODE) %>%
      filter(series_ok == TRUE) %>%
      select(birth_year, age, ccfr = my_ccfr) %>%
      spread(age, ccfr) ->  tmp
    birth_years <- pull(tmp, birth_year)
    tmp <- tmp[,-1]
    ages <- colnames(tmp)
    ccfr <- as.matrix(tmp)
    return(list(ccfr = ccfr, birth_years = birth_years, ages = ages))
  }
  
  turn_ccfr_diffs_to_matrix <- function(DTA, CODE, CODE2){
    DTA %>%
      filter(code %in% c(CODE, CODE2)) %>%
      filter(series_ok == TRUE) %>%
      select(birth_year, age, ccfr = my_ccfr) %>%
      spread(code, ccfr) %>% 
      mutate(diff = pull(., 4) - pull(., 3)) -> tmp
    
    tmp %>%
      select(birth_year, age, diff) %>%
      spread(age, diff) ->  tmp
    
    birth_years <- pull(tmp, birth_year)
    
    tmp <- tmp[,-1]
    ages <- colnames(tmp)
    diff_ccfr <- as.matrix(tmp)
    return(list(diff_ccfr = diff_ccfr, birth_years = birth_years, ages = ages))
  }
  
  
  
  dta_list <- if(input$select_surface_type == "asfr_year"){
    turn_asfr_period_to_matrix(
      DTA = dta,
      CODE = input$country_for_surface)
  } else if(input$select_surface_type == "asfr_cohort"){
    turn_asfr_to_matrix(
      DTA = dta,
      CODE = input$country_for_surface)
  } else if (input$select_surface_type == "ccfr"){
    turn_ccfr_to_matrix(
      DTA = dta,
      CODE = input$country_for_surface)
  } 
  
  if (input$show_second_surface %in% c('two', 'diff')){
    dta2_list <- if(input$select_surface_type == 'asfr_year'){
      turn_asfr_period_to_matrix(
        DTA = dta,
        CODE = input$second_country_for_surface
      )
    } else if (input$select_surface_type == 'asfr_cohort'){
      turn_asfr_to_matrix(
        DTA = dta,
        CODE = input$second_country_for_surface
      )
    } else if (input$select_surface_type == 'ccfr'){
      turn_ccfr_to_matrix(
        DTA = dta,
        CODE = input$second_country_for_surface
      )
    } 
    
    z_lims <- if (input$select_surface_type == 'ccfr'){
      c(
        min(dta_list$ccfr, dta2_list$ccfr),
        max(dta_list$ccfr, dta2_list$ccfr)
      )
    } else {
      c(
        min(dta_list$asfr, dta2_list$asfr),
        max(dta_list$asfr, dta2_list$asfr)
      )
    }
  }      
  pal <- pal_set()
  
  n_years <- if (input$select_surface_type == 'asfr_year'){
    length(dta_list$years)
  } else {
    length(dta_list$birth_years)
  }
  
  n_ages <- length(dta_list$ages)
  
  
  #    All versions should have the same xy labelling - so should be added as common layout option
  
  
  if (input$show_second_surface == 'two'){
    
    y_vals <- if(input$select_surface_type == 'asfr_year'){
      dta_list$years
    } else {
      dta_list$birth_years
    }
    
    z_vals <- if(input$select_surface_type == 'ccfr'){
      dta_list$ccfr
    } else {
      dta_list$asfr
    }
    
    y_vals2 <- if(input$select_surface_type == 'asfr_year'){
      dta2_list$years
    } else {
      dta2_list$birth_years
    }
    
    z_vals2 <- if(input$select_surface_type == 'ccfr'){
      dta2_list$ccfr
    } else {
      dta2_list$asfr
    }
    
    plot_ly(
      source = "fert_surface"
    ) %>%
      add_surface(
        x = ~dta_list$ages,
        y = ~y_vals,
        z = ~z_vals,
        opacity = input$alpha_first_country,
        colorscale = list(
          c(0, 1),
          c('rgb(255,0,0)', 'rgb(255,0,0)')
        ),
        colorbar = list(
          title = input$country_for_surface
        )
      )  %>%
      add_surface(
        x = ~dta2_list$ages,
        y = ~y_vals2,
        z = ~z_vals2,
        opacity = input$alpha_second_country,
        colorscale = list(
          
          c(0, 1),
          c('rgb(0,0,255)', 'rgb(0,0,255)')
        ),
        colorbar = list(
          title = input$second_country_for_surface
        )
        
      ) -> output
    
  } else if (input$show_second_surface == 'one'){
    
    if(input$pal_type == "cubeyf"){
      pal <- colorRampPalette(cubeyf_palette)(200)
    } else {
      pal <- pal_set()
    }
    
    custom_text <- if(input$select_surface_type == "asfr_cohort"){
      paste0(
        "Birth year: ", rep(dta_list$birth_years, times = length(dta_list$ages)), "\n",
        "Age: ", rep(dta_list$ages, each = length(dta_list$birth_years)), "\n",
        "Fertility: ", round(dta_list$asfr * 1000, 1), " babies / 1000 women"
      ) %>%
        matrix(length(dta_list$birth_years), length(dta_list$ages))
    } else if (input$select_surface_type == "asfr_year"){
      paste0(
        "Year: ", rep(dta_list$years, times = length(dta_list$ages)), "\n",
        "Age: ", rep(dta_list$ages, each = length(dta_list$years)), "\n",
        "Fertility: ", round(dta_list$asfr * 1000, 1), " babies / 1000 women"
      ) %>%
        matrix(length(dta_list$years), length(dta_list$ages))
    } else if (input$select_surface_type == "ccfr"){
      paste0(
        "Birth year: ", rep(dta_list$birth_years, times = length(dta_list$ages)), ".\t",
        "Cumulative Fertility: ", round(dta_list$ccfr, 2), " babies by age ",
        rep(dta_list$ages, each = length(dta_list$birth_years)), "\n"
      ) %>%
        matrix(length(dta_list$birth_years), length(dta_list$ages))
    }
    # from https://stackoverflow.com/questions/44488845/plotly-surface-text-hoverinfo-not-working?rq=1
    
    y_vals <- if(input$select_surface_type == 'asfr_year'){
      dta_list$years
    } else {
      dta_list$birth_years
    }
    
    z_vals <- if(input$select_surface_type == 'ccfr'){
      dta_list$ccfr
    } else {
      dta_list$asfr
    }
    
    
    
    plot_ly(
      source = "fert_surface"
    ) %>%
      add_surface(
        x = ~dta_list$ages,
        y = ~y_vals,
        z = ~z_vals,
        opacity = input$alpha_first_country,
        hoverinfo = "text",
        name = input$country_for_surface,
        text = custom_text,
        colorscale = list(
          seq(from = 0, to = 1, length.out = length(pal)),
          pal
        ),
        colorbar = list(
          title = input$country_for_surface
        )
      )  -> output
    
    # Add planes to CCFR
    
    # 
    if (input$select_surface_type == "ccfr" & input$show_threshold_planes == TRUE){
      # find range of birth years and ages 
      y_lims <- c(min(y_vals, na.rm = T), max(y_vals, na.rm = T))
      x_lims <- c(min(dta_list$ages, na.rm = T), max(dta_list$ages, na.rm = T))
      
      output <- output %>%   
        add_surface(
          x = ~x_lims,
          y = ~y_lims,
          z = ~matrix(rep(input$contour_1, 4), nrow = 2),
          showscale = FALSE, showlegend = FALSE, 
          opacity = 0.5,
          name = paste0("First milestone:", input$contour_1),
          hoverinfo = "name"
        ) %>% 
        add_surface(
          x = ~x_lims,
          y = ~y_lims,
          z = ~matrix(rep(input$contour_2, 4), nrow = 2),
          showscale = FALSE, showlegend = FALSE,
          name = paste0("Second milestone: ", input$contour_2),
          opacity = 0.5,
          hoverinfo = "name"
        ) %>% 
        add_surface(
          x = ~x_lims,
          y = ~y_lims,
          z = ~matrix(rep(input$contour_3, 4), nrow = 2),
          showscale = FALSE, showlegend = FALSE,
          name = paste0("Third milestone: ", input$contour_3),
          opacity = 0.5,
          hoverinfo = "name"
        ) %>% 
        add_surface(
          x = ~x_lims,
          y = ~y_lims,
          z = ~matrix(rep(input$contour_4, 4), nrow = 2),
          showscale = FALSE, showlegend = FALSE,
          name = paste0("Forth milestone: ", input$contour_4),
          opacity = 0.5,
          hoverinfo = "name"
        ) 
    }
    
  } else if (input$show_second_surface == "diff"){
    if (input$select_surface_type == "ccfr"){
      matrixify <- function(X, colname){
        tmp <- X %>% 
          select(birth_year, age, !!colname)
        tmp %>% spread(age, !!colname) -> tmp
        birth_years <- pull(tmp, birth_year)
        tmp <- tmp %>% select(-birth_year)
        ages <- names(tmp)
        mtrx <- as.matrix(tmp)
        return(list(ages = ages, birth_years = birth_years, vals = mtrx))
      }
      
      first_code <- input$country_for_surface
      second_code <- input$second_country_for_surface
      
      diff_df <- dta %>% 
        filter(code %in% c(first_code, second_code)) %>% 
        select(code, birth_year, age, ccfr = my_ccfr, series_ok) %>% 
        filter(series_ok) %>% 
        group_by(birth_year, age) %>% 
        filter(n() == 2) %>% 
        mutate(diff = ccfr[code == second_code] - ccfr[code == first_code]) %>% 
        filter(!is.na(diff)) %>% 
        ungroup() %>% 
        spread(code, ccfr) %>% 
        select(birth_year, age, !!first_code, !!second_code, diff) 
      # dataframe with birth_year, age, code1, code1, diff
      
      
      diff_mtrx <- matrixify(diff_df, "diff")
      diff_c1 <- matrixify(diff_df, first_code)
      diff_c2 <- matrixify(diff_df, second_code)
      
      
      abs_max_diff = abs(max(diff_df$diff, na.rm = T))
      
      n_years <- length(diff_mtrx$birth_years)
      n_ages <- length(diff_mtrx$ages)
      
      
      custom_text <- paste0(
        "For the ", rep(diff_mtrx$birth_years, times = length(diff_mtrx$ages)), " cohort, ",
        "by age ", rep(diff_mtrx$ages, each = length(diff_mtrx$birth_years)), ", the cumulative difference ",
        "was ", round(diff_mtrx$vals, 2), "\n",
        "(", round(diff_c1$vals, 2), " in ", first_code, "; ", 
        round(diff_c2$vals, 2), " in ", second_code, ")"
      ) %>%
        matrix(length(diff_mtrx$birth_years), length(diff_mtrx$ages))
      
      
      plot_ly(
        source = "fert_surface"
      ) %>%
        add_surface(
          x = ~diff_mtrx$ages, y = ~diff_mtrx$birth_years, z = ~diff_mtrx$vals,
          colorbar = list(
            title = "Difference"
          ),
          hoverinfo = "text", 
          text = custom_text, 
          colorscale = list(
            seq(from = -1, to = 1, length.out = 10),
            colorRampPalette(brewer.pal(5, "RdBu"))(10)
          ),
          cmin = -abs_max_diff,
          cmax =  abs_max_diff,
          cauto = F
        )  -> output
      
    } else if (input$select_surface_type == "asfr_year"){
      matrixify <- function(X, colname){
        tmp <- X %>% 
          select(year, age, !!colname)
        tmp %>% spread(age, !!colname) -> tmp
        years <- pull(tmp, year)
        tmp <- tmp %>% select(-year)
        ages <- names(tmp)
        mtrx <- as.matrix(tmp)
        return(list(ages = ages, years = years, vals = mtrx))
      }
      
      first_code <- input$country_for_surface
      second_code <- input$second_country_for_surface
      
      diff_df <- dta %>% 
        filter(code %in% c(first_code, second_code)) %>% 
        select(code, year, age, asfr) %>% 
        group_by(year, age) %>% 
        filter(n() == 2) %>% 
        mutate(diff = asfr[code == second_code] - asfr[code == first_code]) %>% 
        filter(!is.na(diff)) %>% 
        ungroup() %>% 
        spread(code, asfr) %>% 
        select(year, age, !!first_code, !!second_code, diff) 
      
      
      diff_mtrx <- matrixify(diff_df, "diff")
      diff_c1 <- matrixify(diff_df, first_code)
      diff_c2 <- matrixify(diff_df, second_code)
      
      
      abs_max_diff = abs(max(diff_df$diff, na.rm = T))
      
      n_years <- length(diff_mtrx$years)
      n_ages <- length(diff_mtrx$ages)
      
      custom_text <- paste0(
        "Year: ", rep(diff_mtrx$years, times = length(diff_mtrx$ages)), "\n",
        "Age: ", rep(diff_mtrx$ages, each = length(diff_mtrx$years)), "\n",
        "Difference: ", round(diff_mtrx$vals * 1000, 1), " babies / 1000 women\n",
        "(", round(diff_c1$vals * 1000, 1), " in ", first_code, "; ", 
        round(diff_c2$vals * 1000, 1), " in ", second_code, ")"
      ) %>%
        matrix(length(diff_mtrx$years), length(diff_mtrx$ages))
      
      plot_ly(
        source = "fert_surface"
        
      ) %>%
        add_surface(
          x = ~diff_mtrx$ages, y = ~diff_mtrx$years, z = ~diff_mtrx$vals,
          colorbar = list(
            title = "Difference"
          ),
          hoverinfo = "text", 
          text = custom_text, 
          colorscale = list(
            seq(from = -1, to = 1, length.out = 10),
            colorRampPalette(brewer.pal(5, "RdBu"))(10)
          ),
          cmin = -abs_max_diff,
          cmax =  abs_max_diff,
          cauto = F
        )  -> output
      
      
    } else if (input$select_surface_type == "asfr_cohort"){
      
      
      matrixify <- function(X, colname){
        
        tmp <- X %>% 
          select(birth_year, age, !!colname)
        tmp %>% spread(age, !!colname) -> tmp
        birth_years <- pull(tmp, birth_year)
        tmp <- tmp %>% select(-birth_year)
        ages <- names(tmp)
        mtrx <- as.matrix(tmp)
        return(list(ages = ages, birth_years = birth_years, vals = mtrx))
      }
      
      first_code <- input$country_for_surface
      second_code <- input$second_country_for_surface
      
      diff_df <- dta %>% 
        filter(code %in% c(first_code, second_code)) %>% 
        select(code, birth_year, age, asfr) %>% 
        group_by(birth_year, age) %>% 
        filter(n() == 2) %>% 
        mutate(diff = asfr[code == second_code] - asfr[code == first_code]) %>% 
        filter(!is.na(diff)) %>% 
        ungroup() %>% 
        spread(code, asfr) %>% 
        select(birth_year, age, !!first_code, !!second_code, diff) 
      
      diff_mtrx <- matrixify(diff_df, "diff")
      diff_c1 <- matrixify(diff_df, first_code)
      diff_c2 <- matrixify(diff_df, second_code)
      
      
      abs_max_diff = abs(max(diff_df$diff, na.rm = T))
      
      n_years <- length(diff_mtrx$birth_years)
      n_ages <- length(diff_mtrx$ages)
      
      custom_text <- paste0(
        "Birth year: ", rep(diff_mtrx$birth_years, times = length(diff_mtrx$ages)), "\n",
        "Age: ", rep(diff_mtrx$ages, each = length(diff_mtrx$birth_years)), "\n",
        "Difference: ", round(diff_mtrx$vals * 1000, 1), " babies / 1000 women\n",
        "(", round(diff_c1$vals * 1000, 1), " in ", first_code, "; ", 
        round(diff_c2$vals * 1000, 1), " in ", second_code, ")"
      ) %>%
        matrix(length(diff_mtrx$birth_years), length(diff_mtrx$ages))
      
      plot_ly(
        source = "fert_surface"
        
      ) %>%
        add_surface(
          x = ~diff_mtrx$ages, y = ~diff_mtrx$birth_years, z = ~diff_mtrx$vals,
          colorbar = list(
            title = "Difference"
          ),
          hoverinfo = "text", 
          text = custom_text, 
          colorscale = list(
            seq(from = -1, to = 1, length.out = 10),
            colorRampPalette(brewer.pal(5, "RdBu"))(10)
          ),
          cmin = -abs_max_diff,
          cmax =  abs_max_diff,
          cauto = F
        )  -> output
      
    }
  }
  
  output <- output %>%
    layout(
      
      scene = list(
        aspectratio = list(x = n_ages / n_years, y = 1, z = 0.50),
        xaxis = list(
          title = "Age in years"
        ),
        yaxis = list(
          title = ifelse(input$select_surface_type == "asfr_year", "Year", "Birth cohort year")
        ),
        zaxis = list(
          title = ifelse(input$show_second_surface == "diff", "Fertility difference", "Fertility")
        )
      )
    )
  
  return(output)
}