# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Contour line adjustments 
  # See example from here 
  # https://shiny.rstudio.com/reference/shiny/1.1.0/updateSliderInput.html
  
  observe({
    val <- input$contour_4
    updateSliderInput(session, "contour_3", max = val)
  })
  observe({
    val <- input$contour_3
    updateSliderInput(session, "contour_2", max = val)
  })
  observe({
    val <- input$contour_2
    updateSliderInput(session, "contour_1", max = val)
  })
  
  pal_set <- reactive({
    palname <- input$pal_type
    if (palname == "adjusted_paired"){
      out <- colorRampPalette(adjusted_paired)(200)
    } else if (palname == "cubeyf"){
      out <- cubeyf_palette 
    } else {
      out <- colorRampPalette(brewer.pal(11, palname))(200)
    }
    return(out)
  })
  
  get_schedule_subset <- reactive({
    dta_subset <- dta %>% 
      filter(code %in% input$countries) %>% 
      filter(birth_year == input$cohort)
    
    dta_subset %>% 
      select(country, geography, age, series_ok, country, geography, asfr, my_ccfr) %>% 
      mutate(ccfr = ifelse(series_ok, my_ccfr, NA)) %>% 
      select(-my_ccfr) %>% 
      gather(key = "measure", value = "value", asfr:ccfr) %>% 
      select(-series_ok) -> dta_subset 
    
    
  })
  
  make_cclp <- function(){
    dta_subset <- isolate(dta %>% filter(code %in% input$countries))
    add_gridlines <- isolate(input$gridlines)
    return <- isolate(input$vis_type)
    
    pal <- pal_set()
    
    contour_vals <- c(
      isolate(input$contour_1), 
      isolate(input$contour_2),
      isolate(input$contour_3),
      isolate(input$contour_4)
    )
    
    pal <- pal_set()
    
    if(is.null(isolate(input$shade_range))){
      
      out <- produce_composite_lattice(
        DTA = dta_subset,
        add_gridlines = add_gridlines,
        return = return,
        colscheme = pal,
        cohort = ifelse(isolate(input$show_schedule), isolate(input$cohort), NA),
        contour_vals = contour_vals,
        shading_cuts = isolate(input$num_cuts)
      )
    }  else {
      
      out <- produce_composite_lattice(
        DTA = dta_subset,
        add_gridlines = add_gridlines,
        return = return,
        colscheme = pal,
        cohort = ifelse(isolate(input$show_schedule), isolate(input$cohort), NA),
        contour_vals = contour_vals,
        shading_cuts = isolate(input$num_cuts),
        shading_limits = isolate(input$shade_range)
      )
    }
    return(out)
  }
  
  ## Aesthetics 
  
  output$country_subtitle <- renderUI({
    if (input$tabset_1 == "Composite Plot"){
      h3("Select Countries")
    } else if (input$tabset_1 == "3D Surface Plot"){
      h3("Select Country")
    }
  })
  
  
  ## Figures 
  
  output$cclp <- renderPlot({
    input$redraw_figure
    
    out <- make_cclp()
    
    print(out)
  })
  
  output$schedule <- renderPlotly({
    
    get_schedule_subset() %>% 
      spread(measure, value) -> dta_subset
    
    dta_subset %>% 
      plot_ly(y = ~age, x = ~asfr, color = ~country,
              hoverinfo = 'text',
              text = ~paste0(
                country, " (", geography, ")\n",
                round(asfr * 1000, 0), " babies/1000 women at age ", age
              )
      ) %>% 
      add_paths(name = ~country) %>% 
      layout(title = paste("Age-specific & cumulative schedules for", input$cohort, "Birth cohort"),
             xaxis = list(title = "Fertility rate"),
             yaxis = list(title = "Age in years")
      ) -> p1
    
    
    p2 <- dta_subset %>% 
      plot_ly(x = ~ccfr, y = ~age, color = ~country, showlegend = FALSE,
              hoverinfo = 'text',
              text = ~paste0(
                country, " (", geography, ")\n",
                round(ccfr , 2), " babies by age ", age
              )
              
      ) %>% 
      add_paths(name = ~country) %>% 
      add_segments( 
        x = input$contour_1, xend = input$contour_1, 
        y = ~min(age), yend = ~max(age),
        line = list(color = 'black', dash = 'dash')
      ) %>% 
      add_segments( 
        x = input$contour_2, xend = input$contour_2, 
        y = ~min(age), yend = ~max(age),
        line = list(color = 'black')
      ) %>% 
      add_segments( 
        x = input$contour_3, xend = input$contour_3, 
        y = ~min(age), yend = ~max(age),
        line = list(color = 'black', dash = 'dash', width = 3)
      ) %>% 
      add_segments( 
        x = input$contour_4, xend = input$contour_4, 
        y = ~min(age), yend = ~max(age),
        line = list(color = 'black',                width = 3)
      ) %>% 
      layout(
        xaxis = list(title = "Cumulative Fertility")
      ) 
    
    
    
    subplot(
      p1, p2,
      shareY = TRUE,
      titleX = TRUE
    ) %>% 
      layout(
        xaxis = list(title = "Fertility rate (Babies/woman)"),
        xaxis2 = list(title = "Cumulative fertility (Babies by age on y axis)")
      ) -> out
    
    if (input$subplot_limit == TRUE){
      out %>% 
        layout(
          xaxis = list(range = input$subplot_limits_asfr),
          xaxis2 = list(range = input$subplot_limits_ccfr)
        ) -> out
    }
    
    return(out)
  })
  
  # Now to add a surface plot 
  
  output$surfaceplot <- renderPlotly({
    
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
    
    input$redraw_figure
    
    dta_list <- turn_asfr_to_matrix(DTA = dta, CODE = input$country_for_surface)
    
    if (input$show_second_surface %in% c('two', 'diff')){
      dta2_list <- turn_asfr_to_matrix(DTA = dta, CODE = input$second_country_for_surface)
      z_lims <- c(
        min(dta_list$asfr, dta2_list$asfr),
        max(dta_list$asfr, dta2_list$asfr)
      )
    }  
    pal <- pal_set()
    
    n_years <- length(dta_list$birth_years)
    n_ages <- length(dta_list$ages)
    

    # All versions should have the same xy labelling - so should be added as common layout option
    
    if (input$show_second_surface == 'two'){
      plot_ly(
      ) %>% 
        add_surface(
          x = ~dta_list$ages, y = ~dta_list$birth_years, z = ~dta_list$asfr,
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
          x = ~dta2_list$ages, y = ~dta2_list$birth_years, z = ~dta2_list$asfr,
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
      plot_ly() %>% 
        add_surface(
          x = ~dta_list$ages, y = ~dta_list$birth_years, z = ~dta_list$asfr,
          opacity = input$alpha_first_country,
          colorscale = list(
            seq(from = 0, to = 1, length.out = length(pal)), 
            pal
          ),
          colorbar = list(
            title = input$country_for_surface
          )
        )  -> output
    } else if (input$show_second_surface == "diff"){
      dta %>% 
        filter(code %in% c(input$country_for_surface, input$second_country_for_surface)) %>% 
        select(code, birth_year, age, asfr) %>% 
        spread(code, asfr) %>% 
        mutate(diff = pull(., 4) - pull(., 3)) -> tmp
      
      tmp %>% 
        select(birth_year, age, diff) %>% 
        spread(age, diff) ->  tmp
      
      birth_years <- pull(tmp, birth_year)
      
      tmp <- tmp[,-1]
      ages <- colnames(tmp)
      diff_asfr <- as.matrix(tmp)
      
      abs_max_diff = abs(max(diff_asfr, na.rm = T))
      
      n_years <- length(dta_list$birth_years)
      n_ages <- length(dta_list$ages)
      
      plot_ly() %>% 
        add_surface(
          x = ~ages, y = ~birth_years, z = ~diff_asfr,
          colorbar = list(
            title = "Difference"
          ),
          colorscale = list(
            seq(from = -1, to = 1, length.out = 10), 
            colorRampPalette(brewer.pal(5, "RdBu"))(10)
          ),
          cmin = -abs_max_diff,
          cmax =  abs_max_diff,
          cauto = F
        )  -> output
    }
    
    output <- output %>% 
      layout(
        
        scene = list(
          aspectratio = list(x = n_ages / n_years, y = 1, z = 0.50),
          xaxis = list(
            title = "Age in years"
          ),
          yaxis = list(
            title = "Birth cohort year"
          ),
          zaxis = list(
            title = ifelse(input$show_second_surface == "diff", "Fertility difference", "Fertility")
          )
        )
      )
    
    return(output)
  })
  
  output$download_figure <- downloadHandler(
    filename <- function() {
      paste(input$fig_name, input$fig_type, sep = ".")
    },
    content = function(file){
      if (input$fig_type == "png"){
        png(file, width = input$fig_width, height = input$fig_height, units = input$fig_unit, res = input$fig_res)
        print(make_cclp())
        dev.off()
      } else if (input$fig_type == "svg"){
        svg(file)
        print(make_cclp())
        dev.off()
        
      } else if (input$fig_type == "bmp"){
        bmp(file, width = input$fig_width, height = input$fig_height, units = input$fig_unit, res = input$fig_res)
        print(make_cclp())
        dev.off()
      } else if (input$fig_type == "pdf"){
        pdf(file, width = input$fig_width, height = input$fig_height, units = input$fig_unit)
        print(make_cclp())
        dev.off()
      }
      
    }
  )
  # )
}