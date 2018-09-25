# Define server logic required to draw a histogram

source("scripts/surfaceplot_function.R")

server <- function(input, output, session) {
  
  # REACTIVE FUNCTIONS
  
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
  
  ## UI Modifications
  
  output$country_subtitle <- renderUI({
    if (input$tabset_1 == "Composite Plot"){
      h3("Select Countries")
    } else if (input$tabset_1 == "3D Surface Plot"){
      h3("Select Country")
    }
  })
  
  
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
  })## Figures 
  
  output$cclp <- renderPlot({
    input$redraw_figure
    
    out <- make_cclp()
    
    print(out)
  })
  
  # RENDER FUNCTIONS 
  
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
    surfaceplot_function(input=input, output = output, pal_set = pal_set)
    
  })
  
  output$surfaceplot_mini <- renderPlotly({
    surfaceplot_function(input=input, output = output, pal_set = pal_set)
    
  })
  
  output$cross_sections <- renderPlotly({
    s <- event_data("plotly_hover")
    if (length(s) == 0){
      return(NULL)
    } else {

      if(input$show_second_surface == "one"){
        if (input$select_surface_type == "asfr_year"){
          age <- as.numeric(s$x)
          year <- as.numeric(s$y)
          cohort <- year - age
          code <- input$country_for_surface

          # schedule by age 
          p1 <- dta %>% 
            filter(year == !!year) %>% 
            filter(code == !!code) %>% 
            plot_ly(x = ~age, y = ~asfr) %>% 
            add_lines() 
          
          # schedule by year
          p2 <- dta %>% 
            filter(age == !!age) %>% 
            filter(code == !!code) %>% 
            plot_ly(x = ~year, y = ~asfr) %>% 
            add_lines()
          
          # schedule by cohort 
          p3 <- dta %>% 
            mutate(cohort = year - age) %>% 
            filter(cohort == !!cohort) %>% 
            filter(code == !!code) %>% 
            plot_ly(x = ~age, y = ~asfr) %>% 
            add_lines()
          
          p <- subplot(list(p1, p2, p3), shareY = T, shareX = F) %>% 
            layout(
              yaxis = list(title = "Fertility", range = c(0, 0.5)),
              xaxis = list(title = "Age in years", range = c(12, 60)),
              xaxis2 = list(title = "Year", range = c(1900, 2015)),
              xaxis3 = list(
                title = paste0("Age in years for ", cohort, " birth cohort"), 
                range = c(12, 60)),
              title = paste0(
                "Cross sections at age ", age, ", year ", year, ", and birth cohort ", cohort
              ),
              showlegend = FALSE
            )
          
          return(p)
        } else if (input$select_surface_type == "asfr_cohort"){
          age <- as.numeric(s$x)
          year <- as.numeric(s$y)
          cohort <- year - age
          code <- input$country_for_surface
          
          #          browser()          
          # schedule by age 
          p3 <- dta %>% 
            filter(year == !!year) %>% 
            filter(code == !!code) %>% 
            plot_ly(x = ~age, y = ~asfr) %>% 
            add_lines() 
          
          # schedule by year
          p2 <- dta %>% 
            filter(age == !!age) %>% 
            filter(code == !!code) %>% 
            plot_ly(x = ~year, y = ~asfr) %>% 
            add_lines()
          
          # schedule by cohort 
          p1 <- dta %>% 
            mutate(cohort = year - age) %>% 
            filter(cohort == !!cohort) %>% 
            filter(code == !!code) %>% 
            plot_ly(x = ~age, y = ~asfr) %>% 
            add_lines()
          
          p <- subplot(list(p1, p2, p3), shareY = T, shareX = F) %>% 
            layout(
              yaxis = list(title = "Fertility", range = c(0, 0.5)),
              xaxis3 = list(title = "Age in years", range = c(12, 60)),
              xaxis2 = list(title = "Year", range = c(1900, 2015)),
              xaxis = list(
                title = paste0("Age in years for ", cohort, " birth cohort"), 
                range = c(12, 60)),
              title = paste0(
                "Cross sections at age ", age, ", year ", year, ", and birth cohort ", cohort
              ),
              showlegend = FALSE
            )
          
          return(p)
          
        } else if (input$select_surface_type == "ccfr"){
          age <- as.numeric(s$x)
          cohort <- s$y
          code <- input$country_for_surface
          
          dta %>% 
            filter(birth_year == !!cohort) %>% 
            filter(code == !!code) %>% 
            filter(series_ok) -> dta_ss
          
          if (dim(dta_ss)[1] == 0){
            return(NULL)
          } else {
            p <- dta_ss %>% 
              mutate(prev = 0) %>% 
              plot_ly(x = ~age) %>% 
              add_segments( 
                y = input$contour_1, yend = input$contour_1, 
                x = ~min(age), xend = 60,
                line = list(color = 'black', dash = 'dash')
              ) %>% 
              add_segments( 
                y = input$contour_2, yend = input$contour_2, 
                x = ~min(age), xend = 60,
                line = list(color = 'black')
              ) %>% 
              add_segments( 
                y = input$contour_3, yend = input$contour_3, 
                x = ~min(age), xend = 60,
                line = list(color = 'black', dash = 'dash', width = 3)
              ) %>% 
              add_segments( 
                y = input$contour_4, yend = input$contour_4, 
                x = ~min(age), xend = 60,
                line = list(color = 'black',                width = 3)
              ) %>% 
              add_ribbons(ymin = 0, ymax = ~my_ccfr) %>% 
              layout(
                yaxis = list(title = "Cumulative Fertility", range = c(0, 3)),
                xaxis = list(
                  title = paste0("Age in years for ", cohort, " birth cohort"), 
                  range = c(12, 60)),
                showlegend = FALSE
              )
            
            
            return(p)
          }
        }
        
      } else if (input$show_second_surface == "two"){

          code <- input$country_for_surface
          code2 <- input$second_country_for_surface
          if (input$select_surface_type == "asfr_year"){
            
            age <- as.numeric(s$x)
            year <- as.numeric(s$y)
            cohort <- year - age

            #          browser()          
            # schedule by age 
            p1 <- dta %>% 
              filter(year == !!year) %>% 
              filter(code %in% !!c(code, code2)) %>% 
              group_by(code) %>% 
              plot_ly(x = ~age, y = ~asfr, color = ~code, colors = c("red", "blue")) %>% 
              add_lines(showlegend = TRUE) 
            
            # schedule by year
            p2 <- dta %>% 
              filter(age == !!age) %>% 
              filter(code %in% !!c(code, code2)) %>%
              group_by(code) %>% 
              plot_ly(x = ~year, y = ~asfr, color = ~code, colors = c("red", "blue")) %>% 
              add_lines(showlegend = FALSE)
            
            # schedule by cohort 
            p3 <- dta %>% 
              mutate(cohort = year - age) %>% 
              filter(cohort == !!cohort) %>% 
              filter(code %in% !!c(code, code2)) %>%
              group_by(code) %>% 
              plot_ly(x = ~age, y = ~asfr, color = ~code, colors = c("red", "blue")) %>% 
              add_lines(showlegend = FALSE)
            
            p <- subplot(list(p1, p2, p3), shareY = T, shareX = F) %>% 
              layout(
                yaxis = list(title = "Fertility", range = c(0, 0.5)),
                xaxis = list(title = "Age in years", range = c(12, 60)),
                xaxis2 = list(title = "Year", range = c(1900, 2015)),
                xaxis3 = list(
                  title = paste0("Age in years for ", cohort, " birth cohort"), 
                  range = c(12, 60)),
                title = paste0(
                  "Cross sections at age ", age, ", year ", year, ", and birth cohort ", cohort
                )
              )
            
            return(p)
            
            
        } else if (input$select_surface_type == "asfr_cohort"){
          age <- as.numeric(s$x)
          year <- as.numeric(s$y)
          cohort <- year - age
          
          #          browser()          
          # schedule by age 
          p1 <- dta %>% 
            filter(year == !!year) %>% 
            filter(code %in% !!c(code, code2)) %>% 
            group_by(code) %>% 
            plot_ly(x = ~age, y = ~asfr, color = ~code, colors = c("red", "blue")) %>% 
            add_lines(showlegend = TRUE) 
          
          # schedule by year
          p2 <- dta %>% 
            filter(age == !!age) %>% 
            filter(code %in% !!c(code, code2)) %>%
            group_by(code) %>% 
            plot_ly(x = ~year, y = ~asfr, color = ~code, colors = c("red", "blue")) %>% 
            add_lines(showlegend = FALSE)
          
          # schedule by cohort 
          p3 <- dta %>% 
            mutate(cohort = year - age) %>% 
            filter(cohort == !!cohort) %>% 
            filter(code %in% !!c(code, code2)) %>%
            group_by(code) %>% 
            plot_ly(x = ~age, y = ~asfr, color = ~code, colors = c("red", "blue")) %>% 
            add_lines(showlegend = FALSE)
          
          p <- subplot(list(p1, p2, p3), shareY = T, shareX = F) %>% 
            layout(
              yaxis = list(title = "Fertility", range = c(0, 0.5)),
              xaxis3 = list(title = "Age in years", range = c(12, 60)),
              xaxis2 = list(title = "Year", range = c(1900, 2015)),
              xaxis = list(
                title = paste0("Age in years for ", cohort, " birth cohort"), 
                range = c(12, 60)),
              title = paste0(
                "Cross sections at age ", age, ", year ", year, ", and birth cohort ", cohort
              )
            )
          
          return(p)
        } else if (input$select_surface_type == "ccfr"){
          

          age <- as.numeric(s$x)
          cohort <- s$y
          code <- input$country_for_surface
          code2 <- input$second_country_for_surface

          
          dta %>% 
            filter(birth_year == !!cohort) %>% 
            filter(code %in% !!c(code, code2)) %>% 
            filter(series_ok) -> dta_ss
          
          if (dim(dta_ss)[1] == 0){
            return(NULL)
          } else {
            dta_ss1 <- dta_ss %>% filter(code == !!code)
            dta_ss2 <- dta_ss %>% filter(code == !!code2)
            
            if (dim(dta_ss1)[1] == 0 | dim(dta_ss2)[1] == 0) {
              return(NULL)
            } else {
              p <- dta_ss %>% 
                plot_ly() %>%
                add_ribbons(ymin = 0, ymax = ~my_ccfr, x = ~age, inherit = F,
                            data = dta_ss %>% filter(code == !!code), 
                            color = I("red"), opacity = 0.5) %>% 
                add_ribbons(ymin = 0, ymax = ~my_ccfr, x = ~age, inherit = F,
                            data = dta_ss %>% filter(code == !!code2),
                            color = I("blue"), opacity = 0.5) %>% 
                add_segments( 
                  y = input$contour_1, yend = input$contour_1, 
                  x = ~min(age), xend = 60,
                  line = list(color = 'black', dash = 'dash')
                ) %>% 
                add_segments( 
                  y = input$contour_2, yend = input$contour_2, 
                  x = ~min(age), xend = 60,
                  line = list(color = 'black')
                ) %>% 
                add_segments( 
                  y = input$contour_3, yend = input$contour_3, 
                  x = ~min(age), xend = 60,
                  line = list(color = 'black', dash = 'dash', width = 3)
                ) %>% 
                add_segments( 
                  y = input$contour_4, yend = input$contour_4, 
                  x = ~min(age), xend = 60,
                  line = list(color = 'black',                width = 3)
                ) %>% 
                
                layout(
                  yaxis = list(title = "Cumulative Fertility", range = c(0, 3)),
                  xaxis = list(
                    title = paste0("Age in years for ", cohort, " birth cohort"), 
                    range = c(12, 60)),
                  showlegend = FALSE
                )
              
            }

            return(p)
            }
          }
        
      } else if (input$show_second_surface == "diff"){
          code1 <- input$country_for_surface
          code2 <- input$second_country_for_surface
          
        if (input$select_surface_type == "asfr_year"){

          diff_df <- dta %>% 
            filter(code %in% c(code1, code2)) %>% 
            select(code, year, birth_year, age, asfr) %>% 
            group_by(year, age) %>% 
            filter(n() == 2) %>% 
            mutate(diff = asfr[code == code1] - asfr[code == code2]) %>% 
            filter(!is.na(diff)) %>% 
            ungroup() %>% 
            spread(code, asfr) %>% 
            select(year, age, birth_year, !!code1, !!code2, diff) 

          this_age <- as.numeric(s$x)
          this_year <- s$y
          this_cohort <- this_year - this_age
          
          p1 <- diff_df %>% 
            filter(year == this_year) %>% 
            plot_ly() %>% 
            add_lines(x = ~age, y = ~diff)
          
          p2 <- diff_df %>% 
            filter(age == this_age) %>% 
            plot_ly() %>% 
            add_lines(x = ~year, y = ~diff)
          
          p3 <- diff_df %>% 
            filter(birth_year == this_cohort) %>% 
            plot_ly() %>% 
            add_lines(x = ~age, y = ~diff)
          
          p <- subplot(list(p1, p2, p3), shareY = TRUE) %>% 
            layout(
              yaxis = list(title = "Difference in fertility", 
                           range = c(-0.1, 0.1)
              ),
              xaxis = list(title = "Age in years", range = c(12, 60)),
              xaxis2 = list(title = "Year", range = c(1900, 2016)),
              xaxis3 = list(title = "Age (cohort)", range = c(12, 60)),
              showlegend = FALSE
            )
          
          return(p)
          
        } else if (input$select_surface_type == "asfr_cohort"){
          diff_df <- dta %>% 
            filter(code %in% c(code1, code2)) %>% 
            select(code, year, birth_year, age, asfr) %>% 
            group_by(birth_year, age) %>% 
            filter(n() == 2) %>% 
            mutate(diff = asfr[code == code1] - asfr[code == code2]) %>% 
            filter(!is.na(diff)) %>% 
            ungroup() %>% 
            spread(code, asfr) %>% 
            select(year, age, birth_year, !!code1, !!code2, diff) 
          
          this_age <- as.numeric(s$x)
          this_year <- s$y
          this_cohort <- this_year - this_age
          
          p1 <- diff_df %>% 
            filter(birth_year == this_cohort) %>% 
            plot_ly() %>% 
            add_lines(x = ~age, y = ~diff)
          
          p2 <- diff_df %>% 
            filter(age == this_age) %>% 
            plot_ly() %>% 
            add_lines(x = ~year, y = ~diff)
          
          p3 <- diff_df %>% 
            filter(year == this_year) %>% 
            plot_ly() %>% 
            add_lines(x = ~age, y = ~diff)
          
          p <- subplot(list(p1, p2, p3), shareY = TRUE) %>% 
            layout(
              yaxis = list(title = "Difference in fertility", 
                           range = c(-0.1, 0.1)
              ),
              xaxis = list(title = "Age (cohort)",
                           range = c(12, 60)
              ),
              xaxis2 = list(title = "Year", 
                            range = c(1900, 2016)
              ),
              xaxis3 = list(title = "Age", 
                            range = c(12, 60)
              ),
              
              showlegend = FALSE
            )
          
          return(p)
          
        } else if (input$select_surface_type == "ccfr"){
          diff_df <- dta %>% 
            filter(code %in% c(code1, code2)) %>% 
            filter(series_ok) %>% 
            select(code, year, birth_year, age, ccfr = my_ccfr) %>% 
            group_by(birth_year, age) %>% 
            filter(n() == 2) %>% 
            mutate(diff = ccfr[code == code1] - ccfr[code == code2]) %>% 
            filter(!is.na(diff)) %>% 
            ungroup() %>% 
            spread(code, ccfr) %>% 
            select(year, age, birth_year, !!code1, !!code2, diff) 
          
          this_age <- as.numeric(s$x)
          this_year <- s$y
          this_cohort <- this_year - this_age
          
          p1 <- diff_df %>% 
            filter(birth_year == this_cohort) %>% 
            plot_ly() %>% 
            add_lines(x = ~age, y = ~diff)
          
          p2 <- diff_df %>% 
            filter(age == this_age) %>% 
            plot_ly() %>% 
            add_lines(x = ~year, y = ~diff)
          
          p3 <- diff_df %>% 
            filter(year == this_year) %>% 
            plot_ly() %>% 
            add_lines(x = ~age, y = ~diff)
          
          p <- subplot(list(p1, p2, p3), shareY = TRUE) %>% 
            layout(
              yaxis = list(title = "Difference in cumulative fertility", 
                           range = c(-1, 1)
              ),
              xaxis = list(title = "Age (cohort)",
                           range = c(12, 60)
              ),
              xaxis2 = list(title = "Year", 
                            range = c(1900, 2016)
              ),
              xaxis3 = list(title = "Age", 
                            range = c(12, 60)
              ),
              
              showlegend = FALSE
            )
          
        }
        
      }
        

    }
    
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_hover")
    if (length(s) == 0 ){
    "Move around!"
    } else {
    as.list(s)
  }
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