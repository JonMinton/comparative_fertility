#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(lattice)
library(latticeExtra)
library(shiny)
library(RColorBrewer)
library(plotly)

source("scripts/functions.R")
source("scripts/data_prep.R")


countries_to_select <- country_codes %>% 
  filter(to_keep == 1) %>% 
  pull(code)

names(countries_to_select) <- country_codes %>% 
  filter(to_keep == 1) %>% 
  pull(country)

palette_options <- c("New" = "adjusted_paired", "Cube YF" = "cubeyf", brew_pals)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Cumulative Comparative Cohort Fertility Explorer App"),
   
   # Sidebar 
   sidebarLayout(fluid = T,
    sidebarPanel(width = 3,
                 
        # CountrIES to compare (composite plot)   
        uiOutput("country_subtitle"),
        
        conditionalPanel(
          condition = "input.tabset_1 == 'Composite Plot'",
          selectInput("countries", label = "Select countries to compare",
            choices = countries_to_select,
            selected = "GBR_SCO",
            multiple = TRUE
          )
        ),
        
        # COUNTRY (singular) to compare (3D surface plot)
        conditionalPanel(
          condition = "input.tabset_1 == '3D Surface Plot'",
          selectInput("country_for_surface", label = "Select country visualise in 3D",
                      choices = countries_to_select,
                      selected = "GBR_SCO",
                      multiple = FALSE
          )
        ),

        conditionalPanel(
          condition = "input.tabset_1 == '3D Surface Plot'",
          selectInput("show_second_surface", label = "Show one surface, two surfaces, or differences?",
                        choices = c(
                          "One surface" = "one",
                          "Two surfaces" = "two",
                          "Differences between two surfaces" = "diff"),
                      selected = "one", multiple = FALSE
          )
        ),
        # Second CountRY to compare (3D surface plot)
        conditionalPanel(
          condition = "input.show_second_surface == 'two' || input.show_second_surface == 'diff'",
          selectInput("second_country_for_surface", label = "Select second country to visualise",
                      choices = countries_to_select,
                      selected = "GBRTENW",
                      multiple = FALSE
          )
        ),
        # Second CountRY to compare (3D surface plot)
        conditionalPanel(
          condition = "input.show_second_surface == 'two'",
          sliderInput("alpha_first_country", label = "Adjust first country transparency",
                        value = 1, min = 0, max = 1, step = 0.05)
        ),
        
        # Second CountRY to compare (3D surface plot)
        conditionalPanel(
          condition = "input.show_second_surface == 'two'",
          sliderInput("alpha_second_country", label = "Adjust second country transparency",
                      value = 1, min = 0, max = 1, step = 0.05)
        ),
        
        # Composite: Gridline options
        conditionalPanel(
          condition = "input.tabset_1 == 'Composite Plot'",
          checkboxInput("gridlines", label = "Check for gridlines",
                        value = FALSE
          )
        ),
        
        
        conditionalPanel(
          condition = "input.tabset_1 == 'Composite Plot'",
          selectInput("vis_type", label = "Select type of visualisation",
                      choices = c(
                        "Shade and lines" = "all",
                        "Shade only" = "shade",
                        "Lines only" = "contours"
                      )            
          )
        ),
        
        checkboxInput("shade", label = "Check to adjust palette/shading"),

        conditionalPanel(
          condition = "input.shade == true",
          selectInput("pal_type", label = "Select colour palette",
                      choices = palette_options,
                      selected = "adjusted_paired"
          )
        ),
        
        conditionalPanel(
          condition = "input.shade == true",
          sliderInput("num_cuts", label = "change number of shades displayed",
                      min = 3, max = 100, value = 20
          )
        ),
        
        conditionalPanel(
          condition = "input.shade == true",
          checkboxInput("adjust_shade_range", label = "Check to adjust shade range manually",
                        value = FALSE
          )
        ),
        
        conditionalPanel(
          condition = "input.adjust_shade_range == true",
          sliderInput("shade_range", label = "Adjust min and max of shade range",
                      min = 0, max = 1, value = c(0, 0.25)
          )
        ),
        

        
        conditionalPanel(
          condition = "input.tabset_1 == 'Composite Plot'",
          checkboxInput("contour_sliders", label = "Check to adjust contour lines",
                        value = FALSE)
        ),
        
        conditionalPanel(
          condition = "input.contour_sliders == true",
          sliderInput("contour_4", label = "Top contour",
                      value = 2.05, min = 0, max = 3.0, step = 0.05
          )
        ),
        
        conditionalPanel(
          condition = "input.contour_sliders == true",
          sliderInput("contour_3", label = "Third contour",
                      value = 1.80, min = 0, max = 3.0, step = 0.05)
        ),
        
        conditionalPanel(
          condition = "input.contour_sliders == true",
          sliderInput("contour_2", label = "Second contour",
                      value = 1.50, min = 0, max = 3.0, step = 0.05)
        ),
        
        conditionalPanel(
          condition = "input.contour_sliders == true",
          sliderInput("contour_1", label = "Bottom contour",
                      value = 1.30, min = 0, max = 3.0, step = 0.05)
        ),
        
        conditionalPanel(
          condition = "input.tabset_1 == 'Composite Plot'",
          actionButton("redraw_figure", "Click to render figure")
        ),
        
        conditionalPanel(
          condition = "input.tabset_1 == 'Composite Plot'",
          checkboxInput("show_schedule", label = "Check to visualise a cohort's schedule",
            value = FALSE
          )          
        ),
        
        conditionalPanel(
          condition = "input.tabset_1 == 'Composite Plot'",
          checkboxInput("adjust_fileoptions", label = "Check to adjust figure download options",
            value = FALSE
          )
        ),
        
        
        conditionalPanel(
          condition = "input.adjust_fileoptions == true",
          textInput("fig_name", "Figure name",
                    value = "figure")
        ),
        
        conditionalPanel(
          condition = "input.adjust_fileoptions == true",
          numericInput("fig_width", "Figure width",
                       value = 10, 
                       min = 1, max = 10000)
        ),
        
        conditionalPanel(
          condition = "input.adjust_fileoptions == true",
          numericInput("fig_height", "Figure height", 
                       value = 10,
                       min = 1, max = 10000)
        ),
        
        conditionalPanel(
          condition = "input.adjust_fileoptions == true",
          numericInput("fig_res", "Figure resolution (DPI)",
                       value = 150, 
                       min = 10, max = 10000)
        ),
        
        conditionalPanel(
          condition = "input.adjust_fileoptions == true",
          selectInput("fig_unit", "Dimension unit type (cm default)",
                      choices = c(
                        "Pixels" = "px",
                        "Inches" = "in",
                        "Centimetres" = "cm",
                        "Millimetres" = "mm"
                      ),
                      selected = "cm"
          )
        ),
        
        conditionalPanel(
          condition = "input.adjust_fileoptions == true", 
          selectInput("fig_type", label = "Select file type",
                      choices = c(
                        "PNG" = "png",
                        "SVG" = "svg",
                        "PDf" = "pdf",
                        "Bitmap" = "bmp"
                      ),
                      selected = "png"
          )
        ),
        
        conditionalPanel(
          condition = "input.tabset_1 == 'Composite Plot' && input.show_schedule == true ",
          sliderInput("cohort", label = "Birth cohort", sep = "",
                      value = 1950, min = 1920, max = 1980, step = 1)
        )


      ),

      
      # Show a plot of the generated distribution
      
      mainPanel( width = 9,
        tabsetPanel(id = "tabset_1", type = "tab",
          tabPanel(title = "Composite Plot", 
            plotOutput("cclp"),
            downloadButton(outputId = "download_figure", "Download figure"),
            br(),
            conditionalPanel(
              condition = "input.show_schedule == true",
              plotlyOutput("schedule")
            )
          ),
          tabPanel(title = "3D Surface Plot",
            plotlyOutput("surfaceplot", height = 800)       
          )
        )

      )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
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
      input$contour_1, 
      input$contour_2,
      input$contour_3,
      input$contour_4
    )
    
    pal <- pal_set()
    
    if(is.null(input$shade_range)){
      
      out <- produce_composite_lattice(
        DTA = dta_subset,
        add_gridlines = add_gridlines,
        return = return,
        colscheme = pal,
        cohort = ifelse(input$show_schedule, input$cohort, NA),
        contour_vals = contour_vals,
        shading_cuts = input$num_cuts
      )
    }  else {
      
      out <- produce_composite_lattice(
        DTA = dta_subset,
        add_gridlines = add_gridlines,
        return = return,
        colscheme = pal,
        cohort = ifelse(input$show_schedule, input$cohort, NA),
        contour_vals = contour_vals,
        shading_cuts = input$num_cuts,
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
        )


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
     
     # cat(file=stderr(), "show_second_surface is", input$show_second_surface, "\n")

     dta_list <- turn_asfr_to_matrix(DTA = dta, CODE = input$country_for_surface)

     if (input$show_second_surface %in% c('two', 'diff')){
       dta2_list <- turn_asfr_to_matrix(DTA = dta, CODE = input$second_country_for_surface)
     }  
     pal <- pal_set()
     
     n_years <- length(dta_list$birth_years)
     n_ages <- length(dta_list$ages)
     

     if (input$show_second_surface == 'two'){
       plot_ly() %>% 
         add_surface(
           x = ~dta_list$ages, y = ~dta_list$birth_years, z = ~dta_list$asfr,
           opacity = input$alpha_first_country,
           colorscale = list(
             c(0, 1),
             c('rgb(255,0,0)', 'rgb(255,0,0)')
           )
         )  %>% 
         layout(
           scene = list(
             aspectratio = list(x = n_ages / n_years, y = 1, z = 0.40)
           )
           
         ) %>% 
         add_surface(
           x = ~dta2_list$ages, y = ~dta2_list$birth_years, z = ~dta2_list$asfr,
           opacity = input$alpha_second_country,
           colorscale = list(
             c(0, 1),
             c('rgb(0,0,255)', 'rgb(0,0,255)')
           )
         ) -> output
     } else if (input$show_second_surface == 'one'){
       plot_ly() %>% 
         add_surface(
           x = ~dta_list$ages, y = ~dta_list$birth_years, z = ~dta_list$asfr,
           opacity = input$alpha_first_country,
           colorscale = list(
             seq(from = 0, to = 1, length.out = length(pal)), 
             pal
           )
         )  %>% 
         layout(
           scene = list(
             aspectratio = list(x = n_ages / n_years, y = 1, z = 0.40)
           )
           
         ) -> output
       
       
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
       
       n_years <- length(dta_list$birth_years)
       n_ages <- length(dta_list$ages)
       
       plot_ly() %>% 
         add_surface(
           x = ~ages, y = ~birth_years, z = ~diff_asfr,
           colorscale = list(
             seq(from = 0, to = 1, length.out = length(pal)), 
             pal
           )
         )  %>% 
         layout(
           scene = list(
             aspectratio = list(x = n_ages / n_years, y = 1, z = 0.40)
           )
           
         ) -> output
       
       
       
       
     }
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
# Run the application 
shinyApp(ui = ui, server = server)

