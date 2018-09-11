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
   titlePanel("Select Countries"),
   
   # Sidebar 
   sidebarLayout(fluid = T,
      sidebarPanel(width = 3,
        selectInput("countries", label = "Select countries to compare",
          choices = countries_to_select,
          selected = "GBR_SCO",
          multiple = TRUE
        ),
        actionButton("redraw_figure", "Click to render figure"),
        checkboxInput("gridlines", label = "Check for gridlines",
          value = FALSE
        ),
        selectInput("vis_type", label = "Select type of visualisation",
          choices = c(
            "Shade and lines" = "all",
            "Shade only" = "shade",
            "Lines only" = "contours"
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
        
        checkboxInput("contour_sliders", label = "Check to adjust contour lines",
                      value = FALSE
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
      
      checkboxInput("show_schedule", label = "Check to visualise a cohort's schedule",
                      value = FALSE
        ),
      
      conditionalPanel(
        condition = "input.show_schedule == true",
        sliderInput("cohort", label = "Birth cohort", sep = "",
                    value = 1950, min = 1920, max = 1980, step = 1,
                    animate = animationOptions(interval = 300, loop = FALSE))
        )
      ),

      
      # Show a plot of the generated distribution
      
      mainPanel( width = 9,
         plotOutput("cclp"),
         conditionalPanel(
           condition = "input.show_schedule == true",
           plotlyOutput("schedule")
         ),
         plotlyOutput("surfaceplot")
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
  
   output$cclp <- renderPlot({
     input$redraw_figure
     
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
         cohort = input$cohort,
         contour_vals = contour_vals,
         shading_cuts = input$num_cuts
       )
     }  else {
        
       out <- produce_composite_lattice(
         DTA = dta_subset,
         add_gridlines = add_gridlines,
         return = return,
         colscheme = pal,
         cohort = input$cohort,
         contour_vals = contour_vals,
         shading_cuts = input$num_cuts,
         shading_limits = isolate(input$shade_range)
       )
     }
    return(out)
     
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
     
     input$redraw_figure
     dta_subset <- isolate(dta %>% filter(code %in% input$countries))
     
     # temp while getting to work with just one figure
     dta_first_country <- dta_subset %>% filter(code == unique(code)[1])
     
     dta_first_country %>% 
       select(birth_year, age, asfr) %>% 
       spread(age, asfr) ->  tmp
     birth_years <- pull(tmp, birth_year)
     tmp <- tmp[,-1]
     ages <- colnames(tmp)
     asfr <- as.matrix(tmp)
#     asfr <- asfr[,dim(asfr)[2]:1]
     n_years <- length(birth_years)
     n_ages <- length(ages)
      
     pal <- pal_set()
     
     plot_ly(

     ) %>% 
       add_surface(
         x = ~ages, y = ~birth_years, z = ~asfr,
         colorscale = list(
           seq(from = 0, to = 1, length.out = length(pal)), 
           pal
          )
       )  %>% 
         layout(
  
           scene = list(
             aspectratio = list(x = n_ages / n_years, y = 1, z = 1)
           )
           
         ) 
       
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

