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

source("../scripts/functions.R")
source("../scripts/data_prep.R")


countries_to_select <- country_codes %>% 
  filter(to_keep == 1) %>% 
  pull(code)

names(countries_to_select) <- country_codes %>% 
  filter(to_keep == 1) %>% 
  pull(country)

palette_options <- c("New" = "adjusted_paired", brew_pals)


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
        selectInput("pal_type", label = "Select colour palette",
          choices = palette_options,
          selected = "adjusted_paired"
        ),
        sliderInput("contour_4", label = "Top contour",
          value = 2.05, min = 0, max = 3.0, step = 0.05),
        sliderInput("contour_3", label = "Third contour",
          value = 1.80, min = 0, max = 3.0, step = 0.05),
        sliderInput("contour_2", label = "Second contour",
          value = 1.50, min = 0, max = 3.0, step = 0.05),
        sliderInput("contour_1", label = "Bottom contour",
          value = 1.30, min = 0, max = 3.0, step = 0.05),
        
        sliderInput("cohort", label = "Birth cohort", sep = "",
          value = 1950, min = 1920, max = 1980, step = 1)
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel( width = 9,
         plotOutput("cclp"),
         plotOutput("schedule")
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  pal_set <- reactive({
    palname <- input$pal_type
    
    if (palname == "adjusted_paired"){
      out <- colorRampPalette(adjusted_paired)(200)
    } else {
      out <- colorRampPalette(brewer.pal(12, palname))(200)
    }
    return(out)
  })
  
   output$cclp <- renderPlot({
     input$redraw_figure
     
     isolate({
       dta_subset <- dta %>% filter(code %in% input$countries)
       pal <- pal_set()
       add_gridlines <- input$gridlines
       return <- input$vis_type
       contour_vals <- c(
         input$contour_1, 
         input$contour_2,
         input$contour_3,
         input$contour_4
       )
       
     })
     

     dta_subset <- isolate(dta %>% filter(code %in% input$countries))
     pal <- pal_set()
     produce_composite_lattice(
       DTA = dta_subset,
       add_gridlines = add_gridlines,
       return = return,
       colscheme = pal,
       contour_vals = contour_vals
        )

   })
   
   output$schedule <- renderPlot({
     input$redraw_figure
     
     isolate({
       dta_subset <- dta %>% 
         filter(code %in% input$countries) %>% 
         filter(birth_year == input$cohort)
       
     })
     
     dta_subset %>% 
       ggplot(
         aes(x = age, y = asfr, group = country, fill = country, colour = geography)
         ) +
       geom_polygon(alpha = 0.3)
     
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

