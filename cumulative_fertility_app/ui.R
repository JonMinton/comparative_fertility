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
          condition= "input.tabset_1 == '3D Surface Plot'",
          selectInput("select_surface_type", label = "Select type of surface to show",
                        choices = c(
                          "ASFR by year" = "asfr_year",
                          "ASFR by cohort" = "asfr_cohort",
                          "CCFR by cohort" = "ccfr"
                      ),
                      selected = "asfr_cohort", multiple = FALSE
          )
        ),
        
        conditionalPanel(
          condition= "input.tabset_1 == '3D Surface Plot'",
          checkboxInput("show_threshold_planes", label = "Check to display contour thresholds",
            value = FALSE
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
          condition = "input.tabset_1 == 'Composite Plot' && input.show_schedule == true ",
          sliderInput("cohort", label = "Birth cohort", sep = "",
                      value = 1950, min = 1920, max = 1980, step = 1)
        ),
        conditionalPanel(
          condition = "input.tabset_1 == 'Composite Plot' && input.show_schedule == true",
          checkboxInput("subplot_limit", label = "Check to manually adjust subplot limits",
                        value = FALSE
          )
        ),
        
        conditionalPanel(
          condition = "input.tabset_1 == 'Composite Plot' && input.subplot_limit == true",
          sliderInput("subplot_limits_asfr", label = "Slide to adjust ASFR subplot limits",
                      min = 0, max = 1, value = c(0, 0.3), step = 0.1
          )
        ),
        conditionalPanel(
          condition = "input.tabset_1 == 'Composite Plot' && input.subplot_limit == true",
          sliderInput("subplot_limits_ccfr", label = "Slide to adjust CCFR subplot limits",
                      min = 0, max = 5, value = c(0, 3), step = 0.1
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

