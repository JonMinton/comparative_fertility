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
library(glue)

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
          condition = "input.tabset_1 == '3D Surface Plot' || input.tabset_1 == 'Linked Plots'",
          selectInput("country_for_surface", label = "Select country visualise in 3D",
                      choices = countries_to_select,
                      selected = "GBR_SCO",
                      multiple = FALSE
          )
        ),

        conditionalPanel(
          condition= "input.tabset_1 == '3D Surface Plot' || input.tabset_1 == 'Linked Plots'",
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
          condition= "input.tabset_1 == '3D Surface Plot' || input.tabset_1 == 'Linked Plots'",
          checkboxInput("show_threshold_planes", label = "Check to display contour thresholds",
            value = FALSE
          )
        ),
        conditionalPanel(
          condition = "input.tabset_1 == '3D Surface Plot' || input.tabset_1 == 'Linked Plots'",
          selectInput("show_second_surface", label = "Show one surface, two surfaces, or differences?",
                        choices = c(
                          "One surface" = "one",
                          "Two surfaces" = "two",
                          "Differences between two surfaces" = "diff"),
                      selected = "one", multiple = FALSE
          )
        ),
        # Second Country to compare (3D surface plot)
        conditionalPanel(
          condition = "input.show_second_surface == 'two' || input.show_second_surface == 'diff' && input.tabset_1 != 'Composite Plot'",
          selectInput("second_country_for_surface", label = "Select second country to visualise",
                      choices = countries_to_select,
                      selected = "GBRTENW",
                      multiple = FALSE
          )
        ),
        # Second Country to compare (3D surface plot)
        conditionalPanel(
          condition = "input.show_second_surface == 'two' && input.tabset_1 != 'Composite Plot'",
          sliderInput("alpha_first_country", label = "Adjust first country transparency",
                        value = 1, min = 0, max = 1, step = 0.05)
        ),
        
        # Second Country to compare (3D surface plot)
        conditionalPanel(
          condition = "input.show_second_surface == 'two' && input.tabset_1 != 'Composite Plot'",
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
        # Composite: Gridline options
        conditionalPanel(
          condition = "input.tabset_1 == 'Composite Plot' && input.gridlines == true",
          checkboxInput("period_gridlines", label = "Also add period gridlines?",
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
                        "PDF" = "pdf",
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
            ),
            br(),
            br(),
            h2("Information"),
            br(),
            p("Welcome to the comparative cohort fertility explorer app!"),
            br(),
            h3("Options"),
            p("The initial options available in this tab include:"),
            tags$ol(
              tags$li(
                tags$b("Select countries to compare"),
                tags$i(
                  "This allows you to select one or more of 45 countries from the Human Fertility Database",
                  " and Human Fertility Collection. The countries you select will appear in the",
                  "Composite plot at the top, whenever you click the button",
                  tags$b("'Click to render figure'"),
                  "and will appear in the cohort schedule plot below if activated"
                )
              ),
              tags$li(
                tags$b("Check for gridlines"),
                tags$i("This adds 5 year by 5 year gridlines to the composite plot")
              ),
              tags$li(
                tags$b("Select type of visualisation"),
                tags$i("This allows you to select three different renderings of the main plot:"),
                tags$ul(
                  tags$li("Lines: shows only the contour lines which map the ages at which particular cumulative fertility milestones are reached for different birth cohorts in each of the countries selected"),
                  tags$li("Shade: Show only the heatmap part of the visualisation, which are mapped from the age-specific fertility rates at each age and cohort"),
                  tags$li("Shades and lines (Default): Show both the heatmap/levelplot and contour plot aspects of the visualisation, meaning both age-specific fertility rates and cumulative cohort fertility milestones are shown in the same visualisation")
                )
              ),
              tags$li(
                tags$b("Check to adjust palette/shading"),
                tags$i("This opens up two additional options:"),
                tags$ul(
                  tags$li("Select colour palette: Select from a range of colour palettes for presenting the visualisations. These are mainly taken from the ColorBrewer package, but also include the Cube YF palette"),
                  tags$li("Change number of shades displayed: This determines the number of distinct colours/shades displayed in the composite plots")
                )
              ),
              tags$li(
                tags$b("Check to adjust shade range manually"),
                tags$i("This sets a lower and upper limit to the age-specific fertility rates shown on the composite plot")
              ),
              tags$li(
                tags$b("Check to adjust contour lines"),
                tags$i("This opens up four sliders, which determine the 'milestone' values for cumulative cohort fertility levels which each of the four contours are mapped to:"),
                tags$ol(
                  tags$li("Top contour: This is the thick solid line on the plots"),
                  tags$li("Third contour: This is represented by a thick dashed line"),
                  tags$li("Second contour: This is represented by a think solid line"),
                  tags$li("Bottom contour: This is represented by a thin dashed line")
                )
              ),
              tags$li(
                tags$b("Click to render figure"),
                tags$i("This redraws the composite figure")
              ),
              tags$li(
                tags$b("Check to visualise a cohort's schedule"),
                tags$i("This opens up two subfigures which show the age-specific fertility rates (left plot), and cumulative cohort fertility rates along with vertical dashed lines indicating fertility milestones (right plot)",
                        "for a specific selected birth cohort. Hovering over the lines reveals further information about that population"),
                tags$ul(
                  tags$li("Birth cohort: A slider which allows different birth cohorts to be selected"),
                  tags$li("Check to manually adjust subplot limits: Opens up two sliders to set the range of the left and right subplots")
                )
              ),
              tags$li(
                tags$b("Check to adjust figure download options"),
                tags$i("This opens up a range of drop-down options to configure file download options. This includes the 
                       name of the figure, the width and height, the units of the width and height, the resolution in 
                       dots per inch (DPI), and the file type (default PNG)")
              )
            ),
            br(),
            p("Finally, below the top figure is a button, 'download figure', which allows you to download the composite figure you have produced in your preferred file format, size and resolution")
          ),
          tabPanel(title = "3D Surface Plot",
            plotlyOutput("surfaceplot", height = 800),
            br(),
            h3("Information"),
            p("The 3D surface plot tab shows the Lexis surfaces which the composite plots visualise in a number of different ways."),
            p("The difference between the 3D surface plot and the 'linked plots' tab is that the linked plots tab has a smaller 'canvas' for the 3d Surface, 
              but also additional subplots which show 'slices' through the surface across the age, year and birth cohort axes"),
            p("There are a number of options available in the side panel"),
            tags$ol(
              tags$li(
                tags$b("Select country to visualise"),
                tags$i("This selects a single country to display on the Lexis surface. Unlike the Composite plot version only a single country
                       can be selected within this drop-down menu")
              ),
              tags$li(
                tags$b("Select type of surface to show"),
                tags$i("Options include: Age-specific fertility rates by cohort (the attribute mapped to the colour/shade in the composite
chart); ASFR by period; and cumulative cohort fertility rates (CCFR) by cohort, which is the attribute the contour lines in the composite plot maps to")
                ),
              tags$li(
                tags$b("Check to display contour thresholds"),
                tags$i("If the surface type shown is CCFR by cohort, then four horizontal planes are shown when this option is selected.
                       The intersection between the CCFR surface and a horizontal plane shows the age at which each displayed birth cohort 
                       reaches the cumulative fertility associated with that plane. These intersections between surfaces and planes is what the 
                       contour lines in the composite plot show (though the surfaces in the 3D plots and composite plots are reversed)")
              ),
              tags$li(
                tags$b("Show one surface, two surfaces or differences"),
                tags$i("If two surfaces are selected, another drop-down menu opens allowing a second population to be 
                       visualised within the same Lexis space. The first population surface is coloured blue and the second 
                       red, and the opacity of both surfaces can be adjusted with slider options that open up when 
                       this option is selected, allowing you to see one surface within another. If differences is selected,
                        then difference between surfaces (second - first) is shown on a scale whose z value is set to the middle of the scale. The 
                        surfaces are coloured red if the second population has a higher value than the first, and blue if the first population has a 
                        higher value than the second")
                )
              ),
            br(),
            p(
              tags$b("Note:"),
              "When the user hovers over the surface, tooltips appear with information corresponding to the surface's point"
            )
          ),
          tabPanel(title = "Linked Plots",
            plotlyOutput("surfaceplot_mini", height = 500),
            plotlyOutput("cross_sections", height = 300),
            br(),
            h3("Information"),
            p("The linked plot is like the previous tab ('3d surface plot'), except the 3D surface canvas is smaller, 
              and when the user moves the cursor over the 3D surface corresponding sweeps through the data along age,
              period and year appear in subplots below the main image.")
          )
        )

      )
  )
)

