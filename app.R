
# Call tools needed for app -----------------------------------------------
library(shiny)
library(sf)
library(dtplyr)
library(dplyr)
library(leaflet)
library(shiny.semantic)
source("r/func_load_data.R")
source("r/func_filter_vessel_name.R")
source("r/func_2_spatial_df.R")
source("r/func_2_calc_distances.R")
source("r/func_get_max_distance.R")
source("r/func_get_record_of_top_distance.R")
source("r/func_create_map.R")
# source modules
source('r/mod-dropdowns.R')
source('r/mod-leafletmap.R')


# Create useful variables -------------------------------------------------
#* gridTemplate ------------------------------------------------------------

grid_main_app <- grid_template(default = list(
  areas = rbind(c("leaftlet_map")),
  rows_height = c("100%"),
  cols_width = c("100%")
))

# Define UI for application that draws a histogram
ui <- semanticPage(

  title = "Semantic Marine Ships",
  h1("Port movements analyzer"),

# main panel begins -------------------------------------------------------

    main_panel(

# begin of selectors segment: row 1 ----------------------------------------------

      segment(
        ship_dropdowns_UI("conditional_dropdowns")
      ), #end of selectors segment

# row 2 -------------------------------------------------------------------

      segment(
        cards(
          class = "two",
          card(class = "red",
               div(class = "content",
                   div(class = "header", "Main title card 1"),
                   div(class = "meta", "Sub title card 1"),
                   div(class = "description", "More detail description card 1")
               )
          ),
          card(class = "blue",
               div(class = "content",
                   div(class = "header", "Main title card 2"),
                   div(class = "meta", "Sub title card 2"),
                   div(class = "description", "More detail description card 2")
               )
          )
        )
      ), # end of cards segment
  segment(
    class = "basic",
    tags$a(class = "ui blue right ribbon label right",
           href = "https://www.marinetraffic.com/blog/information-transmitted-via-ais-signal/",
           "Find my ship"),
    leaflet::leafletOutput("reactive_leaflet_map")
  )
    ) # end of main panel
)

# Define server logic required to draw a histogram
server <- function(input, output) {

# call module servers -----------------------------------------------------

  dropdown_data <- ship_dropdowns_server("conditional_dropdowns")
  # create map ---------------------------------------------
  output$reactive_leaflet_map <- renderLeaflet({
    my_map <- create_distance_map_between_a_n_b(dropdown_data$my_filtered_data())
    my_map
  })
  
  observe({
    print(dropdown_data$vessel_name())
    print(head(dropdown_data$my_filtered_data()))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
