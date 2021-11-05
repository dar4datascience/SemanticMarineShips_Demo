library(shinyWidgets)
library(shiny)
library(dtplyr)
library(dplyr)
library(leaflet)
library(shiny.semantic)
source("R/func_load_data.R")
source("R/func_filter_vessel_name.R")
source("R/func_2_spatial_df.R")
source("R/func_2_calc_distances.R")
source("R/func_get_max_distance.R")
source("R/func_get_record_of_top_distance.R")
source("R/func_create_map.R")

loaded_data <- load_ship_data()

filtered_df <- filter_vessel_name(loaded_data,
                                  "KERLI - 3338")

# Define UI for application that draws a histogram
ui <- semanticPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

        # Show a plot of the generated distribution
        mainPanel(
          segment(
            class = "basic",
            tags$a(class = "ui blue right ribbon label right",
                   href = "https://www.marinetraffic.com/blog/information-transmitted-via-ais-signal/",
                   "Find my ship"),
            leafletOutput("reactive_leaflet_map")
          )
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  reactive_df <- reactive(filtered_df)                 
  
  # create map ---------------------------------------------
  output$reactive_leaflet_map <- renderLeaflet({
    my_map <- create_distance_map_between_a_n_b(reactive_df())
    my_map
  })
  
  observe({
    print(head(reactive_df()))
    
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
