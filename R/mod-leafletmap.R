# load libraries ----

library(shiny)
library(sf)
library(dtplyr)
library(dplyr)
library(leaflet)
library(shiny.semantic)



# gridTemplate ------------------------------------------------------------

grid_leaftlet <- grid_template(default = list(
  areas = rbind(c("leaftlet_map")),
  rows_height = c("100%"),
  cols_width = c("100%")
))

# UI starts here ----------------------------------------------------------

leaftlet_map__UI <- function(id) {
  ns <- NS(id)
  tagList(# declare grid ------------------------------------------------------------
          grid(
            grid_leaftlet,
            # Leaftlet map ----------------------------------------------------
            leaftlet_map = segment(
              class = "basic",
              tags$a(class = "ui blue right ribbon label right",
                     href = "https://www.marinetraffic.com/blog/information-transmitted-via-ais-signal/",
                     "Find my ship"),
              leaflet::leafletOutput("reactive_leaflet_map")
            )
          ) # end of grid
        ) # end of tagslists
}


#' ship_dropdowns_server
#'
#' @param input, output, session standard \code{shiny} boilerplate
#' @param filtered_df data frame (reactive) with variables necessary for leaflet map
#' @return list with following components
#' \describe{
#'   \item{leaflet_map}{reactive leaflet map to be displayed}
#' }
leaftlet_map_server <- function(id, filtered_df) {
  moduleServer(id,
               function(input, output, session) {
                 # create map ---------------------------------------------
output$reactive_leaflet_map <- renderLeaflet({
                   my_map <- create_distance_map_between_a_n_b(filtered_df$data())
                   my_map
                 })
                 
                 observe({
                   print(head(filtered_df$data()))
                   
                 }
                 )


               })
}


leaflet_map_demo <- function() {

  
  loaded_data <- load_ship_data()
  
  filtered_df <- filter_vessel_name(loaded_data,
                                    "KERLI - 3338")
 
  
  ui <- semanticPage(leaftlet_map__UI("distance_map"))
  
  server <- function(input, output, session) {

    #simulate mod dropdown return value

    leaftlet_map_server("distance_map",
                        filtered_df =  list(data = reactive({filtered_df})))
    
  }
  #run test app
  shinyApp(ui, server)
  
}
