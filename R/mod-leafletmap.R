



# gridTemplate ------------------------------------------------------------

grid_leaftlet <- grid_template(default = list(
  areas = rbind(c("leaftlet_map")),
  rows_height = c("100%"),
  cols_width = c("100%")
))

# UI starts here ----------------------------------------------------------

#' leaflet_map_UI
#' @description UI leaflet map module for displaying a map 
#' of distances
#' @export
leaftlet_map__UI <- function(id) {
  ns <- NS(id)
  tagList(# declare grid ------------------------------------------------------------
          grid(
            grid_leaftlet,
           #  Leaftlet map ----------------------------------------------------
            leaftlet_map =
          segment(
             class = "basic",
              tags$a(class = "ui blue right ribbon label right",
                     href = "https://www.marinetraffic.com/blog/information-transmitted-via-ais-signal/",
                     "Find my ship"),
              leaflet::leafletOutput("module_leaflet") %>% 
                shinycssloaders::withSpinner(.,
                                             type = 1,
                                             color = "#A20EF2",
                                             size = 3)
            
            ) #end of segment
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
#' @export
leaftlet_map_server <- function(id, dropdown_data) {
  moduleServer(id,
               function(input, output, session) {
                 
                 top_distance_operations <- reactiveValues()
                 
                 observeEvent(dropdown_data$my_filtered_data(),{
                   # perform distance calculations -------------------------------------------
                   
                   top_distance_operations$top_distance_df <- dropdown_data$my_filtered_data() %>%
                     transform_to_spatial_df(.) %>%
                     calc_circle_distance(.) %>%
                     #how to protect against same max distance? SEE FUNCTION TO LEARN
                     get_max_distance(.)
                   
                   
                   #print("top distance operations")
                   #print(top_distance_operations$top_distance_df)
                   
                   # obtain the records associated to max distance ------------------------------------------------------------------
                   
                   top_distance_operations$top_record_df_distance <-
                     obtain_record_of_top_distance(dropdown_data$my_filtered_data(),
                                                   top_distance_operations$top_distance_df)
                   
                   
                   #print("top distance df")
                   #print(top_distance_operations$top_record_df_distance)
                   
                   # add point reference -----------------------------------------------------
                   
                   top_distance_operations$plot_df <- top_distance_operations$top_record_df_distance %>% 
                     arrange(DATETIME) %>% 
                     mutate(
                       position = c("Point A", "Point B")
                     )
                   
                   
                  # print("plot df")
                   #print(top_distance_operations$plot_df)
                   
                   # plot map ----------------------------------------------------------------
                   
                   top_distance_operations$map_leaftlet <-  leaflet(data = top_distance_operations$plot_df) %>%
                     addTiles() %>%
                     addMarkers(~LON, ~LAT,
                                #message to be displayed on click
                                popup = ~paste(sep = "<br/>",
                                               ShipName,
                                               ShipType,
                                               DATETIME,
                                               position),
                                #to display a text label either on hover or statically
                                label = ~as.character(position),
                                labelOptions = labelOptions(noHide = T,
                                                            textsize = "15px")) %>%
                     addMiniMap() %>% 
                     addPolylines(~LON, ~LAT,
                                  label = ~paste(sep = " ", "Distance travelled:",
                                                 prettyNum(top_distance_operations$top_distance_df$distance_travelled,
                                                           big.mark = ","),
                                                 "meters")
                     )
                   
                   print("leaflet map")
                   print(class(top_distance_operations$map_leaftlet))
                 })
                 
                 # create map ---------------------------------------------
                 output$module_leaflet <- renderLeaflet({
                   my_map <- top_distance_operations$map_leaftlet
                   my_map
                 })
                 
                 observe({
                   print(head(dropdown_data$my_filtered_data()))
                 })
                 
                 return(
                   list(
                     reactive_map = reactive({
                   output$module_leaflet
                 })))
                 
                 
               })
}


#' leaflet_map_demo
#' @import shiny
leaflet_map_demo <- function() {
  # Call tools needed for shinyapp.io -----------------------------------------------
  library(shiny)
  library(here) # path referenceer
  library(sf)
  library(waiter)
  library(shinycssloaders)
  library(dtplyr)
  library(dplyr)
  library(leaflet)
  library(shiny.semantic)
  
  
  ui <- semanticPage(
    main_panel( ship_dropdowns_UI("conditional_dropdowns"),
                     leaftlet_map__UI("distance_map")
    )
                     )
  
  server <- function(input, output, session) {
    
    dropdown_data <- ship_dropdowns_server("conditional_dropdowns")
    
    #simulate mod dropdown return value
    leaftlet_map_server("distance_map",
                        dropdown_data = dropdown_data)
    
    #observe({
    #  print(dropdown_data$my_filtered_data())
     # })
    
  }
  #run test app
  shinyApp(ui, server)
  
}
