#' Dropdown module 4 filtering vessels
#' @description the module has 2 selectors and loads the needed 
#' data for the filtering of the big data frame




# HERE IS WHERE DATA IS LOADED ---------------------------------------------------------------

#load ship names n types

ship_names_n_types <- load_ship_names_n_types() %>% as_tibble()

# load ship data
ship_data <- load_ship_data()

lazy_ship_data <- dtplyr::lazy_dt(ship_data)

# Declare selectors

## Vessel types
vessel_types <- ship_names_n_types$ShipType %>% unique()

## Vessel Names
vessel_names <- ship_names_n_types$ShipName

# gridTemplate ------------------------------------------------------------

grid_selectors <- grid_template(default = list(
  areas = rbind(
    c(
      "vesseltype_selector",
      "vesselname_selector"
    )
  ),
  rows_height = c("100%"),
  cols_width = c("50%", "50%")
))

# UI starts here ----------------------------------------------------------

#' ship_dropdowns_UI
#' @description UI taglists for dynamic selectors
#' @import shiny
#' @export
ship_dropdowns_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("Select the Ship you wish to know its maximum consecutive distance:"),
    
    # declare grid ------------------------------------------------------------
    grid(
      grid_selectors,
      # Dropdown vessel type ----------------------------------------------------
      vesseltype_selector = dropdown_input(
        input_id = ns("vesselType"),
        choices = vessel_types,
        value = "Cargo - 7",
        default_text = "Select Vessel Type",
        type = "search selection big"
      ),
      
      # Dropdown select vessel --------------------------------------------------
      vesselname_selector = dropdown_input(
        input_id = ns('vesselSelect'),
        choices = vessel_names,
        value = "KAROLI - 2764",
        default_text = 'Select a Vessel',
        type = "search selection big"
      )
      ) # end of grid
  ) # end of tagslists
}


#' ship_dropdowns_server 
#'
#' @param input, output, session standard \code{shiny} boilerplate
#'
#' @return list with following components
#' \describe{
#'   \item{my_filtered_df}{reactive tibble used for performing calculation the create_distance_map_between_a_n_b function}
#'   \item{vessel_name}{reactive character of vessel selected}
#' }
ship_dropdowns_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 filtered_ship_names <- reactiveValues()

# observe change of vessel type -------------------------------------------

              
                 observeEvent(input$vesselType, {
                   filtered_ship_names$data <- Demo_SemanticMarineShips::filter_vessel_type(ship_names_n_types,
                                                                  input$vesselType)
                    #print(filtered_ship_names$data %>% head())
                   

# update vessel name dropdown ---------------------------------------------

                   update_dropdown_input(session,
                                         "vesselSelect",
                                         choices = filtered_ship_names$data$ShipName)
                 }) #end of observe event
                 

# print selected vessel name to see on console ----------------------------

#observe({
 # print(input$vesselSelect)
#})

 # Dataframe filtering -----------------------------------------------------
                 
                 vessel_df <- reactiveValues()
                 
                 observeEvent(c(input$vesselType, input$vesselSelect),
                              {
                                vessel_df$data <- Demo_SemanticMarineShips::filter_vessel_name(lazy_ship_data,
                                                                       input$vesselSelect
                                                                      ) #end of function
                                
                                
                                #print(vessel_df$data %>% head())
                                
                                
                              })
                 
                 

# return filtered df ------------------------------------------------------

                 
                 return(
                   list(
                     my_filtered_data = reactive({vessel_df$data}),
                     vessel_name = reactive({input$vesselSelect})
                   )
                 )
                 
               })
}


dropdowns_demo <- function() {
  # load libraries ----
  
  library(shiny)
  library(dplyr)
  library(dtplyr)
  library(shiny.semantic)
# app body ----------------------------------------------------------------


  ui <- semanticPage(
    ship_dropdowns_UI("conditional_dropdowns")
    )
  
  server <- function(input, output, session) {
    ship_dropdowns_server("conditional_dropdowns")
   
  }
  #run test app
  shinyApp(ui, server)
  
}
