#' @description the module has 3 selectors when the 
#' action button is pressed a calculation starts that
#' filters the lazy ship df. The result is returned
#' as a tibble for calculations and mapping

# load libraries ----
library(shinyWidgets)
library(shiny)
library(dplyr)
library(dtplyr)
library(shiny.semantic)
source("R/func_filter_vesselname_by_type.R")
source("R/func_load_ship_names_n_types.R")

# data prep ---------------------------------------------------------------

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
      "vesselname_selector",
      "activate_search_button"
    )
  ),
  rows_height = c("100%"),
  cols_width = c("40%", "40%", "20%")
))

# UI starts here ----------------------------------------------------------

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
        type = "search selection large"
      ),
      
      # Dropdown select vessel --------------------------------------------------
      vesselname_selector = dropdown_input(
        input_id = ns('vesselSelect'),
        choices = vessel_names,
        value = "KAROLI - 2764",
        default_text = 'Select a Vessel',
        type = "search selection large"
      ),
      
      # Activate logic button ---------------------------------------------------
      activate_search_button = button(ns("search4vessel"),
                                      "Find Vessel",
                                      icon = icon("ship"))
    ) # end of grid
  ) # end of tagslists
}


#' ship_dropdowns_server 
#'
#' @param input, output, session standard \code{shiny} boilerplate
#'
#' @return list with following components
#' \describe{
#'   \item{filtered_df}{reactive tibble used for performing calculation the create_distance_map_between_a_n_b function}
#' }
ship_dropdowns_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 filtered_ship_names <- reactiveValues()

# observe change of vessel type -------------------------------------------

              
                 observeEvent(input$vesselType, {
                   filtered_ship_names$data <- filter_vessel_type(ship_names_n_types,
                                                                  input$vesselType)
                    #print(filtered_ship_names$data %>% head())
                   

# update vessel name dropdown ---------------------------------------------

                   update_dropdown_input(session,
                                         "vesselSelect",
                                         choices = filtered_ship_names$data$ShipName)
                 }) #end of observe event
                 

# print selected vessel name to see on console ----------------------------

observe({
  print(input$vesselSelect)
})

 # Dataframe filtering -----------------------------------------------------
                 
                 vessel_df <- reactiveValues()
                 
                 observeEvent(input$search4vessel,
                              {
                                vessel_df$data <- filter_vessel_name(lazy_ship_data,
                                                                     isolate(
                                                                       input$vesselSelect
                                                                     )
                                )
                                
                                
                              })
                 
                 
                 observeEvent(vessel_df$data,{
                   print(vessel_df$data %>% head())
                 })


# return filtered df ------------------------------------------------------

                 
                 return(
                   list(
                     filtered_df = reactive({ vessel_df$data })
                   )
                 )
                 
               })
}


dropdowns_demo <- function() {

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
