# load libraries ----
library(shinyWidgets)
library(shiny)
library(dplyr)
library(shiny.semantic)
source("R/func_filter_vesselname_by_type.R")
source("R/func_load_ship_names_n_types.R")


# gridTemplate ------------------------------------------------------------

grid_leaftlet <- grid_template(default = list(
  areas = rbind(
    c(
      "leaftlet_map"
    )
  ),
  rows_height = c("100%"),
  cols_width = c("100%")
))

# UI starts here ----------------------------------------------------------

leaftlet_map__UI <- function(id) {
  ns <- NS(id)
  tagList(
    # declare grid ------------------------------------------------------------
    grid(
      grid_leaftlet,
      # Leaftlet map ----------------------------------------------------
      leaftlet_map = dropdown_input(
        input_id = ns("vesselType"),
        choices = vessel_types,
        value = "Cargo - 7",
        default_text = "Select Vessel Type",
        type = "search selection large"
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
#'   \item{vesselSelect}{reactive character string indicating vessel name selecter by user}
#' }
leaftlet_map_server <- function(id, filtered_df, vessel_name) {
  moduleServer(id,
               function(input, output, session, filtered_df, vessel_name) {
                 filtered_ship_names <- reactiveValues()
                 
                 # observe change of vessel type -------------------------------------------
                 
                 
                 observeEvent(input$vesselType, {
                   filtered_ship_names$data <- filter_vessel_type(ship_names_n_types,
                                                                  input$vesselType)
                   # print(filtered_ship_names$data %>% head())
                   
                   
                   # update vessel name dropdown ---------------------------------------------
                   
                   update_dropdown_input(session,
                                         "vesselSelect",
                                         choices = filtered_ship_names$data$ShipName)
                 }) #end of observe event
                 
                 
                 # return value -----------------------------------------------------------
                 
                 return(
                   list(
                     vesselName = reactive({ input$vesselSelect })
                   )
                 )
               })
}


dropdowns_demo <- function() {
  ui <- semanticPage(
    ship_dropdowns_UI("conditional_dropdowns")
  )
  
  server <- function(input, output, session) {
    ship_dropdowns_server("conditional_dropdowns")
    
  }
  #run test app
  shinyApp(ui, server)
  
}
