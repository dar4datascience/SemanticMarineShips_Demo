# load libraries ----
library(shinyWidgets)
library(shiny)
library(dplyr)


ship_dropdowns_UI <- function(id) {
  ns <- NS(id)
  tagList(
      tags$h2("Ship Finder"),
      
# Dropdown vessel type ----------------------------------------------------


dropdown_input(
  input_id = ns("vesselType"),
               choices = LETTERS[1:5],
               value = "A",
               default_text = "Select Vessel Type",
               type = "search selection large"),

# Dropdown select vessel --------------------------------------------------

dropdown_input(
  input_id = ns('vesselSelect'),
  choices = LETTERS[1:5],
  value = "A",
                  default_text = 'Select a Vessel',
  type = "search selection large"),
      

# Activate logic button ---------------------------------------------------

      
      button(ns("search4vessel"),
                   "Find Vessel",
                   icon = icon("ship"))
  ) # end of tagslists
}

ship_dropdowns_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$simple_button, {
                update_dropdown_input(session, "simple_dropdown", value = "D")
             })
    }
  )
}


dropdowns_demo <- function() {
  
  df <- data.frame(day = 1:30, arr_delay = 1:30)
  
  ui <- fluidPage(ship_dropdowns_UI("x"))
  
  server <- function(input, output, session) {
    ship_dropdowns_server("x", reactive({df}), "arr_delay", 15)
  }
  shinyApp(ui, server)
  
}
