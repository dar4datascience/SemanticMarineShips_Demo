
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


#wrap to make a package

#' SemanticMarineShips
#' @description main function call that servers run_app.R
#' @export
SemanticMarineShips <- function(...) {

# Create useful variables -------------------------------------------------
#* gridTemplate ------------------------------------------------------------

grid_value_boxes <- grid_template(default = list(
  areas = rbind(c("port_and_destiny",
                  "Datetime_last_movement",
                  "parked_status")),
  rows_height = c("100%"),
  cols_width = c("40%", "40%", "20%")
))

# Define UI for application that draws a histogram
ui <- semanticPage(

# add loading elements ----------------------------------------------------
useWaiter(),
waiterPreloader(html = spin_gauge()),  
#autoWaiter(id = "reactive_leaflet_map",
 #          html = spin_terminal()),

  title = "Semantic Marine Ships",
  # main panel begins -------------------------------------------------------

    main_panel(

# row 1: begin of selectors segment ----------------------------------------------

      segment(
        div(class = "ui top left attached label",
            h2("Port movements analyzer")
            ),
        ship_dropdowns_UI("conditional_dropdowns")
      ), #end of selectors segment

# row 2: distance map -------------------------------------------------------------------

    segment(
    class = "basic",
   tags$a(class = "ui blue right ribbon label",
           href = "https://www.marinetraffic.com/blog/information-transmitted-via-ais-signal/",
           "Find my ship"),
    leaflet::leafletOutput("reactive_leaflet_map") %>% 
     shinycssloaders::withSpinner(.,
                                  type = 1,
                                  color = "#A20EF2",
                                  size = 3)
  ),

# row 3: value boxes ------------------------------------------------------
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
) # end of cards segment


#leaftlet_map__UI("module_map"), #PLOT DOESNT RENDER EVEN THOUGH ALL COMPONENTS ARE OKAY
) # end of main panel
) # end of semantic page

# Define server logic required to draw a histogram
server <- function(input, output) {

# call module servers -----------------------------------------------------

  dropdown_data <- ship_dropdowns_server("conditional_dropdowns")
  # create map ---------------------------------------------
  output$reactive_leaflet_map <- renderLeaflet({
    my_map <- create_distance_map_between_a_n_b(dropdown_data$my_filtered_data())
    my_map
  })
  
  #observe({
   # print(dropdown_data$vessel_name())
    #print(head(dropdown_data$my_filtered_data()))
#  })
  
  
  
  #output$output_map_module <- renderLeaflet({
   # my_map <- leaftlet_map_server("module_map",
    #                    dropdown_data = dropdown_data)
    #my_map
  #})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

}
