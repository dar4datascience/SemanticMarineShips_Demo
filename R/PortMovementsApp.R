
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
        ship_dropdowns_UI("conditional_dropdowns")
      ), #end of selectors segment


# UI separator ------------------------------------------------------------

div(class = "ui horizontal divider",
    tags$i(class = "compass outline icon"), 
   "Port Map"),

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
    class = "three",
    card(class = "blue",
         div(class = "content",
             h2(class = "ui center aligned icon header",
                tags$i(class = "anchor icon"),
                div(class = "content",
                    "Port movements analyzer",
                    div(class = "sub header",
                        "Get to know your ships port activity."
                    ) # end of sub header div
                ) # end of content div
             )# end of h2
             ) # end of div content
         ), # end of card
    card(class = "red",
         div(class = "content",
             div(class = "header", "Vessel Report"),
             div(class = "meta",
                 textOutput("vesselname")),
             div(class = "description",
                 textOutput("ship_status"))
         )
    ),
    card(class = "blue",
         div(class = "content",
             div(class = "header", "Is the Vessel parked?"),
             div(class = "description", 
                 textOutput("parking_status")),
             div(class = "meta",
                 "More detail description card 2")
         )
    )
  )
) # end of cards segment


#leaftlet_map__UI("module_map"), #PLOT DOESNT RENDER EVEN THOUGH ALL COMPONENTS ARE OKAY
) # end of main panel
) # end of semantic

# Define server logic required to run marine app
server <- function(input, output) {

# call module servers -----------------------------------------------------

  dropdown_data <- ship_dropdowns_server("conditional_dropdowns")

     observe({
    print(dropdown_data$vessel_name())
   # print(head(dropdown_data$my_filtered_data()))
  })
 

# optional analytics ------------------------------------------------------

  top_distance_operations <- reactiveValues()
  
  observeEvent(dropdown_data$my_filtered_data(),{
    # perform distance calculations -------------------------------------------
    
    top_distance_operations$top_distance_df <- dropdown_data$my_filtered_data() %>%
      transform_to_spatial_df(.) %>%
      calc_circle_distance(.) %>%
      #how to protect against same max distance? SEE FUNCTION TO LEARN
      get_max_distance(.)
    
    
    print("top distance operations")
    print(top_distance_operations$top_distance_df)
    
    # obtain the records associated to max distance ------------------------------------------------------------------
    
    top_distance_operations$top_record_df_distance <-
      obtain_record_of_top_distance(dropdown_data$my_filtered_data(),
                                    top_distance_operations$top_distance_df)
    
    
    print("top distance df")
    print(top_distance_operations$top_record_df_distance)
    
    # add point reference -----------------------------------------------------
    
    top_distance_operations$plot_df <- top_distance_operations$top_record_df_distance %>% 
      arrange(DATETIME) %>% 
      mutate(
        position = c("Point A", "Point B")
      )
    
    
    print("plot df")
    print(top_distance_operations$plot_df)
    
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
  output$reactive_leaflet_map <- renderLeaflet({
    my_map <- top_distance_operations$map_leaftlet
    my_map
  })
  

# value boxes -------------------------------------------------------------

  output$vesselname <- renderText({
    as.character(dropdown_data$vessel_name())
  })
  
  #should have used glue but time was ticking
  output$ship_status <- renderText({
    paste(sep = "<br>",
          paste0("Vessel's flag: ", top_distance_operations$plot_df$FLAG[1]),
          paste0("Vessel's Name: ", top_distance_operations$plot_df$ShipName[1]),
          paste0("Distance travelled: ",top_distance_operations$top_record_df_distance$distance_travelled),
          paste0("Destination: ", top_distance_operations$plot_df$DESTINATION[2]),
          paste0("Port name: ", top_distance_operations$plot_df$port[2]),
          paste0("starting datetime: ", top_distance_operations$top_record_df_distance$datetime_from),
          paste0("ending datetime: ",top_distance_operations$top_record_df_distance$datetime_to),
          paste0("Move from coords: ",
                 "Lat: ", top_distance_operations$plot_df$LAT[1],
                 " & ",
                 "Lon: ", top_distance_operations$plot_df$LON[1]),
          paste0("To coords: ",
                 "Lat: ", top_distance_operations$plot_df$LAT[2],
                 " & ",
                 "Lon: ", top_distance_operations$plot_df$LON[2]),
          paste0("Speed: ", 
                 top_distance_operations$plot_df$SPEED[2], " knots")
          )
  })  
  
  output$parking_status <- renderText({
    paste(sep = "<br>",
          paste0("Is vessel parked? ",
                 tags$b(
                   if_else(top_distance_operations$plot_df$is_parked[1] == 1, "NO", "YES")
                   )
                 ),
    paste0("In which week of the year: ", top_distance_operations$plot_df$week_of_year[1])
    )
  })
  #output$output_map_module <- renderLeaflet({
   # my_map <- leaftlet_map_server("module_map",
    #                    dropdown_data = dropdown_data)
    #my_map
  #})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

}
