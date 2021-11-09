
# Call tools needed for shinyapp.io -----------------------------------------------
library(shiny)
library(here) # path referenceer
library(sf)
library(waiter)
library(shinycssloaders)
library(dtplyr)
library(dplyr)
library(stringr)
library(leaflet)
library(shiny.semantic)

options(semantic.themes = TRUE)

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
autoWaiter(id = c("vesselname",
                  "parking_status"
                  ),
           html = spin_refresh()
           ),  
#autoWaiter(id = "reactive_leaflet_map",
 #          html = spin_terminal()),

  title = "Semantic Marine Ships",
  theme = "cosmo", #doesnt change anything

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

# first value box ---------------------------------------------------------

    
    card(class = "red",
         div(class = "content",
             div(class = "header", "Vessel Report"),
             div(class = "meta",
                 textOutput("vesselname")),
             div(class = "description",
                 tags$p(verbatimTextOutput("vesselreport"))
                 )
         )
    ),

# second value box --------------------------------------------------------

    card(class = "blue",
         div(class = "content",
             div(class = "header", "Was the Vessel parked?"),
             div(class = "description", 
                 verbatimTextOutput("parkedreport")
                 
                 )
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
    print("filtered_data_changed")
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
    
    
    #print("plot df")
    #print(top_distance_operations$plot_df)

# glue reports ------------------------------------------------------------


    top_distance_operations$vesselreport <- stringr::str_glue("
                                 Vessel's flag: {flag}
                                 Max distance travelled: 
                                          {distancetravelled} mts
                                 Destination: {destination}
                                 Speed: {speed} knots
                                 Move from coords: Lat: {latfrom} 
                                 & Lon: {lonfrom} 
                                 Move to coords: Lat: {latto} 
                                 & Lon: {lonto} ",
                                 flag = top_distance_operations$plot_df$FLAG[1],
                                 destination = top_distance_operations$plot_df$DESTINATION[2],
                                 distancetravelled = top_distance_operations$top_distance_df[1],
                                 latfrom = top_distance_operations$plot_df$LAT[1],
                                 lonfrom = top_distance_operations$plot_df$LON[1],
                                 latto = top_distance_operations$plot_df$LAT[2],
                                 lonto = top_distance_operations$plot_df$LON[2],
                                 speed = top_distance_operations$plot_df$SPEED[2]
    )
    
    top_distance_operations$parkedreport <- stringr::str_glue("
                                 Was {shipname} parked? {parkingstatus}
                                 Port name: {port}
                                 Starting datetime: 
                                    {starttime} 
                                 Ending datetime: 
                                    {endingtime}
                                 In which week of the year?: {weekofyear}",
                                 port = top_distance_operations$plot_df$port[2],
                                 starttime = top_distance_operations$top_distance_df$datetime_from[1],
                                 endingtime = top_distance_operations$top_distance_df$datetime_to[1],
                                 shipname = dropdown_data$vessel_name(),
                                 parkingstatus = if_else(top_distance_operations$plot_df$is_parked[1] == 1, "NO", "YES"),
                                 weekofyear = top_distance_operations$plot_df$week_of_year[2]
                                 
    )
                                 
    
    print("description of vessel event")
    print(top_distance_operations$vesselreport)
    
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
                                  "meters"),
                   labelOptions = labelOptions(noHide = T,
                                               textsize = "15px")
      )
    
    #print("leaflet map")
    #print(class(top_distance_operations$map_leaftlet))
  }) 
  
  # create map ---------------------------------------------
  output$reactive_leaflet_map <- renderLeaflet({
    my_map <- top_distance_operations$map_leaftlet
    my_map
  })  %>%
    bindCache(input$vesselSelect, top_distance_operations$map_leaftlet)  %>% 
    bindEvent(top_distance_operations$map_leaftlet)
  
  

# value boxes -------------------------------------------------------------

  output$vesselname <- renderText({
    as.character(dropdown_data$vessel_name())
  })  %>%
    bindCache(dropdown_data$vessel_name())
  
  

# Dynamic texts -----------------------------------------------------------

  #* vessel report -------------------------------------------------------------
  
  
  output$vesselreport <- renderPrint({
    
    top_distance_operations$vesselreport
    
    
  })   %>%
    bindCache(input$vesselSelect, top_distance_operations$vesselreport)
 
  #* parking report -------------------------------------------------------------
  
  output$parkedreport <- renderPrint({
    
    top_distance_operations$parkedreport
    
    
  })   %>%
    bindCache(input$vesselSelect, top_distance_operations$parkedreport)
  
  
  
#* vessel flag -------------------------------------------------------------


  #should have used glue but time was ticking
  output$vesselflag <- renderText({
    
    paste0("Vessel's flag: ", top_distance_operations$plot_df$FLAG[1])
    
          
  })   %>%
    bindCache(input$vesselSelect, top_distance_operations$plot_df)
  

#* distance travelled ------------------------------------------------------

  output$distancetravelled <- renderText({
    
    paste0("Distance travelled: ",top_distance_operations$top_distance_df$distance_travelled,
           " mts")
    
    
  })   %>%
    bindCache(input$vesselSelect, top_distance_operations$top_distance_df)
  
  
#* destination ------------------------------------------------------------

  output$destination <- renderText({
    
    paste0("Destination: ", top_distance_operations$plot_df$DESTINATION[2])
    
    
  })   %>%
    bindCache(input$vesselSelect, top_distance_operations$plot_df)
  
  

#* port ------------------------------------------------------------------

  output$port <- renderText({
    
    paste0("Port name: ", top_distance_operations$plot_df$port[2])
    
    
  })   %>%
    bindCache(input$vesselSelect, top_distance_operations$plot_df)
  
  
  
  

#* Starting date -----------------------------------------------------------

  output$startdate <- renderText({
    
    paste0("starting datetime: ", top_distance_operations$top_distance_df$datetime_from)
    
    
  })   %>%
    bindCache(input$vesselSelect, top_distance_operations$top_distance_df)
  
  
  
#* ending date -------------------------------------------------------------

  output$endingdate <- renderText({
    
    paste0("ending datetime: ",top_distance_operations$top_distance_df$datetime_to)
    
    
  })   %>%
    bindCache(input$vesselSelect, top_distance_operations$top_distance_df)
  
  


#* parking status ----------------------------------------------------------

  
  output$parking_status <- renderText({
    
          paste0("Was ", dropdown_data$vessel_name(), " parked? ",
                 
                 if_else(top_distance_operations$plot_df$is_parked[1] == 1, "NO", "YES")
                 
                 )
    
  }) %>%
    bindCache(dropdown_data$vessel_name(), top_distance_operations$plot_df)

  

#* week of year ------------------------------------------------------------

  output$weekofyear <- renderText({
    
    paste0("In which week of the year?: ",
           top_distance_operations$plot_df$week_of_year[2]
    )     
    
    
  }) %>%
    bindCache(dropdown_data$vessel_name(), top_distance_operations$plot_df)
  

#* move from coords ----------------------------------------------------------

  output$movefromcoords <- renderText({
    
    paste0("Move from coords: ",
           "Lat: ", top_distance_operations$plot_df$LAT[1],
           " & ",
           "Lon: ", top_distance_operations$plot_df$LON[1])     
    
    
  }) %>%
    bindCache(dropdown_data$vessel_name(), top_distance_operations$plot_df)
  

#* move to coords ----------------------------------------------------------

  output$movetocoords <- renderText({
    
    paste0("To coords: ",
           "Lat: ", top_distance_operations$plot_df$LAT[2],
           " & ",
           "Lon: ", top_distance_operations$plot_df$LON[2])
    
    
  }) %>%
    bindCache(dropdown_data$vessel_name(), top_distance_operations$plot_df)
  
  

#* Speed -------------------------------------------------------------------

  output$speed <- renderText({
    
    paste0("Speed: ", 
           top_distance_operations$plot_df$SPEED[2], " knots")
    
    
  }) %>%
    bindCache(dropdown_data$vessel_name(), top_distance_operations$plot_df)
  
    
    
      
  #output$output_map_module <- renderLeaflet({
   # my_map <- leaftlet_map_server("module_map",
    #                    dropdown_data = dropdown_data)
    #my_map
  #})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

}
