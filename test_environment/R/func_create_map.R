library(sf)
library(leaflet)
create_distance_map_between_a_n_b <- function(filtered_df) {
  
  if (nrow(filtered_df %>% distinct(ShipName)) > 1) {
    
    print("this df has more than 1 ship name. please provide a df with only 1 distinct ship name")
    
  }else{
  
  # perform distance calculations -------------------------------------------
  
  top_distance_df <- filtered_df %>%
    transform_to_spatial_df(.) %>%
    calc_circle_distance(.) %>%
    #how to protect against same max distance?
    get_max_distance(.)
  
  
  # obtain the records associated to max distance ------------------------------------------------------------------
  
  top_record_df_distance <-
    obtain_record_of_top_distance(filtered_df,
                                  top_distance_df)
  

# add point reference -----------------------------------------------------
  
  plot_df <- top_record_df_distance %>% 
    arrange(DATETIME) %>% 
    mutate(
      position = c("Point A", "Point B")
    )



# plot map ----------------------------------------------------------------

 map_leaftlet <-  leaflet(data = plot_df) %>%
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
                                prettyNum(top_distance_df$distance_travelled,
                                          big.mark = ","),
                                "meters")
    )
 
 return(map_leaftlet)
  
  }
  
}
