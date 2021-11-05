#function distace to use

func_distance <- function(from, to) {
  sf::st_distance(from, to, by_element = TRUE)
}

#' calc_circle_distance
#' @description a function that calculates the lead distances of vessel
#' observations. calculate a new tibble after applied function
#' @export
calc_circle_distance <- function(spatial_df) {
  
  if (class(spatial_df)[1] != "sf") {
    print("not an sf object. Lat and lon features must be encoded as geometries in an sf object.")
    
  } else{
    
    #grab the observation after the current one
    lead_df <- spatial_df %>% lead()
    
    df_distances <- tibble(
      datetime_from = spatial_df$DATETIME,
      datetime_to = lead_df$DATETIME,
      #calculate the distance between poit A from to point B to
      distance_travelled =  func_distance(from = spatial_df, to = lead_df)
    )
    
    return(df_distances)
  }
  
}
