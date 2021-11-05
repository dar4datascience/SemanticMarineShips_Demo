#transform data to spatial df to perform operations
transform_to_spatial_df <- function(df) {
  if (class(df)[1] != "tbl_df") {
    print("object is not of class tbl_df. please make your df a tibble")
    
  } else{
    spatial_df <- df %>%
      select(DATETIME, .data[['LAT']], .data[['LON']]) %>%
      sf::st_as_sf(.,
               coords = c("LON", "LAT"),
               crs = 4326) %>%
      arrange(DATETIME)
    
    return(spatial_df)
  }
}
