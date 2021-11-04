obtain_record_of_top_distance <- function(df, top_distance){
  
  if () {
    
    print("columns datetime_from and datetime_to might be missing.")
    
  }
  record_top_distance_points <- tibble(
    
    df %>% 
      filter(DATETIME == top_distance$datetime_from |
               DATETIME == top_distance$datetime_to)
  ) 
  
  return(record_top_distance_points)
}
