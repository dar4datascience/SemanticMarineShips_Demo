obtain_record_of_top_distance <- function(filtered_df, top_distance){
  
  if (colnames(top_distance)[1] != "datetime_from" &
          colnames(top_distance)[2] != "datetime_to") {
    
    print("columns datetime_from and datetime_to might be missing.")
    
  }else{
    
  record_top_distance_points <- tibble(
    filtered_df %>% 
      filter(DATETIME == top_distance$datetime_from |
               DATETIME == top_distance$datetime_to)
  ) 
  
  return(record_top_distance_points)
  }
}
