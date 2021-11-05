get_max_distance <- function(df_distances) {
  if (colnames(df_distances)[3] != "distance_travelled") {
    print("Column needed for operation missing. `distance travelled` is needed")
    
  } else{
    top_distance <- df_distances  %>%
      #this returns multiple values if the same max distance
      slice_max(order_by = distance_travelled) %>% 
      #HOW TO PROTECT AGAINST SAME DISTANCE AND GRAB THE MOST RECENT?
      arrange(desc(datetime_from)) %>% 
      slice_head(n = 1)
      
    
    return(top_distance)
  }
}
