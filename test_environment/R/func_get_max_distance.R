get_max_distance <- function(df_distances) {
  if (colnames(df_distances)[3] != "distance_travelled") {
    print("Column needed for operation missing. `distance travelled` is needed")
    
  } else{
    top_distance <- df_distances  %>%
      #HOW TO PROTECT AGAINST SAME DISTANCE AND GRAB THE MOST RECENT?
      slice_max(order_by = distance_travelled)
    
    return(top_distance)
  }
}
