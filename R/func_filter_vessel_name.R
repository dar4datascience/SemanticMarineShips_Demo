#' @export
filter_vessel_name <- function(df, vessel_name){

filtered_df <- df %>% 
  filter(.data[['ShipName']] == vessel_name)

return(filtered_df)
}
