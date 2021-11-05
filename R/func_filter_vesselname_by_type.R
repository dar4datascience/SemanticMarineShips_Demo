#' filter_vessel_type
#' @description function to filter vessel names n typs
#' by type. used for dynamic filtering in dropdown module
#' @import dplyr
#' @export
filter_vessel_type <- function(df, vessel_type){
  
  filtered_df <- df %>% 
    filter(.data[['ShipType']] == vessel_type)
  
  return(filtered_df)
}
