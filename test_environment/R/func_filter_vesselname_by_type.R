#' @import dplyr
#' @export
library(dplyr)

filter_vessel_type <- function(df, vessel_type){
  
  filtered_df <- df %>% 
    filter(.data[['ShipType']] == vessel_type)
  
  return(filtered_df)
}
