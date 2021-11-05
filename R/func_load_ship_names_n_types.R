#' load_ship_names_n_types
#' @description load csv data for data selector dynamic filtering
#' @importFrom data.table fread
#' @export
load_ship_names_n_types <- function(){
  
  ship_names_n_types <- data.table::fread(
    here::here("data", "ship_types_n_names.csv"),
    encoding = "UTF-8",
    sep = ",",
    header = TRUE
  ) 
  
  return(ship_names_n_types)
  
}
