#' @importFrom data.table fread
#' @export
load_ship_data <- function(){
  
  df_ships <- data.table::fread(
    here::here("data", "clean_ships.csv"),
    encoding = "UTF-8",
    sep = ",",
    header = TRUE
  )
  
  return(df_ships)
  
}
