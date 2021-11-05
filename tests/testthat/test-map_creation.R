library(testthat)
library(sf)
library(dplyr)

test_that("function returns a map", {
  
  loaded_data <- Demo_SemanticMarineShips::load_ship_data()
  
  filtered_df <- Demo_SemanticMarineShips::filter_vessel_name(loaded_data, "KERLI - 3338")
  
  map_test <- Demo_SemanticMarineShips::create_distance_map_between_a_n_b(filtered_df)
  
  print(class(map_test))
  expect_type(map_test, "list")
  expect_true(class(map_test)[1] == "leaflet" &
                class(map_test)[2] == "htmlwidget")
})
