source("R/func_load_data.R")
source("R/func_filter_vessel_name.R")
source("R/func_create_map.R")

test_that("function returns a map", {
  
  loaded_data <- load_ship_data()
  
  filtered_df <- filter_vessel_name(loaded_data, "KERLI - 3338")
  
  map_test <- create_distance_map_between_a_n_b(filtered_df)
  
  expect_type(map_test, "list")
  expect_true(class(map_test)[1] == "leafleft" &
                class(map_test)[2] == "htmlwidget")
})
