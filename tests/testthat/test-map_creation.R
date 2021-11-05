source("r/func_load_data.R")
source("r/func_filter_vessel_name.R")
source("r/func_2_spatial_df.R")
source("r/func_2_calc_distances.R")
source("r/func_get_max_distance.R")
source("r/func_get_record_of_top_distance.R")
source("r/func_create_map.R")
library(testthat)
library(sf)
library(dplyr)

test_that("function returns a map", {
  
  loaded_data <- load_ship_data()
  
  filtered_df <- filter_vessel_name(loaded_data, "KERLI - 3338")
  
  map_test <- create_distance_map_between_a_n_b(filtered_df)
  
  print(class(map_test))
  expect_type(map_test, "list")
  expect_true(class(map_test)[1] == "leaflet" &
                class(map_test)[2] == "htmlwidget")
})
