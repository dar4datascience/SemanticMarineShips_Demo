source("r/func_load_data.R")
source("r/func_filter_vessel_name.R")
source("r/func_2_spatial_df.R")
source("r/func_2_calc_distances.R")
source("r/func_get_max_distance.R")
source("r/func_get_record_of_top_distance.R")
library(testthat)

test_that("2 records relating to max distance are returned", {
  
  loaded_data <- load_ship_data()
  
  filtered_df <- filter_vessel_name(loaded_data, "KERLI - 3338")

  top_distance_df <-   filtered_df %>%
    transform_to_spatial_df(.) %>%
    calc_circle_distance(.) %>%
    #how to protect against same max distance?
    get_max_distance(.)
  
  recorded_df <- obtain_record_of_top_distance(filtered_df,
                                               top_distance_df)
  
  expect_equal(ncol(recorded_df), 11)
  expect_equal(nrow(recorded_df), 2)
  expect_true((colnames(recorded_df)[9] == "LAT" & 
                 colnames(recorded_df)[10] == "LON"))
  
})
