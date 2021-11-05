
library(testthat)

test_that("2 records relating to max distance are returned", {
  
  loaded_data <- Demo_SemanticMarineShips::load_ship_data()
  
  filtered_df <- Demo_SemanticMarineShips::filter_vessel_name(loaded_data, "KERLI - 3338")

  top_distance_df <-   filtered_df %>%
    Demo_SemanticMarineShips::transform_to_spatial_df(.) %>%
    Demo_SemanticMarineShips::calc_circle_distance(.) %>%
    #how to protect against same max distance?
    Demo_SemanticMarineShips::get_max_distance(.)
  
  recorded_df <- Demo_SemanticMarineShips::obtain_record_of_top_distance(filtered_df,
                                               top_distance_df)
  
  expect_equal(ncol(recorded_df), 11)
  expect_equal(nrow(recorded_df), 2)
  expect_true((colnames(recorded_df)[9] == "LAT" & 
                 colnames(recorded_df)[10] == "LON"))
  
})
