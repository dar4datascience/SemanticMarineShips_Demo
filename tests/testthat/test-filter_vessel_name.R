library(testthat)

test_that("filtering of ship data works", {
  
  loaded_data <- load_ship_data()
  
  filtered_df <- Demo_SemanticMarineShips::filter_vessel_name(loaded_data, "KERLI - 3338")
  
  unique_ship_name <- filtered_df %>% distinct(ShipName)
  
  expect_true(unique_ship_name$ShipName == "KERLI - 3338")
  expect_true(class(filtered_df)[1] == "tbl_df")
  expect_equal(ncol(filtered_df),11)
})
