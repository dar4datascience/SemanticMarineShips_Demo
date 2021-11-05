source("R/func_load_data.R")
library(testthat)

test_that("loading ship df is correct and works", {
  
  loaded_data <- load_ship_data()
  
  expect_type(loaded_data, 'list')
  expect_true(class(loaded_data)[1] == "data.table" )
  expect_equal(ncol(loaded_data),11)
  expect_equal(nrow(loaded_data), 3102887)
})
