source("R/func_load_ship_nameS_n_types.R")
test_that("ship names and types is loaded correctly", {

  ship_names_n_types <- load_ship_names_n_types()
  
  expect_type(ship_names_n_types, 'list')
  expect_true(class(ship_names_n_types)[1] == "data.table")
  expect_equal(ncol(ship_names_n_types), 2)
  expect_equal(nrow(ship_names_n_types), 1226)
  
  
  
})
