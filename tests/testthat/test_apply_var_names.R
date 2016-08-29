context("Variable names are correctly applied")

test_that("Variable names only changed when var_names is given", {
  
  clim1 <- muc_clim
  clim_names1 <- c("test1", "test2")
  clim_names2 <- c("test1", "test2", "test3")
  expect_equal(names(apply_var_names(clim1, NULL)), c("year", "month", "temp", "prec")) 
  expect_equal(names(apply_var_names(clim1, clim_names1)), c("year", "month", "test1", "test2")) 
  expect_error(names(apply_var_names(clim1, clim_names2)), "Count of supplied")
})