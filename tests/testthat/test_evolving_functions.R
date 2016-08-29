context("Test evolving functions")

test_that("Evolving functions select years correctly for windows", {
  d1 <- dcc(muc_spruce, muc_clim, 4:9, win_size = 40, dynamic = "evolv")
  expect_equal(names(d1$coef$coef)[1], "1949-2007")
  expect_equal(tail(names(d1$coef$coef), 1), "1968-2007")
  
  d2 <- dcc(muc_spruce, muc_clim, 4:9,
            method = "corr", win_size = 40, dynamic = "evolv", start_last = FALSE)
  expect_equal(names(d2$coef$coef)[1], "1949-1988")
  expect_equal(tail(names(d2$coef$coef), 1), "1949-2007")
  
  d3 <- dcc(muc_spruce, muc_clim, -4:-9,
            method = "corr", win_size = 50, dynamic = "evolv",
            start_last = FALSE)
  expect_equal(names(d3$coef$coef)[1], "1950-1999")
  expect_equal(tail(names(d3$coef$coef), 1), "1950-2007")
  expect_error(dcc(muc_spruce, muc_clim, win_size = 90, dynamic = "evolv"),
               "Less than 2 windows")
})
