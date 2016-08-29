context("Aggregators")

test_that("’.range’ aggregator returns a correct ’tc_paramlist’", {
  expect_that(.range(), is_a("list"))
  expect_that(.range(), is_a("tc_paramlist"))
  expect_that(.range()[[1]], equals("full"))
  expect_that(.range()[[2]], equals(-6:9))
  
  expect_that(.range(1:12), is_a("list"))
  expect_that(.range(1:12), is_a("tc_paramlist"))
  expect_that(.range(1:12)[[1]], equals("full"))
  expect_that(.range(1:12)[[2]], equals(1:12))

  expect_that(.range("temp"), is_a("list"))
  expect_that(.range("temp"), is_a("tc_paramlist"))
  expect_that(.range("temp")[[1]], equals("full"))
  expect_that(.range("temp")[[2]], equals("temp"))
  
  expect_that(.range(1:12, "temp"), is_a("list"))
  expect_that(.range(1:12, "temp"), is_a("tc_paramlist"))
  expect_that(.range(1:12, "temp")[[1]], equals("full"))
  expect_that(.range(1:12, "temp")[[2]], equals(1:12))
  expect_that(.range(1:12, "temp")[[3]], equals("temp"))

  expect_that(.range(1:12, c("temp")), is_a("list"))
  expect_that(.range(1:12, c("temp")), is_a("tc_paramlist"))
  expect_that(.range(1:12, c("temp"))[[1]], equals("full"))
  expect_that(.range(1:12, c("temp"))[[2]], equals(1:12))
  expect_that(.range(1:12, c("temp"))[[3]], equals("temp"))

  expect_that(.range(1:12, c("temp", "prec")), is_a("list"))
  expect_that(.range(1:12, c("temp", "prec")), is_a("tc_paramlist"))
  expect_that(.range(1:12, c("temp", "prec"))[[1]], equals("full"))
  expect_that(.range(1:12, c("temp", "prec"))[[2]], equals(1:12))
  expect_that(.range(1:12, c("temp", "prec"))[[3]], equals(c("temp", "prec")))
})

test_that("’.mean’ and ’.sum’ aggregators also work correctly", {
  expect_that(.mean(-2:2), is_a("list"))
  expect_that(.mean(-2:2), is_a("tc_paramlist"))
  expect_that(.mean(-2:2)[[1]], equals("mean"))
  expect_that(.mean(-2:2)[[2]], equals(-2:2))
  expect_that(.mean(-2:2, "temp")[[2]], equals(-2:2))

  expect_that(.sum(-2:2), is_a("list"))
  expect_that(.sum(-2:2), is_a("tc_paramlist"))
  expect_that(.sum(-2:2)[[1]], equals("sum"))
  expect_that(.sum(-2:2)[[2]], equals(-2:2))
  expect_that(.sum(-2:2, "temp")[[2]], equals(-2:2))
})

test_that("’exfr’ correctly modifies a numeric vector", {
  expect_that(exfr(2), equals(2))
  expect_that(exfr(2:10), equals(c(2:10)))
  expect_that(exfr(2:10, 3), equals(c(2, 4:10)))
  expect_that(exfr(2:10, -3), equals(c(2:10)))
  expect_that(exfr(2:10, 3:4), equals(c(2, 5:10)))
  expect_that(exfr(-2:10, 4), equals(c(-2:-12, 1:3, 5:10)))
  expect_that(exfr(-2:10, 3:4), equals(c(-2:-12, 1:2, 5:10)))
  expect_that(exfr(-2:10, 4:3), equals(c(-2:-12, 1:2, 5:10)))
  expect_that(exfr(-2:-10, 3:4), equals(c(-2:-10)))
  expect_that(exfr(-2:10, -3:3), equals(c(-2, 4:10)))
  expect_that(exfr(-2:10, -3:-6), equals(c(-2, -7:-12, 1:10)))
  expect_that(exfr(-2:10, -6:-3), equals(c(-2, -7:-12, 1:10)))
  expect_equal(exfr(..(5):5, list(..(12):-3, ..(10):..(12), ..(10):..(12))), 
              c(-17L, -18L, -19L, -20L, -21L, -4L, -5L, -6L, -7L, -8L, -9L, 
                -10L, -11L, -12L, 1L, 2L, 3L, 4L, 5L))
  expect_equal(exfr(..(2):10, ..(10):2),
               c(-14L, -15L, -16L, -17L, -18L, -19L, -20L,
                 -21L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L))
  expect_equal(exfr(..(2):10, list(..(12):-3, .(5):6)),
               c(-14L, -15L, -16L, -17L, -18L, -19L, -20L,
                 -21L, -22L, -23L, -4L, 7L, 8L, 9L, 10L))
})  
