context("Month processing")

test_that("continuity in month sequences is recognized correctly", {
  expect_that(is_continuous(1:10), is_true())
  expect_that(is_continuous(-1:10), is_true())
  expect_that(is_continuous(c(2, 4)), is_false())
  expect_that(is_continuous(-10:-2), is_true())
  expect_that(is_continuous(-2:10), is_true())
  expect_that(is_continuous(-22:10), is_true())
  expect_that(is_continuous(-22:-10), is_true())
})

test_that("the correct continuity in month sequences is generated", {
  expect_that(correct_continuous(1:10), equals(1:10))
  expect_that(correct_continuous(-1:10), equals(c(-1:-12, 1:10)))
  expect_that(correct_continuous(-1:-9), equals(c(-1:-9)))
  expect_that(correct_continuous(-12:2), equals(c(-12, 1:2)))
  expect_that(correct_continuous(-21:2), equals(c(-21:-24, -1:-12, 1:2)))
})

test_that("’check_months’ correctly identifies wrong month specs", {
  expect_that(check_months(-6:9)$check, is_true())
  expect_that(check_months(.mean(1:10))$check, is_true())
  expect_that(check_months(.mean(1:19))$check, is_false())
  expect_that(check_months(.mean(2:10, "temp"))$check, is_true())
  expect_that(check_months(.mean(2:10) + .sum(3:9))$check, is_true())
  expect_that(check_months(.mean(2:10) + .sum(-13:9))$check, is_true())
  expect_that(check_months(.mean(2:10) + .sum(-25:9))$check, is_false())
})

test_that("’check_months’ correctly returns earliest month", {
  expect_that(check_months(-6:9)$minmonth, equals(-1))
  expect_that(check_months(.mean(1:10))$minmonth, equals(1))
  expect_that(check_months(.mean(2:10, "temp"))$minmonth, equals(2))
  expect_that(check_months(.mean(2:10) + .sum(-3:-9))$minmonth, equals(-3))
  expect_that(check_months(.mean(2:10) + .sum(-15:-9))$minmonth, equals(-13))
})

test_that("months are correctly formatted", {
  expect_that(format_month(c(-3:3, 5:8)), throws_error("mix ranges"))
  expect_that(format_month(-25:12), throws_error("within january"))
  expect_that(format_month(1), equals(list(match = 25,
                                           names = "curr.jan",
                                           single = "JAN")))
  expect_that(format_month(-1), equals(list(match = 13,
                                            names = "prev.jan",
                                            single = "Jan")))
  expect_that(format_month(-6), equals(list(match = 18,
                                            names = "prev.jun",
                                            single = "Jun")))
  expect_that(format_month(6), equals(list(match = 30,
                                           names = "curr.jun",
                                           single = "JUN")))
  expect_that(format_month(-22), equals(list(match = 10,
                                           names = "bepr.oct",
                                           single = "oct")))
  expect_that(format_month(-2:2)$match, equals(14:26))
  expect_that(format_month(-12:2)$single, equals(c("Dec", "JAN", "FEB")))
  expect_that(format_month(-23:-2)$single, equals(c("nov", "dec", "Jan", "Feb")))
})

test_that("months are correctly shifted", {
  expect_that(shift_year(1), equals(1))
  expect_that(shift_year(5, -2), equals(-17))
  expect_that(.(1), equals(-1))
  expect_that(..(1), equals(-13))
  expect_that(..(5), equals(-17))
  expect_that(..(-19), equals(-19))
  expect_that(..(-25), throws_error("can only use months"))
  expect_error(dcc(muc_spruce, muc_clim, ..(12:-5)),
               "must be integer vector of length 1")
  expect_error(dcc(muc_spruce, muc_clim, ..(5.2)),
               "must be integer vector of length 1")
  expect_error(dcc(muc_spruce, muc_clim, ..(.mean(2:4))),
               "must be integer vector of length 1")
})
