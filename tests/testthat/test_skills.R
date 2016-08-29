context("Selection of calibration/verification periods for skills")

test_that("skills correctly splits timespan", {
  dc1 <- dcc(muc_spruce, muc_clim, 4:8)
  
  sk1 <- skills(dc1, .mean(6:9, "temp"))
  expect_equal(sk1$cal.years, 1978:2007)
  expect_equal(sk1$ver.years, 1949:1977)
  
  sk2 <- skills(dc1, .mean(6:9, "temp"), calibration = 1960:1990)
  expect_equal(sk2$cal.years, 1960:1990)
  expect_equal(sk2$ver.years, c(1949:1959, 1991:2007))
  
  sk3 <- skills(dc1, .mean(6:9, "temp"), calibration = 30)
  expect_equal(sk3$cal.years, tail(c(1949:2007), 30))
  expect_equal(sk3$ver.years, c(1949:2007)[1:29])
  
  sk4 <- skills(dc1, .mean(6:9, "temp"), calibration = -30)
  expect_equal(sk4$cal.years, head(c(1949:2007), 30))
  expect_equal(sk4$ver.years, c(1949:2007)[31:59])
  
  sk5 <- skills(dc1, .mean(6:9, "temp"), calibration = "30%")
  expect_equal(sk5$cal.years, tail(c(1949:2007), 18))
  expect_equal(sk5$ver.years, c(1949:2007)[1:41])
  
  sk6 <- skills(dc1, .mean(6:9, "temp"), calibration = "-30%")
  expect_equal(sk6$cal.years, head(c(1949:2007), 18))
  expect_equal(sk6$ver.years, c(1949:2007)[19:59])
  
  sk7 <- skills(dc1, .mean(6:9, "temp"), calibration = "-30%", timespan = c(1955, 2004))
  expect_equal(sk7$cal.years, head(c(1955:2004), 15))
  expect_equal(sk7$ver.years, c(1955:2004)[16:50])
})