context("Numerical accuracy of seascorr")

test_that("Numerical results are identical to benchmark version", {
  comp1 <- c(-0.0073210792957407, -0.122882799476871, -0.040510904750517, 
             -0.0811626223441039, -0.00287112147265235, -0.216711508332049, 
             0.0965622391342735, 0.194546770444512, -0.0586297958340763, 0.213082267988723, 
             -0.229822307076872, 0.289828779248469, -0.0464073967781443, -0.201352877557232)
  set.seed(42)
  scoef1 <- seascorr(muc_spruce, muc_clim)$coef[[1]]$primary$coef
  expect_equal(comp1, scoef1)
}
)