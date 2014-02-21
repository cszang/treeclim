##' initialize data structure for bootstrapping
##'
##' this will create an array for the climate data and corresponding
##' matrix for the tree-ring data, holding the randomly resampled data
##' for bootstrapping
##' @param u climate data (matrix with parameters in columns and years
##' in rows)
##' @param g vector with tree-ring data
##' @param n times for resampling
##' @param bootmethod one of c("std", "exact")
##' @return a list
##' @keywords internal 
init_boot_data <- function(u, g, n, bootmethod) {
  m <- length(g)
  k <- dim(u)[2]
  out_u <- array(dim = c(m, k, n))
  out_g <- matrix(nrow = m, ncol = n)
  if (bootmethod == "std") {
    for (i in 1:n) {
      .sample <- sample(1:m, m, replace = TRUE)
      out_u[, ,i] <- u[.sample, ]
      out_g[, i] <- g[.sample]
    }
  }
  list(climate = out_u, chrono = out_g)
}
