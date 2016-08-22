##' Reformat climate input data into separate lists
##' 
##' Make separate lists for each parameter in the climate data input, and make
##' them accessible by the parameter names. The single lists correspond to the
##' scheme years in rows, observations from previous december to current
##' december in columns.
##' @title Make list of parameter matrices from climate input data
##' @param x climate data as returned from as_tcclimate
##' @param pad how many previous years should be padded with NAs? This is useful when
##'   we want to use the full range of available data when all predictors derive
##'   from the current year (pad = 2, or current and previous, for pad = 1)
##' @return a list of matrices
##' @keywords manip internal
make_pmat <- function(x, pad = 0) {
  if (any(names(x) == "year")) {
    years <- x$year
  } else {
    years <- x[,1]
  }
  n <- length(unique(years))
  no.vars <- dim(x)[2] - 2
  .names <- names(x)[-c(1:2)]

  ## create a full matrix from -13 to 12 for every parameter, names are
  ## not needed for this step

  m <- list()
  
  if (pad == 0) {
    npad <- n 
    padyears <- unique(years)
  } else {
    npad <- n + pad
    padyears <- c((min(years) - pad):min(years), unique(years)[-1])
    years <- x[,1]
    for (p in 1:pad) {
      x <- rbind(x[1:12,], x)
      x[1:12, 1] <- x[1:12, 1] - pad
      x[1:12, 3:(2 + no.vars)] <- NA
      years <- c(rep(min(years) - 1, 12), years)
    }
    x$year <- years
  }

  for (k in 1:no.vars) {
    m_k <- matrix(NA, nrow = 36, ncol = npad - 2)
    colnames(m_k) <- padyears[-c(1:2)]
    for (i in 3:npad) { # loop through years
      start_with <- which(years == padyears[i - 2])[1] # january of year before previous
      for (j in 1:36) {                 # loop through months
        m_k[j, (i - 2)] <- x[(start_with + j - 1), 2 + k]
      }
    }
    m[[k]] <- t(m_k)
  }

  names(m) <- .names
  class(m) <- list("tc_pmat", "list")
  m
}
