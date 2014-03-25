##' Reformat climate input data into separate lists
##' 
##' Make separate lists for each parameter in the climate data input,
##' and make them accessible by the parameter names. The single lists
##' correspond to the scheme years in rows, observations from previous
##' december to current december in columns.
##' @title Make list of parameter matrices from climate input data
##' @param x climate data as returned from as_tcclimate
##' @return a list of matrices
##' @keywords manip internal
make_pmat <- function(x) {
  if (any(names(x) == "year")) {
    years <- x$year
  } else {
    years <- x[,1]
  }
  n <- length(unique(years))
  no.vars <- dim(x)[2] - 2
  .names <- names(x)[-c(1:2)]

  ## create a full matrix from -1 to 12 for every parameter, names are
  ## not needed for this step

  m <- list()

  for (k in 1:no.vars) {
    m_k <- matrix(NA, nrow = 24, ncol = n - 1)
    colnames(m_k) <- unique(years)[-1]
    for (i in 2:n) {
      start_with <- which(years == unique(years)[i - 1])[1] # previous january
      for (j in 1:24) {                 # loop through months
        m_k[j, (i - 1)] <- x[(start_with + j - 1), 2+k]
      }
    }
    m[[k]] <- t(m_k)
  }

  names(m) <- .names
  class(m) <- list("ctpmat", "list")
  m
}
