check_ci <- function(ci) {
  if (!any(ci == c(0.1, 0.05, 0.01)))
    stop("'ci' has to be one of [0.1, 0.05, 0.01].")
}