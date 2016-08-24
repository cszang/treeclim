#' (re)name climate variable
#'
#' @param climate an object of class tc_climate
#' @param var_names either NULL or a character vector
#'
#' @return the (potentially renamed) climate data
#'
#' @keywords internal
apply_var_names <- function(climate, var_names) {
  if (!is.null(var_names)) {
    varno <- dim(climate)[2] - 2
    if (length(var_names) != varno) {
      stop("Count of supplied variable names does not match count of variables in climate data.")
    } else {
      names(climate)[3:(dim(climate)[2])] <- var_names
    }
  }
  climate
}