##' deconstruct call to modifier into list
##' 
##' deconstructs the call to a modifier into a list for further
##' processing with the list-based parameter selection functions
##' @keywords internal
make_range_list <- function(call, method) {
  par_months <- eval(call$.months)
  par_variables <- eval(call$.variables)
  if (is.null(par_months)) {
    par_months <- -6:9
  }
  if (is.null(par_variables)) {
    selection <- list(method, par_months)
  } else {
    selection <- list(method, par_months, par_variables)
  }
  class(selection) <- c("tc_paramlist", "list")
  selection
}

##' Modifiers for climate parameter selection
##' 
##' These modifiers are used to select specific months from specific
##' climate parameters, and potentially transform the selections into
##' their respective sums or means. The modifiers can be chained
##' together using '+'.

##' \code{.range} corresponds to using all specified months
##' separately, while \code{.sum} and \code{.mean} will use the sums
##' and means of the specified months. These modifiers also allow to
##' select specific climatic variables, addressed by name. Thus,
##' \code{.mean(4:8, "temp")} will select the mean for climate
##' parameter "temp" for the months April to August. Not only ranges,
##' but also individual vectors can be used for month specification,
##' like e.g., \code{.range(c(1, 3, 4, 5)}.
##'   
##' The modifiers can be chained together using the '+' symbol, which
##' makes it possible to create arbitrarily complex selections of
##' climate parameters for calibration.  E.g., \code{.mean(2:5,
##' "temp") + .sum(2:5, "prec")} will yield the February-to-May mean
##' for the variable "temp" and the sum of the variable "prec" for the
##' same time. While there is no limitation for number of lists that
##' can be chained together, 'dcc' will not check for meaningful
##' specifications. Testing smart hypotheses is up the researcher.
##'
##' @name treeclim-modifiers
##' @rdname treeclim-modifiers
##' @param .months numeric identifiers for the months (-1 for previous
##'   January until 12 for current December, with -6 for previous
##'   June, etc.)
##' @param .variables names of the variables the modifier shall be
##'   applied to
##' @examples
##' \dontrun{
##' my_calib <- dcc(rt_spruce, list(prec = rt_prec, temp = rt_temp),
##'   .range(4:9, "temp") + .mean(-5:-9, "temp") + .sum(3:10, "prec"))
##' plot(my_calib)
##' }
NULL

##' @rdname treeclim-modifiers
##' @export
.range <- function(.months = NULL, .variables = NULL) {
  make_range_list(match.call(), "full")
}

##' @rdname treeclim-modifiers
##' @export
.mean <- function(.months = NULL, .variables = NULL) {
  make_range_list(match.call(), "mean")
}

##' @rdname treeclim-modifiers
##' @export
.sum <- function(.months = NULL, .variables = NULL) {
  make_range_list(match.call(), "sum")
}
