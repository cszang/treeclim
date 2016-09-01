##' shifts a month 0, 1, or 2 years back
##' 
##' there is no intuitive way to signify months from the year previous to the
##' last one...
##' @keywords internal
shift_year <- function(month, by = 0) {
  if (!(by %in% 0:-2)) {
    stop("`by` must be one of 0, -1, and -2")
  }
  
  month_msg <-
    "`month` to shift (argument to `.` and `..`) must be integer vector of length 1."
  
  if (!is.numeric(month)) {
    stop(month_msg)
  }
  
  if (length(month) != 1) {
    stop(month_msg)
  }

  if (!(floor(month) == month)) {
    stop(month_msg)
  }
  
  if (month > 0) {
    if (!(month %in% 1:12)) {
      stop("can only shift months from 1 to 12.")
    }
    if (by == 0) {
      shifted <- month
    }
    if (by == -1) {
      shifted <- -1 * month
    }
    if (by == -2) {
      shifted <- -1 * month - 12
    }
  } else {
    if (!(month %in% -1:-24)) {
      stop("can only use months from -24 to 12.")
    }
    shifted <- month
  }
  shifted
}

##' Shift months one or two years back
##' 
##' These functions are used to specify months from previous years for
##' the design matrix. \code{.(1)} is the same as \code{-1}, and
##' specifies previous year's January, whereas \code{..(1)} signifies
##' January of the year prior to the last year.
##' @name treeclim-shifters
##' @rdname treeclim-shifters
##' @param x numeric identifier (of length 1) for the months (1 for
##'   January, ..., 12 for December)
NULL

##' @rdname treeclim-shifters
##' @export
. <- function(x) {
  shift_year(x, -1)
}

##' @rdname treeclim-shifters
##' @export
.. <- function(x) {
  shift_year(x, -2)
}
