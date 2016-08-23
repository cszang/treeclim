shift_year <- function(month, by = 0) {
  if (!(by %in% 0:-2)) {
    stop("`by` must be one of 0, -1, and -2")
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

. <- function(x) {
  shift_year(x, -1)
}

.. <- function(x) {
  shift_year(x, -2)
}
