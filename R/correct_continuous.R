##' Correct continuous month specifications
##' 
##' If month specification is continuous, it must be interpreted differently if
##' it contains 0. E.g., -5:8 must be translated to c(-5:-12, 1:8).
##' @param x a numeric month id specification
##' @return a numeric vector with the corrected month id specification
##' @keywords manip internal
correct_continuous <- function(month) {
  ## if it is a continuous range through zero, it is interpreted
  ## differently
  if (length(month) > 1) {
    ## correct order for latest month in current year
    if (sign(month[1]) != sign(month[length(month)])) {
      ## zero is not allowed, so remove it
      month <- month[-which(month == 0)]
      ## earliest month in previous year
      if (min(month) >= -12) {
        month <- c(min(month):-12, 1:max(month))  
      } else { # earliest month in year before previous year
        complete_previous <- -1:-12
        before_previous <- min(month):-24
        current <- 1:max(month)
        month <- c(before_previous, complete_previous, current)
      }
    }
    ## correct order for latest month in previous year
    if (min(month) < -12 & max(month) < 0 & max(month) > -13) {
      before_previous <- min(month):-24
      previous <- -1:max(month[month > -13])
      month <- c(before_previous, previous)
    }
  }
  month
}
