##' Report autocorrelation structure
##'
##' Print the autocorrelation structure of the original proxy data
##' series and its bootstrapped samples.
##' @param x an object of class 'tc_dcc'
##' @return nothing, invoked for side effects (reporting
##' autocorrelation structure)
##' @export
ac <- function(x) {
  if (!any(class(x) == "tc_dcc"))
    stop("Please provide an object of class 'tc_dcc'.")
  x <- x$ac
  if (is.null(x))
    stop("Please run 'dcc' with the option 'check_ac' set to TRUE.")
  if (is.list(x$ac0)) {
    ## moving case
    cat("Autocorrelation of original proxy data:\nFirst order:\n")
    print.default(format(x$ac0$ac01), print.gap = 2L,
                  quote = FALSE)
    cat("\nSecond order:\n")
    print.default(format(x$ac0$ac02), print.gap = 2L,
                  quote = FALSE)
  } else {
    ## static case 
    cat("Autocorrelation of original proxy data:\nFirst order:\n")
    cat(round(x$ac0[1], 3))
    cat("\nSecond order:\n")
    cat(round(x$ac0[2], 3), "\n")
  }
  cat("\nAutocorrelation of bootstrap samples:\nFirst order:\n")
  print.default(format(x$acb$acb1), print.gap = 2L,
                quote = FALSE)
  cat("Second order:\n")
  print.default(format(x$acb$acb2), print.gap = 2L,
                quote = FALSE)
}
