##' Deparse list structure from month specification and return parameter set
##'
##' Deparse list structure from month specification into single calls, call an
##' aggregating function to collect the data, and return a parameter set for
##' calibration. In general, the months must be specified as a list or nested
##' list structure. Possible cases are:
##' list()
##' list(...)
##' list(list(...))
##' list(list(...), list(...), ...)
##' @param selection the list structure to specify the month selection
##' @param climate the climate data as returned by make_pmat
##' @return a data.frame
br_design <- function(selection, climate) {
  ## is it a list?
  if (!is.list(selection)) {
    stop("Please supply information about independent variables as list.")
  }
  ## check for nested list structure
  n <- length(as.list(selection))
  if (n == 0) {
    # empty list, default parameters are taken
    out <- eval_selection(climate, selection)
  }
  if (n == 1) {
    ## could be list("mean") or list(list(...))
    if (is.list(selection[[1]])) {
      ## list(list(...))
      out <- eval_selection(climate, selection[[1]])
    } else {
      ## list("mean")
      out <- eval_selection(climate, selection)
    }
  }
  if (n > 1) {
    ## list(list(...), list(...), ...) or list("mean", 1:10)
    if (is.list(selection[[1]])) {
      ## everything has to be a list
      if (all(sapply(selection, is.list))) {
        ## list(list(...), list(...), ...)
        OUT <- list()
        for (i in 1:n) {
          OUT[[i]] <- eval_selection(climate, selection[[i]])
        }
        out <- list()
        out$month <- list()
        out$month$match <- sapply(OUT, function(x) {
          x$month$match })
        out$month$names <- sapply(OUT, function(x) {
          x$month$names})
        out$method <- sapply(OUT, function(x) {
          x$method })
        out$param <- sapply(OUT, function(x) {
          x$param })
        out$aggregate <- as.data.frame(lapply(OUT, function(x) {
          x$aggregate }))
        out$names <- unlist(as.vector(sapply(OUT, function(x) {
          x$names })))
      } else {
        stop("You may not mix modifiers/lists and other objects for parameter specifications.")
      }
    } else {
      ## everything has to be not a list
      if (all(sapply(selection, function(x) { ifelse(is.list(x), FALSE,
                                             TRUE)}))) {
        ## list("mean", 1:10)
        out <- eval_selection(climate, selection)
      } else {
        stop("You may not mix lists and other objects for parameter specifications.")
      }
    }
  }

  ## avoid duplicate parameters
  dupes <- duplicated(out$names)
  if (any(dupes)) {
    out$aggregate <- out$aggregate[,!dupes]
    out$names <- out$names[!dupes]
  }
  
  class(out) <- list("list", "br_design")
  out
}
