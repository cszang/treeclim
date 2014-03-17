##' TODO
##'
##' TODO
##' @param chrono \code{data.frame} containing a tree-ring
##' chronologies, e.g. as obtained by \code{chron} of package dplR.
##' @param climate either a \code{data.frame} or \code{matrix} with
##' climatic data in monthly resolution, with year, month and climate
##' parameters in columns (all columns except year and month will be
##' recognized as parameters for response or correlation function), or
##' a single \code{data.frame} or \code{matrix} in 13-column format
##' (see below), or list of several of the latter.
##' @param var_names \code{character} vector with variable
##' names. Defaults to corresponding column names of \code{data.frame}
##' clim.
##' @param timespan \code{integer} vector of length 2 specifying the
##' time interval (in years) to be considered for analysis. Defaults
##' to the maximum possible interval.
##' @param complete \code{numeric) month when tree-ring growth is
##' expected to have finished.
##' @param season_lengths \code{numeric} vector giving the lengths of
##' the seasons for variable grouping
##' @param primary position (numeric) or name (character) of primary
##' climate variable
##' @param secondary position (numeric) or name (character) of
##' secondary climate variable
##' @param ci \code{numerical} value to set the test level for
##' significance test (values 0.01, 0.05 and 0.1 are allowed); the
##' confidence intervals are adapted accordingly.
##' @return an object of class "br_seascorr"
##' @references
##' TODO
##' @examples
##' \dontrun{
##' TODO
##' }
##' @author Christian Zang; the procedure incl. exact bootstrapping
##' was implemented first by Dave Meko in MATLAB
##' @export
seascorr <- function(chrono, climate, var_names = NULL, timespan =
                     NULL, complete = 9, season_lengths = c(1, 3, 6),
                     primary = 1, secondary = 2, ci = 0.05) {

  ## check input
  if (!any(1:12 == complete))
    stop("`complete` must be an integer value between 1 and 12.")

  if (!all(sapply(season_lengths, function(x) any(1:12, x))))
    stop("`season_lengths` must be a vector of integers between 1 and 12.")

  if (!any(c(0.01, 0.05, 0.1) == ci))
    stop("`ci` must be any of 0.01, 0.05, or 0.1.")
  
  climate <- as_brclimate(climate)
  ## when var_names are supplied, apply appropriately
  if (!is.null(var_names)) {
    varno <- dim(climate)[2] - 2
    if (length(var_names) != varno) {
      stop("Count of supplied variable names does not match count of variables in climate data.")
    } else {
      names(climate)[3:(dim(climate)[2])] <- var_names
    }
  }

  ## Makes no sense for less than 2 climate variables
  if (dim(climate)[2] < 4) {
    stop("Two climate variables needed for season correlations.")
  }

  ## get names of primary and secondary variable
  if (any(1:100 == primary)) {
    ## specified by position
    pos <- primary + 2
    if (pos > length(names(climate))) {
      stop("Position for primary variable does not exist.")
    }
    primary_name <- names(climate)[pos]
  } else {
    if (is.character(primary)) {
      ## specified by name
      if (any(names(climate) == primary)) {
        primary_name <- primary
      } else {
        stop("Name for primary variable does not exist.")
      }
    } else {
      stop("Please specify either position or name for primary variable.")
    }
  }

  if (any(1:100 == secondary)) {
    ## specified by position
    pos <- secondary + 2
    if (pos > length(names(climate))) {
      stop("Position for secondary variable does not exist.")
    }
    secondary_name <- names(climate)[pos]
  } else {
    if (is.character(secondary)) {
      ## specified by name
      if (any(names(climate) == secondary)) {
        secondary_name <- secondary
      } else {
        stop("Name for secondary variable does not exist.")
      }
    } else {
      stop("Please specify either position or name for secondary variable.")
    }
  }

  ## check if identical
  if (primary_name == secondary_name) {
    stop("Primary and secondary variable are identical.")
  }

  ## truncate climate and tree-ring data to common or specified
  ## time span
  truncated_input <- truncate_input(chrono, climate,
                                    timespan = timespan, 1,
                                    moving = FALSE)
  m <- length(truncated_input$chrono)

  ## create raw parameter matrix
  pmat <- make_pmat(truncated_input$climate)

  ## create seasons, a list entry for each season_length
  seasons1 <- seasons2 <- list()
  n <- length(season_lengths)
  first_month <- -1 * complete + 1
  last_month <- complete

  ## check if largest season spec is feasible
  if (first_month + max(season_lengths) > 0) {
    maxval <- abs(first_month)
    stop(paste("Largest season length is too long. Maximum value is ",
               maxval, ".", sep = ""))
  }

  lmonths <- c(-1:-12, 1:12)
  last_month_index <- which(lmonths == last_month)

  for (i in 1:n) {
    .season_length <- season_lengths[i]
    seasons1[[i]] <- matrix(NA, ncol = 14, nrow = m)
    seasons2[[i]] <- matrix(NA, ncol = 14, nrow = m)

    for (j in 1:14) {
      end <- last_month_index + 1 - j
      start <- end - .season_length + 1
      end_m <- lmonths[end]
      start_m <- lmonths[start]

      ## create parameter selection list for use with eval_selection
      if (start_m == end_m) {
        method <- "full"
      } else {
        method <- "mean"
      }
      primary_selection <- list(method, start_m:end_m, primary_name)
      secondary_selection <- list(method, start_m:end_m, secondary_name)

      seasons1[[i]][,j] <-
        eval_selection(pmat, primary_selection)$aggregate[,1]
      seasons2[[i]][,j] <-
        eval_selection(pmat, secondary_selection)$aggregate[,1]
    }
  }

  chrono_boot <- init_boot_data(pmat, truncated_input$chrono, 1000,
                                "exact")$chrono

  results <- list()
  results$coef <- list()

  for (i in 1:n) {

    results$coef[[i]] <- list()
    
    params <- .Call("bootres2_pcor", PACKAGE = 'bootres2',
                    seasons1[[i]], seasons2[[i]],
                    chrono_boot, truncated_input$chrono)

    results$coef[[i]]$primary <- ptest(params$primary[,2:1001], ci,
                                       params$primary[,1],
                                       "weibull")[,1:2]
    results$coef[[i]]$secondary <- ptest(params$secondary[,2:1001], ci,
                                         params$secondary[,1],
                                         "weibull")[,1:2]
  }

  results$call <- match.call()
  results$seasons <- list(
    primary = seasons1,
    secondary = seasons2
    )
  class(results) <- c("br_seascorr", "list")
  results
}
