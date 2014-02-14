##' moving (bootstrapped) response and correlation function
##'
##' see doc for dcc for details.
##' @param chrono tree-ring chronology
##' @param climate data.frame with climate parameters
##' @param boot logical: bootstrap or not?
##' @param sb logical: draw statusbar or not?
##' @param start_last logical: start with last (oldest) window?
##' @param win_size numeric: size of the moving in years
##' @param win_offset numeric: size of offset between moving windows
##' in years
##' @param ci numeric: p-level for confidence interval (must be in
##' c(0.1, 0.05, 0.01)
##' @param method character: method to be used (one of "response" or
##' "correlation")
br_mfunc <- function(chrono, climate, boot, sb, start_last,
                         win_size, win_offset, ci, method) {

  vnames <- climate$names
  ## number of windows
  years <- as.numeric(rownames(climate$aggregate))
  nyears <- length(years)
  win_num <- (length(chrono) - win_size) %/% win_offset
  if (win_num < 2) {
    stop(paste("Less than 2 windows. Consider a timespan greater than ",
            nyears, " or a win_size smaller than ", win_size, ".",
            sep = ""))
  }
  win_years_string <- character(win_num)
  windows <- 1:win_num

  ## initialize result matrices
  result_matrix_coef <- result_matrix_ci_upper <-
    result_matrix_ci_lower <- result_matrix_significant <-
      matrix(NA, ncol = win_num, nrow = dim(climate$aggregate)[2])

  if (sb)  # initialize status bar (if TRUE)
    mpb <- txtProgressBar(min = 1,  max = win_num, style = 3)
  

  for (k in 1:win_num) {
    if (start_last) {
      series_subset_index <- ((nyears - ((k-1) * win_offset)) -
                              (win_size - 1)):(nyears - ((k-1) * win_offset))
    } else {
      series_subset_index <- (1 + ((k-1) * win_offset)):(1 + ((k-1) *
                                                              win_offset) +
                                                         (win_size - 1))
    }
      
    climate_win <- climate$aggregate[series_subset_index,]
    ## recover the original list structure
    climate_win_list <- list(
      aggregate = climate_win,
      names = climate$names,
      param = climate$param,
      months = climate$month
      )
    chrono_win <- chrono[series_subset_index]

    if (method == "response") {
      window <- br_response(chrono_win, climate_win_list, boot = boot,
                            ci = ci, sb = FALSE)
    } else {
      window <- br_correlation(chrono_win, climate_win_list, boot = boot,
                               ci = ci, sb = FALSE)
    }

    result_matrix_coef[,k] <- window$coef
    result_matrix_ci_upper[,k] <- window$ci_upper
    result_matrix_ci_lower[,k] <- window$ci_lower
    result_matrix_significant[,k] <- window$significant
    win_years_string[k] <- paste(years[series_subset_index][1],
                                 years[series_subset_index][win_size],
                                 sep = "-")

    if (sb) # update status bar (if TRUE)
      setTxtProgressBar(mpb, k)
      
  }

  ## reorder output
  result_matrix_coef <- result_matrix_coef[,win_num:1]
  result_matrix_ci_upper <- result_matrix_ci_upper[,win_num:1]
  result_matrix_ci_lower <- result_matrix_ci_lower[,win_num:1]
  result_matrix_significant <- result_matrix_significant[,win_num:1]
  win_years_string <- win_years_string[win_num:1]

  out <- list()
  out$coef <- data.frame(result_matrix_coef)
  colnames(out$coef) <- win_years_string
  rownames(out$coef) <- vnames
  out$ci_upper <- data.frame(result_matrix_ci_upper)
  colnames(out$ci_upper) <- win_years_string
  rownames(out$ci_upper) <- vnames
  out$ci_lower <- data.frame(result_matrix_ci_lower)
  colnames(out$ci_lower) <- win_years_string
  rownames(out$ci_lower) <- vnames
  out$significant <- data.frame(result_matrix_significant)
  colnames(out$significant) <- win_years_string
  rownames(out$significant) <- vnames
  if (sb) # close status bar (if TRUE)
    close(mpb)
  class(out) <- c("br_mcoef", "list")
  out
}
