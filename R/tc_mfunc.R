##' moving response and correlation function
##' 
##' see doc for dcc for details.
##' @param chrono tree-ring chronology
##' @param climate data.frame with climate parameters
##' @param boot which bootstrapping method should be used? (or "none")
##' @param sb logical: draw statusbar or not?
##' @param start_last logical: start with last (oldest) window?
##' @param win_size numeric: size of the moving in years
##' @param win_offset numeric: size of offset between moving windows
##' in years
##' @param ci numeric: p-level for confidence interval (must be in
##' c(0.1, 0.05, 0.01)
##' @param method character: method to be used (one of "response" or
##' "correlation")
##' @param p probability for rgeom, that determines distribution of
##' sampling blocks for stationary bootstrap scheme
##' @keywords internal
tc_mfunc <- function(chrono, climate, boot, sb, start_last,
                     win_size, win_offset, ci, method, p, check_ac) {

  vnames <- climate$names
  pretty_names <- climate$pretty_names
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
  
  if (check_ac)
    ac0_1 <- ac0_2 <- acb_1 <- acb_2 <- numeric(win_num)
    
  if (sb)                            # initialize status bar (if TRUE)
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
      months = climate$month,
      pretty_names = climate$pretty_names
      )
    chrono_win <- chrono[series_subset_index]

    if (method == "response") {
      window <- tc_response(chrono_win, climate_win_list,
                            ci = ci, boot = boot, p = p,
                            check_ac = check_ac)
    } else {
      window <- tc_correlation(chrono_win, climate_win_list,
                               ci = ci, boot = boot, p = p,
                               check_ac = check_ac)
    }
    
    result_matrix_coef[,k] <- window$result$coef
    result_matrix_ci_upper[,k] <- window$result$ci_upper
    result_matrix_ci_lower[,k] <- window$result$ci_lower
    result_matrix_significant[,k] <- window$result$significant
    win_years_string[k] <- paste(years[series_subset_index][1],
                                 years[series_subset_index][win_size],
                                 sep = "-")
    
    if (check_ac) {
      ac0_1[k] <- window$ac$ac0[1]
      ac0_2[k] <- window$ac$ac0[2]
      acb_1[k] <- window$ac$acb$acb1[1]
      acb_2[k] <- window$ac$acb$acb2[1]
    }

    if (sb)                             # update status bar (if TRUE)
      setTxtProgressBar(mpb, k)
    
  }

  ## reorder output
  result_matrix_coef <- result_matrix_coef[,win_num:1]
  result_matrix_ci_upper <- result_matrix_ci_upper[,win_num:1]
  result_matrix_ci_lower <- result_matrix_ci_lower[,win_num:1]
  result_matrix_significant <- result_matrix_significant[,win_num:1]
  win_years_string <- win_years_string[win_num:1]
  
  if (check_ac) {
    ## calculate ac characteristics
    rm_na <- function(x) {
      if (is.na(x))
        0
      else
        x
    }
    
    ac0_1 <- sapply(ac0_1, rm_na)
    ac0_2 <- sapply(ac0_2, rm_na)
    acb_1 <- sapply(acb_1, rm_na)
    acb_2 <- sapply(acb_2, rm_na)
    ac <- list()
  } else {
    ac <- NULL
  }

  out <- list(
    result = list(),
    ac = ac
  )
  out$result$coef <- data.frame(result_matrix_coef)
  colnames(out$result$coef) <- win_years_string
  rownames(out$result$coef) <- vnames
  out$result$ci_upper <- data.frame(result_matrix_ci_upper)
  colnames(out$result$ci_upper) <- win_years_string
  rownames(out$result$ci_upper) <- vnames
  out$result$ci_lower <- data.frame(result_matrix_ci_lower)
  colnames(out$result$ci_lower) <- win_years_string
  rownames(out$result$ci_lower) <- vnames
  out$result$significant <- data.frame(result_matrix_significant)
  colnames(out$result$significant) <- win_years_string
  rownames(out$result$significant) <- vnames
  out$result$pretty_names <- pretty_names
  
  if (check_ac) {
    out$ac$ac0 <- list(
      ac01 = c(mean = mean(ac0_1), sd = sd(ac0_1)),
      ac02 = c(mean = mean(ac0_2), sd = sd(ac0_2)))
    out$ac$acb <- list(
      acb1 = c(mean = mean(acb_1), sd = sd(acb_1)),
      acb2 = c(mean = mean(acb_2), sd = sd(acb_2)))
  }
    
  if (sb)                               # close status bar (if TRUE)
    close(mpb)
  class(out$result) <- c("tc_mcoef", "list")
  out
}
