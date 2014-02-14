##' (bootstrapped) correlation function
##'
##' See documentation of dcc for details.
##' @param chrono a tree-ring chronology
##' @param climate data.frame with climate parameters
##' @param boot logical: bootstrapping or not?
##' @param sb logical: draw a statusbar or not?
##' @param ci numeric: p-level for confidence interval (must be in
##' c(0.1, 0.05, 0.01)
##' @keywords internal
br_correlation <- function(chrono, climate, boot, sb, ci) {
  vnames <- climate$names
  n <- length(chrono)
  m <- dim(climate$aggregate)[2]

  if (boot) {
    
    param_matrix <- matrix(NA, nrow = m, ncol = 1000)
    if (sb) { # initialize status bar (if TRUE)
      pb <- txtProgressBar(min = 1,  max = 1000, style = 3)
    }
    for (i in 1:1000) {
      boot_sample <- sample(1:n, n, replace = TRUE)
      ## sample
      boot_chrono <- chrono[boot_sample]
      boot_climate <- climate$aggregate[boot_sample, ]
      ## standardize
      boot_chrono <- scale(boot_chrono)
      boot_climate <- scale(boot_climate)
      for (j in 1:m) {
        ## get parameter via singular value decomp
        param_matrix[j, i] <- qr.solve(boot_climate[, j], boot_chrono)
      }
      if (sb)                             # update status bar (if TRUE)
        setTxtProgressBar(pb, i)
    }
    bcf_coef <- apply(param_matrix, 1, median)
    if (ci == 0.05) {
      ci_lower <- apply(param_matrix, 1, function(x) { sort(x)[25] })
      ci_upper <- apply(param_matrix, 1, function(x) { sort(x)[975] })
    } else {
      if (ci == 0.01) {
        ci_lower <- apply(param_matrix, 1, function(x) { sort(x)[5] })
        ci_upper <- apply(param_matrix, 1, function(x) { sort(x)[995] })
      } else {
        ci_lower <- apply(param_matrix, 1, function(x) { sort(x)[50] })
        ci_upper <- apply(param_matrix, 1, function(x) { sort(x)[950] })
      }
    }
    
    ## Significance test
    is_sig <- logical(m)
    for (i in 1:m) {
      if (sign(ci_upper[i]) != sign(ci_lower[i])) {
        is_sig[i] <- FALSE
      } else {
        if (abs(bcf_coef[i]) > abs((abs(ci_upper[i]) - abs(ci_lower[i]))/2)) {
          is_sig[i] <- TRUE
        } else {
          is_sig[i] <- FALSE
        }
      }
    }

    out <- data.frame(coef = bcf_coef, significant = is_sig, ci_lower = ci_lower, ci_upper = ci_upper)
    rownames(out) <- vnames
    if (sb) # close status bar (if TRUE)
      close(pb)
    attributes(out)$npar <- attributes(climate$aggregate)$npar
    attributes(out)$vnames <- vnames
    
  } else {                              # no bootstrapping

    params <- numeric(m)
    .chrono <- scale(chrono)
    .climate <- scale(climate$aggregate)
    for (j in 1:m) {
      params[j] <- qr.solve(.climate[, j], .chrono)
    }
    cf_coef <- params
    ci_lower <- NA
    ci_upper <- NA
    is_sig <- NA
    out <- data.frame(coef = cf_coef, significant = is_sig, ci_lower = ci_lower, ci_upper = ci_upper)
    rownames(out) <- vnames
    attributes(out)$npar <- attributes(climate$aggregate)$npar
    attributes(out)$vnames <- vnames
    
  }
  class(out) <- c("br_coef", "data.frame")
  out
}
