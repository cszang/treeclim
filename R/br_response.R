##' (bootstrapped) response function
##'
##' See documentation of dcc for details.
##' @param chrono a tree-ring chronology
##' @param climate data.frame with climate parameters
##' @param boot logical: bootstrapping or not?
##' @param sb logical: draw a statusbar or not?
##' @param ci numeric: p-level for confidence interval (must be in
##' c(0.1, 0.05, 0.01)
##' @keywords internal
br_response <- function(chrono, climate, boot, sb, ci) {
  
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
      ## correlation matrix X'X (q*q)
      cor_mat <- cor(boot_climate)
      ## eigenvector decomposition
      eigen_decomp <- eigen(cor_mat)
      ## normalized eigenvectors
      eigenvectors <- eigen_decomp$vectors 
      eigenvalues <- eigen_decomp$values
      ## PVP criterion: calculate cumulative eigenvalues until value < 1
      cumprods <- cumprod(eigenvalues)
      ## matrix of reduced eigenvectors (q*m)
      reduced_eigenvectors <- eigenvectors[, cumprods > 1]
      ## calculate princ comp scores (n*m)
      pc_scores <- boot_climate %*% reduced_eigenvectors
      ## calculate solution for Z*K = Y (coefficients) (m*1)
      k <- qr.solve(pc_scores, boot_chrono)
      ## pad K with zero so that Kq*1
      zeros <- rep(0, length(which(cumprods < 1)))
      ## (q*1)
      k <- c(k, zeros)
      ## response coefficients (q*1)
      b <- eigenvectors %*% k 
      param_matrix[, i] <- b
      if (sb)                           # update status bar (if TRUE)
        setTxtProgressBar(pb, i)
    }
    brf_coef <- apply(param_matrix, 1, median)
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
        if (abs(brf_coef[i]) > abs((abs(ci_upper[i]) - abs(ci_lower[i]))/2)) {
          is_sig[i] <- TRUE
        } else {
          is_sig[i] <- FALSE
        }
      }
    }
    
    out <- data.frame(coef = brf_coef, significant = is_sig, ci_lower = ci_lower, ci_upper = ci_upper)
    rownames(out) <- abbrev_name(vnames)
    if (sb)                             # close status bar (if TRUE)
      close(pb)
    attributes(out)$npar <- attributes(climate$aggregate)$npar
    attributes(out)$vnames <- vnames
    
  } else {                              # no bootstrapping

    .chrono <- scale(chrono)
    .climate <- scale(climate$aggregate)
    cor_mat <- cor(.climate)
    eigen_decomp <- eigen(cor_mat)
    eigenvectors <- eigen_decomp$vectors
    eigenvalues <- eigen_decomp$values
    cumprods <- cumprod(eigenvalues)
    reduced_eigenvectors <- eigenvectors[, cumprods > 1]
    pc_scores <- .climate %*% reduced_eigenvectors
    k <- qr.solve(pc_scores, .chrono)
    zeros <- rep(0, length(which(cumprods < 1)))
    k <- c(k, zeros)
    b <- eigenvectors %*% k
    rf_coef <- b
    ci_lower <- NA
    ci_upper <- NA
    is_sig <- NA
    out <- data.frame(coef = rf_coef, significant = is_sig, ci_lower = ci_lower, ci_upper = ci_upper)
    rownames(out) <- vnames
    attributes(out)$npar <- attributes(.climate)$npar
    attributes(out)$vnames <- vnames
  }

  ## include information for pretty printing and assemble output
  ## data.frame
  
  out <- data.frame(
    id = climate$pretty_names$id,
    varname = climate$pretty_names$varname,
    month = climate$pretty_names$month_label,
    out
    )
  
  class(out) <- c("br_coef", "data.frame")
  out
}
