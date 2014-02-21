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

  respoR <- function(u, g) {

    result <- matrix(nrow = dim(u)[2], ncol = 1000)
    
    for (h in 1:1000) {
      chrono <- g[,h]
      climate <- u[, , h]
      ## standardize
      chrono_s <- scale(chrono)
      climate_s <- scale(climate)
      ## correlation matrix X'X (q*q)
      cor_mat <- cor(climate_s)
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
      pc_scores <- climate_s %*% reduced_eigenvectors
      ## calculate solution for Z*K = Y (coefficients) (m*1)
      k <- qr.solve(pc_scores, chrono_s)
      ## pad K with zero so that Kq*1
      zeros <- rep(0, length(which(cumprods < 1)))
      ## (q*1)
      k <- c(k, zeros)
      ## response coefficients (q*1)
      b <- eigenvectors %*% k
      result[, h] <- b
    }
    result
  }

  boot_data <- init_boot_data(as.matrix(climate$aggregate),
                              chrono, 1000, "std")

  param_matrix <- respoR(boot_data$climate, boot_data$chrono)
  
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
    
  out <- data.frame(coef = brf_coef,
                    significant = is_sig,
                    ci_lower = ci_lower,
                    ci_upper = ci_upper)
  rownames(out) <- abbrev_name(vnames)
  attributes(out)$npar <- attributes(climate$aggregate)$npar
  attributes(out)$vnames <- vnames
    
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
