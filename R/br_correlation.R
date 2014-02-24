##' (bootstrapped) correlation function
##'
##' See documentation of dcc for details.
##' @param chrono a tree-ring chronology
##' @param climate data.frame with climate parameters
##' @param sb logical: draw a statusbar or not?
##' @param ci numeric: p-level for confidence interval (must be in
##' c(0.1, 0.05, 0.01)
##' @keywords internal
br_correlation <- function(chrono, climate, sb, ci) {

  vnames <- climate$names
  n <- length(chrono)
  m <- dim(climate$aggregate)[2]

  boot_data <- init_boot_data(as.matrix(climate$aggregate),
                              chrono, 1000, "std")

  param_matrix <- .Call("bootres2_corfun", boot_data$climate,
                        boot_data$chrono, PACKAGE = "bootres2")$coef
  
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
