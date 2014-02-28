##' (bootstrapped) correlation function
##'
##' See documentation of dcc for details.
##' @param chrono a tree-ring chronology
##' @param climate data.frame with climate parameters
##' @param ci numeric: p-level for confidence interval (must be in
##' c(0.1, 0.05, 0.01)
##' @keywords internal
br_correlation <- function(chrono, climate, ci, boot) {

  vnames <- climate$names
  n <- length(chrono)
  m <- dim(climate$aggregate)[2]
  
  boot_data <- init_boot_data(as.matrix(climate$aggregate),
                              chrono, 1000, boot)
  
  if (boot %in% c("std", "dendroclim")) {
    param_matrix <- .Call("bootres2_corfun", boot_data$climate,
                          boot_data$chrono, PACKAGE = "bootres2")$coef
    
    out <- ptest(param_matrix, ci, NULL, "range")
  } else {
    if (boot == "exact") {
      param_matrix <- .Call("bootres2_corfunexact", boot_data$climate,
                            boot_data$chrono, chrono, PACKAGE = "bootres2")$coef
      
      out <- ptest(param_matrix[,2:1001], ci, param_matrix[,1], "weibull")
    }
  }

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
