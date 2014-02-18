##' @S3method print br_dcc
print.br_dcc <- function(x) {
  ci <- x$call$ci
  if (is.null(ci))
    ci <- 0.05
  cat("Coefficients (significance flags correspond to p < ",
      ci, "):\n", sep = "")
  print(x$coef)
}

##' @S3method print br_coef
print.br_coef <- function(x) {
  rownames(x) <- abbrev_name(rownames(x))
  x$coef <- round(x$coef, 3)
  x$ci_lower <- round(x$ci_lower, 3)
  x$ci_upper <- round(x$ci_upper, 3)
  print.data.frame(x)
}

##' @importFrom abind abind
##' @S3method print br_mcoef
print.br_mcoef <- function(x) {
  mm <- abind(x$coef, x$significant, along = 3)
  ms <- apply(mm, c(1, 2), function(x) {
    if (!is.na(x[2])) {
      if (x[2]) {
        xc <- paste(round(x[1], 3), "*", sep = "")
      } else {
        xc <- round(x[1], 3)
      }
    } else {
      xc <- round(x[1], 3)
    }
    xc
  })
  rownames(ms) <- abbrev_name(rownames(ms))
  print(ms)
}

##' @S3method print br_design
print.br_design <- function(x) {
  pr <- x$aggregate
  names(pr) <- abbrev_name(x$names)
  years <- as.numeric(rownames(pr))
  cat(length(years), "observations of", dim(pr)[2],
      "variables.\nObserved years:", min(years), "-", max(years),
      "\nUsed (potentially aggregated) variables:\n")
  cat(paste(names(pr), collapse = "\n"), "\n")
}

##' @S3method coef br_dcc
coef.br_dcc <- function(x) {
  coef(x$coef)
}

##' @S3method coef br_coef
coef.br_coef <- function(x) {
  data.frame(x)
}

##' @S3method coef br_mcoef
coef.br_mcoef <- function(x) {
  data.frame(x$coef)
}

##' @S3method summary br_dcc
summary.br_dcc <- function(x) {
  cat("Call:\n")
  print(x$call)
  cat("\nDesign matrix:\n")
  print(x$design)
  cat("\nCoefficients:\n")
  print(x$coef)
}

##' @S3method + br_paramlist
"+.br_paramlist" <- function(p1, p2) {
  ## check if p1 already is _nested_ list
  if (is.list(p1[[1]])) {
    ## just write to this list
    param_list <- p1
    i <- length(param_list) + 1
    param_list[[i]] <- p2
  } else {
    ## make a wrapper list
    param_list <- list()
    param_list[[1]] <- p1
    param_list[[2]] <- p2
  }
  param_list
}

##' @import ggplot2
##' @S3method plot br_dcc
plot.br_dcc <- function(x) {
  data <- x$coef

  if (any(class(data) == "br_coef")) {

    ggplot(odata, aes(x = id, y = coef)) +
      geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper, color =
                          varname, pch = significant)) + 
                            scale_x_discrete(labels = odata$month)
    
  } else {

    ## mdcc case
    
  }
}
