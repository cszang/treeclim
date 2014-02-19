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
##' @import plyr
##' @S3method plot br_dcc
plot.br_dcc <- function(x) {
  data <- x$coef

  if (any(class(data) == "br_coef")) {

    line0 <- data.frame(
      x = c(0.5, data$id, max(data$id) + 0.5),
      y = rep(0, dim(data)[1] + 2)
      )

    ggplot(data, aes(x = id, y = coef)) +
      geom_line(data = line0, aes(x, y), color = "grey") +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color =
                        varname, lty = significant), size = 1) +
      scale_linetype_manual(values = c("dotted", "solid")) +
      geom_point(aes(color = varname), size = 3) +
      scale_x_continuous(breaks = data$id, labels = data$month) +
      ylab("Coefficients") +
      xlab("Months") +  
      theme_minimal() +
      theme(axis.title.x = element_blank())

  } else {

    ## mdcc case

    ## bootstrapped or not?

    if(!is.null(x$call$boot)) {
      if (x$call$boot) {
        boot <- TRUE
      } else {
        boot <- FALSE
      }
    } else {
      boot <- TRUE
    }
    
    coef <- data$coef
    n <- dim(coef)[2]
    m <- dim(coef)[1]

    ## reformat into ggplot compatible data.frame

    pdata <- data.frame(
      varname = abbrev_name(rep(rownames(coef), n)),
      window = rep(names(coef), each = m),
      coef = as.vector(as.matrix(coef)),
      significant = as.vector(as.matrix(data$significant))
      )

    pdata$wid <- rep(1:n, each = m)
    pdata$vid <- rep(1:m, n)

    pdata$pid <- factor(paste(pdata$wid, pdata$vid, sep = "."))

    create_grid <- function(x) {
      w <- x$wid
      v <- x$vid
      data.frame(
        pid = factor(rep(paste(w, v, sep = "."), 4)),
        x = c(w - 1, w, w, w - 1),
        y = c(v - 1, v - 1, v, v)
        )
    }

    idgrid <- ddply(pdata, .variables = c("wid", "vid"), create_grid)

    idpgrid <- merge(pdata, idgrid, by = c("pid"))

    gg <- ggplot(idpgrid, aes(x = window, y = varname)) +
      geom_polygon(aes(x, y, fill = coef, group = pid)) +
      scale_fill_gradient2() +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0.5, by = 1, length.out = n),
                         labels = names(coef)) +
      scale_y_continuous(breaks = seq(0.5, by = 1, length.out = m), labels = abbrev_name(rownames(coef))) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    if (boot) {

      gg + geom_point(data = subset(pdata, significant), aes(x = wid -
                        0.5, y = vid - 0.5), pch = 8, color = "grey")
      
    } else {

      gg
  
    }
  }
}
