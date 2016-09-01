##' Trace the temporal development of response or correlation coefficients
##' 
##' This is a plotting function that takes the output from a dynamic ("moving"
##' or "evolving") \code{dcc} run to trace the temporal development of all or
##' selected variables. The variables can be selected using their name, partial
##' matching is allowed.
##' 
##' @param x an object returned from a call to \code{dcc} with parameter
##'   \code{dynamic} set to either "moving" or "evolving".
##' @param variables a character vector of variable names to be plotted; if NULL
##'   (default), all variables will be traced
##' @param facet logical: shall the plot be splitted into facets for different
##'   variables?
##' @return an object of class "gg".
##' @examples
##' \dontrun{
##' dc_resp <- dcc(muc_spruce, muc_clim, 4:9, dynamic = "evolving")
##' traceplot(dc_resp, c("prec.curr.may", "prec.curr.jun"))
##' }
##' @import ggplot2
##' @export
traceplot <- function(x, variables = NULL, facet = FALSE) {
  if (!any(class(x$coef) %in% c("tc_mcoef", "tc_ecoef"))) {
    stop("`traceplot` requires the output of a call to `dcc` with 'dynamic' set to either 'moving' or 'evolving'.")
  }
  variable <- winid <- significant <- NULL # to keep R CMD check happy
  coefs <- x$coef$coef
  sigs <- x$coef$significant
  var_names <- x$design$names
  win_num <- ncol(coefs)
  if (is.null(variables)) {
    variables <- var_names
    var_index <- 1:length(variables)
  } else {
    var_index <- pmatch(variables, var_names)
    if (anyNA(var_index)) {
      var_na <- which(is.na(var_index))
      var_names_print <- paste(var_names, collapse = "\n")
      stop(paste0("Variable ", variables[var_na], " not found.\n"), "Available variabes are:\n", var_names_print)
    } else {
      variables <- var_names[var_index]
    }
  }
  nvar <- length(variables)
  df <- data.frame(window = rep(names(coefs), each = nvar),
                   winid = rep(1:win_num, each = nvar),
                   coef = as.vector(as.matrix(coefs[var_index,])),
                   significant = as.vector(as.matrix(sigs[var_index, ])),
                   variable = abbrev_name(rep(variables, win_num)))
  
  if (facet) {
    gg <- ggplot(df) + geom_line(aes(x = winid, y = coef)) +
      geom_point(data = subset(df, significant == TRUE), aes(x = winid, y = coef)) +
      facet_wrap(~variable)
  } else {
    gg <- ggplot(df) + geom_line(aes(x = winid, y = coef, colour = variable)) +
      geom_point(data = subset(df, significant == TRUE), aes(x = winid, y = coef, colour = variable))
  }
  
  gg <- gg + theme_minimal() +
    scale_x_continuous(breaks = seq(0.5, by = 1, length.out = win_num),
                       labels = names(coefs)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  gg
}
