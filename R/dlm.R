##' Dendro-flavoured linear model
##' 
##' This is a wrapper around \code{\link{lm}} for working with the
##' same data structures and modifiers as \code{\link{dcc}} does.
##' 
##' Input chronology data can be a \code{data.frame} such as produced
##' by function \code{chron} of package dplR. It has to be a
##' \code{data.frame} with at least one column containing the
##' tree-ring indices, and the corresponding years as \code{rownames}.
##'   
##' For climatic input data, there are three possibilities: Firstly,
##' input climatic data can be a \code{data.frame} or \code{matrix}
##' consisting of at least 3 rows for years, months and at least one
##' climate parameter in the given order. Secondly, input climatic
##' data can be a single \code{data.frame} or \code{matrix} in the
##' style of the original DENDROCLIM2002 input data, i.e. one
##' parameter with 12 months in one row, where the first column
##' represents the year. Or thirdly, input climatic data can be a
##' (potentially named) list of one or several of the latter described
##' \code{data.frame} or \code{matrices}. If named list is provided,
##' potentially provided variable names through argument
##' \code{var_names} are ignored. As an internal format dispatcher
##' checks the format automatically, it is absolutely necessary that
##' in all three cases, only complete years (months 1-12) are
##' provided. It is not possible to mix different formats in one go.
##'   
##' In 'dlm', there is no default parameter selection, in contrast to
##' 'dcc'. Parameters can be selected with the 'selection' parameter
##' in two different ways:
##'
##' \itemize{
##' \item simple selections: as an example -6:9 selects from all
##'   climate variables all months from previous year's June (-6,
##'   previous year's months are specified as negative integers) to
##'   current years September (9, months of the current year are
##'   specified as positive integers) as model parameters. Months from
##'   the previous year and the year before that can be selected using
##'   \link{treeclim-shifters} like \code{..(6)} to refer to July of
##'   the year before the previous year.
##'
##' \item using \emph{modifiers}: More complex parameter selections
##'   can be obtained by the \emph{modifiers} provided in treeclim:
##'   \code{.range}, \code{.mean}, and \code{.sum}. These modifiers
##'   can also be chained to create complex selections. See
##'   \link{treeclim-modifiers} for details.
##' }
##'
##' For the exclusion of months, the convenience function
##' \code{\link{exclude_from}} (or short \code{\link{exfr}}) is
##' provided.
##' 
##' With 'dlm' one would usually try to keep the number of predictors
##' low.
##'
##' For pretty output of the resulting linear model, parameters can be
##' renamed, to e.g. reflect the season they represent.
##'
##' @inheritParams dcc
##' 
##' @param param_names \code{character} vector with parameter
##'   names. Defaults to auto-generated (potentially compound) names.
##' @param intercept \code{logical}: should intercept be included in
##'   model?
##' @param scale \code{logical}: should data be scaled prior to
##'   computing model?  If \code{TRUE}, \code{intercept} will be set
##'   to \code{FALSE}.
##'   
##' @return 'dlm' returns an 'object' of class '"tc_dlm"', which is a
##'   superclass of 'lm'. Additional elements to what is included in
##'   standard 'lm' objects:
##'   
##'   \item{call_dlm}{the call made to function 'dlm'}
##' 
##'   \item{design}{the design matrix on which this call to 'dlm'
##'   operates}
##'   
##'   \item{truncated}{the input data truncated to the common timespan
##'   or the specified timespan}
##'   
##'   \item{original}{the original input data, with the climate data
##'   being recast into a single data.frame}
##' @export
##' 
##' @examples
##' dlm1 <- dlm(rt_spruce, rt_prec, .sum(6:8), param_names = "summer_prec")
##' summary(dlm1)
##' 
##' dlm2 <- dlm(rt_spruce, list(rt_prec, rt_temp),
##'   .sum(6:8, "prec") + .mean(6:8, "temp"), var_names = c("prec", "temp"),
##'   param_names = c("summer_prec", "summer_temp"))
##' summary(dlm2)
##' anova(dlm1, dlm2)
##' @author Christian Zang
dlm <- function(chrono,
                climate,
                selection,
                timespan = NULL,
                var_names = NULL,
                param_names = NULL,
                intercept = TRUE,
                scale = FALSE) {
  
  climate <- apply_var_names(as_tcclimate(climate), var_names)
  selection <- unify_selection(selection)
  
  ## check, if we have months in the correct numerical representation, and
  ## preevaluate month specification for correct data truncation
  monthcheck <- check_months(selection)
  if (monthcheck$check == FALSE) {
    stop("Please specify months with numbers from -24 or `..(12)` (december of year before last year) to 12 (current december).")
  }
  minmonth <- monthcheck$minmonth
  
  ## truncate climate and tree-ring data to common or specified
  ## time span
  truncated_input <- truncate_input(chrono, climate,
                                    timespan = timespan, minmonth,
                                    dynamic = "static")
  
  climate_pmat <- make_pmat(truncated_input$climate, truncated_input$pad)
  
  ## generate design matrix for calibration by evaluating the selections
  design <- tc_design(selection, climate_pmat, check_2 = FALSE)
  
  ## check if number of parameters is smaller than number of observations
  n_params <- dim(design$aggregate)[2]
  n_obs <- dim(design$aggregate)[1]  
  if (n_params > n_obs) {
    stop(
      paste("Overlapping time span of chrono and climate records is smaller than number of parameters! Consider adapting the number of parameters to a maximum of ",
            n_obs, ".", sep = ""))
  }
  
  design_df <- design$aggregate
  names(design_df) <- design$names
  
  ## rename parameters if requested
  if (!is.null(param_names)) {
    if (ncol(design_df) != length(param_names)) {
      stop("Number of parameter names does not match number of parameters.")
    } else {
      names(design_df) <- param_names
    }
  }
  
  design_df$tree <- truncated_input$chrono
  
  if (scale) {
    design_df <- as.data.frame(scale(design_df))
    intercept <- FALSE
  }
  
  if (intercept) {
    model <- lm(tree ~ ., data = design_df)
  } else {
    model <- lm(tree ~ . - 1, data = design_df)
  }  
  model$design <- design
  model$truncated <- list(tree = truncated_input$chrono,
                            climate = truncated_input$climate)
  model$original <- list(tree = chrono,
                           climate = climate)
  model$call_dlm <- match.call()
  class(model) <- c("tc_dlm", "lm")
  model
}
