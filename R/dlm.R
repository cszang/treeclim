#' Dendro-flavoured linear model
#' 
#' This is a wrapper around \code{\link{lm}} for working with the same data 
#' structures and modifiers as with \code{\link{dcc}}.
#' @details Input chronology data can be a \code{data.frame} such as produced by
#'   function \code{chron} of package dplR. It has to be a \code{data.frame} 
#'   with at least one column containing the tree-ring indices, and the 
#'   corresponding years as \code{rownames}.
#'   
#'   For climatic input data, there are three possibilities: Firstly, input 
#'   climatic data can be a \code{data.frame} or \code{matrix} consisting of at 
#'   least 3 rows for years, months and at least one climate parameter in the 
#'   given order. Secondly, input climatic data can be a single 
#'   \code{data.frame} or \code{matrix} in the style of the original 
#'   DENDROCLIM2002 input data, i.e. one parameter with 12 months in one row, 
#'   where the first column represents the year. Or thirdly, input climatic data
#'   can be a list of several of the latter described \code{data.frame} or 
#'   \code{matrices}. As an internal format dispatcher checks the format 
#'   automatically, it is absolutely necessary that in all three cases, only 
#'   complete years (months 1-12) are provided. It is not possible to mix 
#'   different formats in one go.
#'   
#'   Parameters can be selected with the 'selection' parameter in two different 
#'   ways. In 'dlm', there is no default selection, in contrast to 'dcc'. As an 
#'   example -6:9 selects from all climate variables all months from previous 
#'   year's June (-6, previous year's months are specified as negative integers)
#'   to current years September (9, months of the current year are specified as 
#'   positive integers) as model parameters. With 'dlm' one would usually try to
#'   keep the number of predictors low.
#'   
#'   More complex parameter selections can be obtained by the \emph{modifiers} 
#'   provided in treeclim: \code{.range}, \code{.mean}, and \code{.sum}. 
#'   \code{.range} corresponds the example above, where all specified months are
#'   used, while \code{.sum} and \code{.mean} will use the sums and means of the
#'   specified months. These modifiers also allow to select specific climatic 
#'   variables, addressed by name. Thus, \code{.mean(4:8, "temp")} will select 
#'   the mean for climate parameter "temp" for the months April to August. Not 
#'   only ranges, but also individual vectors can be used for month 
#'   specification, like e.g., \code{.range(c(1, 3, 4, 5)}.
#'   
#'   The modifiers can be chained together using the '+' symbol, which makes it 
#'   possible to create arbitrarily complex selections of climate parameters for
#'   calibration.  E.g., \code{.mean(2:5, "temp") + .sum(2:5, "prec")} will 
#'   yield the February-to-May mean for the variable "temp" and the sum of the 
#'   variable "prec" for the same time. While there is no limitation for number 
#'   of lists that can be chained together, 'dcc' will not check for meaningful 
#'   specifications. Testing smart hypotheses is up the researcher.
#'   
#'   For the exclusion of months, the convenience function \code{excludefrom()} 
#'   (or short \code{exfr()}) is provided. E.g., \code{.range(excludefrom(-6:10,
#'   -11:3))} will yield the monthly values of all parameters for the months 
#'   previous June (-6) to current October (10), but without the months previous
#'   November (-11) to current March (3) in between. While it is also possible 
#'   to supply arbitrary vectors as month specification, and not only ranges as 
#'   shown in most of the examples here, this way of excluding e.g. the dormant 
#'   season is far more convenient.
#'   
#'   For pretty output of the resulting linear model, parameters can be renamed,
#'   to e.g. reflect the season they represent.
#'   
#' @param chrono \code{data.frame} containing a tree-ring chronologies, e.g. as 
#'   obtained by \code{chron} of package dplR.
#' @param climate either a \code{data.frame} or \code{matrix} with climatic data
#'   in monthly resolution, with year, month and climate parameters in columns 
#'   (all columns except year and month will be recognized as parameters for 
#'   response or correlation function), or a single \code{data.frame} or 
#'   \code{matrix} in 13-column format (see below), or list of several of the 
#'   latter.
#' @param selection selection either a numeric vector, a modifier, or a chain of
#'   modifiers specifying the parameter selection for the model (see Details).
#' @param timespan \code{integer} vector of length 2 specifying the time 
#'   interval (in years) to be considered for analysis. Defaults to the maximum 
#'   possible interval.
#' @param var_names \code{character} vector with variable names. Defaults to 
#'   corresponding column names of \code{data.frame} clim.
#' @param intercept \code{logical}: should intercept be included in model?
#' @param scale \code{logical}: should data be scaled prior to computing model? 
#'   If \code{TRUE}, \code{intercept} will be set to \code{FALSE}.
#'   
#' @return 'dlm' returns an 'object' of class '"tc_dlm"', which is a superclass 
#'   of 'lm'. Additional elements to what is included in standard 'lm' objects:
#'   
#'   \item{call_dlm}{the call made to function 'dlm'} \item{design}{the design 
#'   matrix on which this call to 'dcc' operates}
#'   
#'   \item{truncated}{the input data truncated to the common timespan or the 
#'   specified timespan}
#'   
#'   \item{original}{the original input data, with the climate data being recast
#'   into a single data.frame}
#' @export
#' 
#' @examples
#' dlm1 <- dlm(rt_spruce, rt_prec, .sum(6:8), param_names = "summer_prec")
#' summary(dlm1)
#' 
#' dlm2 <- dlm(rt_spruce, list(rt_prec, rt_temp),
#'   .sum(6:8, "prec") + .mean(6:8, "temp"), var_names = c("prec", "temp"),
#'   param_names = c("summer_prec", "summer_temp"))
#' summary(dlm2)
#' anova(dlm1, dlm2)
#' @author Christian Zang
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