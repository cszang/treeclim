##' Truncate climate and tree data to common interval
##' 
##' Truncate climate and tree data either to common shared interval
##' (default) or to specified range of years. Depending on minmonth,
##' the climate data will need to start one year earlier than the tree
##' data, if data from the previous year should be used.
##' @param chrono a tree-ring chronology
##' @param climate the climate data as returned by as_tcclimate
##' @param timespan the timespan for truncating as vector with min and max year
##' @param minmonth the earliest month used for the calibration, as
##' returned by check_months
##' @param moving moving or not? (logical)
##' @return a list of truncated data.frames for climate and tree data
##' @keywords manip internal
truncate_input <- function(chrono, climate, timespan = NULL, minmonth,
                           dynamic, silent = FALSE) {

  ## get time spans of both input data
  chrono_years <- as.numeric(row.names(chrono))
  climate_years <- sort(unique(climate[, 1]))

  ## calculate overlap
  if (chrono_years[1] <= climate_years[1]) {
    overlap <- na.omit(climate_years[match(chrono_years, climate_years)])
  } else {
    overlap <- na.omit(chrono_years[match(climate_years, chrono_years)])
  }

  ## take maximum overlap, when timespan is not set
  if (is.null(timespan)) {
    start_year <- overlap[1]
    end_year <- tail(overlap, 1)
  } else {
    if (minmonth > 0) {
      if (!is.element(timespan[1], overlap) ||
          !is.element(timespan[2], overlap)) {
        stop(paste("`timespan` has to be between ", overlap[1],
                   " and ", tail(overlap, 1),
                   " for start dates in current year.",
                   sep = ""))
      } else {
        start_year <- timespan[1]
        end_year <- timespan[2]
      }
    } else {
      if (!is.element(timespan[1], overlap) ||
          !is.element(timespan[2], overlap)) {
        stop(paste("`timespan` has to be between ", overlap[1] +
                   1, " and ", tail(overlap, 1),
                   " for start dates in previous year.", sep = ""))
      } else {
        start_year <- timespan[1]
        end_year <- timespan[2]
      }
    }
  }
  
  ## check if previous years are available in climatic data
  offset <- 0
  if (minmonth < 0 & minmonth > -13 && !((start_year - 1) %in% climate_years)) { 
    offset <- 1
  }
  if (minmonth < -12 && !((start_year - 2) %in% climate_years)) { 
    offset <- 2
  }
  
  if (offset != 0) {
    if (is.null(timespan) & !silent) {
      message(paste("treeclim tries to use the maximum overlap in timespan for chronology and climate data. The overlap starts in ",
                    start_year,
                    ", but to be able to use climate data from the previous year(s) (as you chose by setting 'selection' accordingly), the analysis starts in ",
                    start_year + offset, ".", sep = ""))
    } else {
      if (!silent) {
        message(paste("`start_year` is set from", start_year, "to",
                      start_year + offset,
                      "to be able to use climate data from the previous year(s)."))
      }
    }
  }
  
  interval_chrono <- (start_year + offset):end_year
  ## make sure that data get truncated properly	
  if (minmonth < 0) { 
    if (minmonth > -13) {
      interval_climate <- (start_year - 1 + offset):end_year
      pad <- 1
    } else {
      interval_climate <- (start_year - 2 + offset):end_year
      pad <- 0
    }
  } else {
    interval_climate <- (start_year + offset):end_year
    pad <- 2
  }

  a <- chrono_years %in% interval_chrono
  b <- climate[, 1] %in% interval_climate
  
  ## calculate timespan for analysis for reporting
  run_years <- chrono_years[a]

  ## truncate data
  chrono_trunc <- chrono[a, 1]
  climate_trunc <- climate[b, ]

  ## check for missing data in tree-ring data and report missing years
  missing <- FALSE
  if (any(is.na(chrono_trunc))) {
    missing <- TRUE
    missing_tree <- which(is.na(chrono_trunc))
    missing_years <- run_years[missing_tree]
    if (!silent) {
      if (length(missing_years) > 1) {
        if (length(missing_years) > 2) {
        ox_comma <- ","
        } else {
          ox_comma <- ""
        }
        cat("Missing proxy data for ",
            paste(missing_years[-length(missing_years)],
                  collapse = ", "), ox_comma, paste0(" and ", tail(missing_years, 1)),
            ".\n", sep = "")  
      } else {
        cat("Missing proxy data for ", missing_years, ".\n", sep = "")
      }
    }
    chrono_trunc <- chrono_trunc[-missing_tree]
    missing_rows_climate <- climate_trunc[,1] %in% missing_years
    climate_trunc <- climate_trunc[!(missing_rows_climate), ]
  }
  
  ## throw error if climate data contains missing values
  if (any(is.na(climate_trunc))) {
    stop("Climate data for considered time span contains missing values.\n")
  }
  
  ## report time span used
  if (dynamic == "static" & !silent) {
    cat("Running for timespan ", run_years[1], " - ", tail(run_years, 1), "...\n",
        sep = "")
  }

  list(chrono = chrono_trunc,
       climate = climate_trunc,
       pad = pad,
       missing = missing)
}
