##' Monthly Mean Temperature and Total Precipitation for Forstenrieder
##' Park, Munich
##' 
##' This dataset gives the monthly mean temperature and total
##' precipitation at Forstenrieder Park, Munich, Bavaria, Germany.
##' @source Zang C, Pretzsch H, Rothe A (2012) Size-dependent responses
##' to summer drought in Scots pine, Norway spruce and common
##' oak. Trees -- Structure and Function, 26, 557–569.
##' @docType data
##' @keywords datasets
##' @name muc.clim
##' @usage data(muc.clim)
##' @format A \code{data.frame} containing four columns with year,
##' month, temperature and precipitation.
NULL

##' Tree-Ring Chronology of a Spruce Population near Munich
##' 
##' This dataset gives the tree-ring indices for \emph{Picea
##' abies} at Forstenrieder Park, Munich, Bavaria, Germany. The
##' chronology represents 20 cores from 10 trees. The series were read
##' in using \code{\link[dplR]{read.rwl}} from package \code{dplR},
##' detrended using a 60a spline with 50\% frequency cutoff (function
##' \code{\link[dplR]{detrend}}), and averaged to a chronology using a
##' robust mean \code{\link[dplR]{chron}}.
##' @source Zang C, Pretzsch H, Rothe A (2012) Size-dependent responses
##' to summer drought in Scots pine, Norway spruce and common
##' oak. Trees -- Structure and Function, 26, 557–569.
##' @docType data
##' @keywords datasets
##' @name muc.spruce
##' @usage data(muc.spruce)
##' @format A \code{data.frame} containing tree-ring indices and
##' replication depth with respective years as \code{rownames}.
NULL

##' Modeled Tree-Ring Chronology of a Spruce Population near Munich
##' 
##' This dataset gives the modelled tree-ring widths for
##' \emph{Picea abies} at Forstenrieder Park, Munich, Bavaria,
##' Germany. Tree growth was modeled as a response of low temperatures
##' in previous and current July and August, high temperatures in
##' current February and March, and high precipitation amounts in
##' current July and August.
##' @source Zang C, Pretzsch H, Rothe A (2012) Size-dependent responses
##' to summer drought in Scots pine, Norway spruce and common
##' oak. Trees - Structure and Function, 26, 557-569.
##' @docType data
##' @keywords datasets
##' @name muc.fake
##' @usage data(muc.fake)
##' @format A \code{data.frame} containing tree-ring indices and
##' replication depth with respective years as \code{rownames}.
NULL

##' Monthly Precipitation Sums for Rothenburg ob der Tauber, Germany
##' 
##' This dataset gives the monthly precipitation sum at
##' Rothenburg ob der Tauber, Bavaria, Germany in a decadal
##' (DENDROCLIM2002-style) format)
##' @source Zang C, Rothe A, Weis W, Pretzsch H (2011) Zur
##' Baumarteneignung bei Klimawandel: Ableitung der
##' Trockenstress-Anfälligkeit wichtiger Waldbaumarten aus
##' Jahrringbreiten. Allgemeine Forst- und Jagdzeitung, 182, 98-112.
##' @docType data
##' @keywords datasets
##' @name rt.prec
##' @usage data(rt.prec)
##' @format A \code{data.frame} containing thirteen columns with year
##' and twelve months of precipitation data in mm rainfall.
NULL

##' Monthly Temperature Means for Rothenburg ob der Tauber, Germany
##' 
##' This dataset gives the monthly temperature means at
##' Rothenburg ob der Tauber, Bavaria, Germany in a decadal
##' (DENDROCLIM2002-style) format)
##' @source Zang C, Rothe A, Weis W, Pretzsch H (2011) Zur
##' Baumarteneignung bei Klimawandel: Ableitung der
##' Trockenstress-Anfälligkeit wichtiger Waldbaumarten aus
##' Jahrringbreiten. Allgemeine Forst- und Jagdzeitung, 182, 98-112.
##' @docType data
##' @keywords datasets
##' @name rt.temp
##' @usage data(rt.temp)
##' @format A \code{data.frame} containing thirteen columns with year
##' and twelve months of temperature data in degree Celsius.
NULL

##' Tree-Ring Chronology of a Spruce Population at Rothenburg
##' ob der Tauber
##' 
##' This dataset gives the tree-ring indices for \emph{Picea
##' abies} at Rothenburg ob der Tauber, Bavaria, Germany. The
##' chronology represents 20 cores from 10 trees. The series were read
##' in using \code{\link[dplR]{read.rwl}} from package \code{dplR},
##' detrended using a 60a spline with 50\% frequency cutoff (function
##' \code{\link[dplR]{detrend}}), and averaged to a chronology using a
##' robust mean \code{\link[dplR]{chron}}.
##' @source Zang C, Rothe A, Weis W, Pretzsch H (2011) Zur
##' Baumarteneignung bei Klimawandel: Ableitung der
##' Trockenstress-Anfälligkeit wichtiger Waldbaumarten aus
##' Jahrringbreiten. Allgemeine Forst- und Jagdzeitung, 182, 98-112.
##' @docType data
##' @keywords datasets
##' @name rt.spruce
##' @usage data(rt.spruce)
##' @format A \code{data.frame} containing tree-ring indices and
##' replication depth with respective years as \code{rownames}.
NULL
