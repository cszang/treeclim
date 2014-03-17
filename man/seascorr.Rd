\name{seascorr}
\alias{seascorr}
\title{TODO}
\usage{
seascorr(chrono, climate, var_names = NULL, timespan = NULL, complete = 9,
  season_lengths = c(1, 3, 6), primary = 1, secondary = 2, ci = 0.05)
}
\arguments{
  \item{chrono}{\code{data.frame} containing a tree-ring
  chronologies, e.g. as obtained by \code{chron} of package
  dplR.}

  \item{climate}{either a \code{data.frame} or
  \code{matrix} with climatic data in monthly resolution,
  with year, month and climate parameters in columns (all
  columns except year and month will be recognized as
  parameters for response or correlation function), or a
  single \code{data.frame} or \code{matrix} in 13-column
  format (see below), or list of several of the latter.}

  \item{var_names}{\code{character} vector with variable
  names. Defaults to corresponding column names of
  \code{data.frame} clim.}

  \item{timespan}{\code{integer} vector of length 2
  specifying the time interval (in years) to be considered
  for analysis. Defaults to the maximum possible interval.}

  \item{complete}{\code{numeric) month when tree-ring
  growth is expected to have finished.}

  \item{season_lengths}{\code{numeric} vector giving the
  lengths of the seasons for variable grouping}

  \item{primary}{position (numeric) or name (character) of
  primary climate variable}

  \item{secondary}{position (numeric) or name (character)
  of secondary climate variable}

  \item{ci}{\code{numerical} value to set the test level
  for significance test (values 0.01, 0.05 and 0.1 are
  allowed); the confidence intervals are adapted
  accordingly.}
}
\value{
an object of class "br_seascorr"
}
\description{
TODO
}
\examples{
\dontrun{
TODO
}
}
\author{
Christian Zang; the procedure incl. exact bootstrapping was
implemented first by Dave Meko in MATLAB
}
\references{
TODO
}

