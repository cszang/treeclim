\name{br_design}
\alias{br_design}
\title{Deparse list structure from month specification and return parameter set}
\usage{
br_design(selection, climate)
}
\arguments{
  \item{selection}{the list structure to specify the month
  selection}

  \item{climate}{the climate data as returned by make_pmat}
}
\value{
a data.frame
}
\description{
Deparse list structure from month specification into single
calls, call an aggregating function to collect the data,
and return a parameter set for calibration. In general, the
months must be specified as a list or nested list
structure. Possible cases are: list() list(...)
list(list(...)) list(list(...), list(...), ...)
}

