\name{init_boot_data}
\alias{init_boot_data}
\title{initialize data structure for bootstrapping}
\usage{
init_boot_data(u, g, n, bootmethod)
}
\arguments{
  \item{u}{climate data (matrix with parameters in columns
  and years in rows)}

  \item{g}{vector with tree-ring data}

  \item{n}{times for resampling}

  \item{bootmethod}{one of c("std", "exact")}
}
\value{
a list
}
\description{
this will create an array for the climate data and
corresponding matrix for the tree-ring data, holding the
randomly resampled data for bootstrapping
}
\keyword{internal}

