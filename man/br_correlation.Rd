\name{br_correlation}
\alias{br_correlation}
\title{(bootstrapped) correlation function}
\usage{
br_correlation(chrono, climate, ci, boot)
}
\arguments{
  \item{chrono}{a tree-ring chronology}

  \item{climate}{data.frame with climate parameters}

  \item{ci}{numeric: p-level for confidence interval (must
  be in c(0.1, 0.05, 0.01)}

  \item{boot}{character, which bootstrapping method should
  be used (or "none")}
}
\description{
See documentation of dcc for details.
}
\keyword{internal}

