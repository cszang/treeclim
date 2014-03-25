\name{tc_response}
\alias{tc_response}
\title{(bootstrapped) response function}
\usage{
tc_response(chrono, climate, ci, boot)
}
\arguments{
  \item{chrono}{a tree-ring chronology}

  \item{climate}{data.frame with climate parameters}

  \item{ci}{numeric: p-level for confidence interval (must
  be in c(0.1, 0.05, 0.01)}

  \item{boot}{string: which bootstrapping method?}
}
\description{
See documentation of dcc for details.
}
\keyword{internal}

