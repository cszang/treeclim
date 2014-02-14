\docType{data}
\name{muc.spruce}
\alias{muc.spruce}
\title{Tree-Ring Chronology of a Spruce Population near Munich}
\format{A \code{data.frame} containing tree-ring indices and
replication depth with respective years as \code{rownames}.}
\source{
Zang C, Pretzsch H, Rothe A (2012) Size-dependent responses
to summer drought in Scots pine, Norway spruce and common
oak. Trees -- Structure and Function, 26, 557–569.
}
\usage{
data(muc.spruce)
}
\description{
This dataset gives the tree-ring indices for \emph{Picea
abies} at Forstenrieder Park, Munich, Bavaria, Germany. The
chronology represents 20 cores from 10 trees. The series
were read in using \code{\link[dplR]{read.rwl}} from
package \code{dplR}, detrended using a 60a spline with 50\%
frequency cutoff (function \code{\link[dplR]{detrend}}),
and averaged to a chronology using a robust mean
\code{\link[dplR]{chron}}.
}
\keyword{datasets}

