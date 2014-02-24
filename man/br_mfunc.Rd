\name{br_mfunc}
\alias{br_mfunc}
\title{moving (bootstrapped) response and correlation function}
\usage{
br_mfunc(chrono, climate, boot, sb, start_last, win_size, win_offset, ci,
  method)
}
\arguments{
  \item{chrono}{tree-ring chronology}

  \item{climate}{data.frame with climate parameters}

  \item{sb}{logical: draw statusbar or not?}

  \item{start_last}{logical: start with last (oldest)
  window?}

  \item{win_size}{numeric: size of the moving in years}

  \item{win_offset}{numeric: size of offset between moving
  windows in years}

  \item{ci}{numeric: p-level for confidence interval (must
  be in c(0.1, 0.05, 0.01)}

  \item{method}{character: method to be used (one of
  "response" or "correlation")}
}
\description{
see doc for dcc for details.
}

