%%% $Id: baseline.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baselineEnv}
\alias{baselineEnv}
\alias{putBaselineEnv}
\alias{getBaselineEnv}
\title{ Baseline environment }
\description{
  Methods to access the baseline environment.
}
\usage{
baselineEnv()
getBaselineEnv(x, mode="any")
putBaselineEnv(x, value)
}
\arguments{
  \item{x}{ Name of object to put/get. }
  \item{mode}{ Mode of object to get. }
  \item{value}{ Object to put. }
}
\value{
  \code{getBaseline} retrieves an object.
}

\seealso{The functions implementing the baseline algorithms:
  \code{\link{baseline.als}}, \code{\link{baseline.fillPeaks}},
  \code{\link{baseline.irls}}, \code{\link{baseline.lowpass}},
  \code{\link{baseline.medianWindow}},
  \code{\link{baseline.modpolyfit}},
  \code{\link{baseline.peakDetection}},
  \code{\link{baseline.rfbaseline}}, \code{\link{baseline.rollingBall}}
}
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
putBaselineEnv('fish', '<==x-<')
getBaselineEnv('fish')
}

\keyword{ baseline }
\keyword{ environment }
