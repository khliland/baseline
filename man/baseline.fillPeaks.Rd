%%% $Id: baseline.fillPeaks.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline.fillPeaks}
\alias{baseline.fillPeaks}

\title{ Fill peaks }
\description{
  An iterative algorithm using suppression of baseline by means in local windows
}
\usage{
baseline.fillPeaks(spectra, lambda, hwi, it, int)
}
\arguments{
  \item{spectra}{ Matrix with spectra in rows }
  \item{lambda}{ 2nd derivative penalty for primary smoothing}
  \item{hwi}{ Half width of local windows }
  \item{it}{ Number of iterations in suppression loop }
  \item{int}{ Number of buckets to divide spectra into }
}
\details{
  In local windows of buckets the minimum of the mean and the previous iteration
  is chosen as the new baseline
}
\value{
  \item{baseline }{Matrix of baselines corresponding to spectra \code{spectra}}
  \item{corrected }{Matrix of baseline corrected spectra}
}
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
data(milk)
bc.fillPeaks <- baseline(milk$spectra[1,, drop=FALSE], lambda=6,
	hwi=50, it=10, int=2000, method='fillPeaks')
\dontrun{
	plot(bc.fillPeaks)
}
}
\keyword{ baseline }
\keyword{ spectra }
