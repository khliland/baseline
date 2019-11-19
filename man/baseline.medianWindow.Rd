%%% $Id: baseline.medianWindow.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline.medianWindow}
\alias{baseline.medianWindow}
\alias{medianWindow}

\title{ Median window }
\description{
  An implementation and extention of Mark S. Friedrichs' model-free algorithm
}
\usage{
baseline.medianWindow(spectra, hwm, hws, end)
}
\arguments{
  \item{spectra}{ Matrix with spectra in rows }
  \item{hwm}{ Window half width for local medians }
  \item{hws}{ Window half width for local smoothing (optional) }
  \item{end}{ Original endpoint handling (optional boolean) }
}
\details{
  An algorithm finding medians in local windows and smoothing with gaussian
  weighting
}
\value{
  \item{baseline }{Matrix of baselines corresponding to spectra \code{spectra}}
  \item{corrected }{Matrix of baseline corrected spectra}
}
\references{ Mark S. Friedrichs:
  A model-free algorithm for the removal of baseline artifacts }
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
data(milk)
bc.medianWindow <- baseline(milk$spectra[1,, drop=FALSE], hwm=300,
	method='medianWindow')
\dontrun{
	plot(bc.medianWindow)
}
}

\keyword{ baseline }
\keyword{ spectra }
