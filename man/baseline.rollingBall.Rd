%%% $Id: baseline.rollingBall.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline.rollingBall}
\alias{baseline.rollingBall}
\alias{rollingBall}

\title{ Rolling ball }
\description{
  Ideas from Rolling Ball algorithm for X-ray spectra by M.A.Kneen and
  H.J. Annegarn. Variable window width has been left out
}
\usage{
baseline.rollingBall(spectra, wm, ws)
}
\arguments{
  \item{spectra}{ Matrix with spectra in rows }
  \item{wm}{ Width of local window for minimization/maximization }
  \item{ws}{ Width of local window for smoothing }
}
\value{
  \item{baseline }{Matrix of baselines corresponding to spectra \code{spectra}}
  \item{corrected }{Matrix of baseline corrected spectra}
}
\references{ M.A. Kneen, H.J. Annegarn:
  Algorithm for fitting XRF, SEM and PIXE X-ray spectra backgrounds }
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
data(milk)
bc.rollingBall <- baseline(milk$spectra[1,, drop=FALSE], wm=200, ws=200,
	method='rollingBall')
\dontrun{
	plot(bc.rollingBall)
}
}

\keyword{ baseline }
\keyword{ spectra }
