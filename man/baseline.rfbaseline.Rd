%%% $Id: baseline.rfbaseline.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline.rfbaseline}
\alias{baseline.rfbaseline}
\alias{rfbaseline}

\title{ Robust Baseline Estimation }
\description{
  Wrapper for Andreas F. Ruckstuhl, Matthew P. Jacobson, Robert W. Field,
  James A. Dodd's algorithm based on LOWESS and weighted regression
}
\usage{
baseline.rfbaseline(spectra, span = 2/3, NoXP = NULL, maxit = c(2, 2),
  b = 3.5, weight = NULL, Scale = function(r) median(abs(r))/0.6745,
  delta = NULL, SORT = FALSE, DOT = FALSE, init = NULL)
}
\arguments{
  \item{spectra}{ Matrix with spectra in rows }
  \item{span}{ Amount of smoothing (by fraction of points) }
  \item{NoXP}{ Amount of smoothing (by number of points) }
  \item{maxit}{ Maximum number of iterations in robust fit }
  \item{b}{ Tuning constant in the biweight function }
  \item{weight}{ Optional weights to be given to individual observations }
  \item{Scale}{ S function specifying how to calculate the scale of the residuals }
  \item{delta}{ Nonnegative parameter which may be used to save computation. (See \code{rfbaseline} }
  \item{SORT}{Boolean variable indicating whether x data must be sorted.}
  \item{DOT}{ Disregard outliers totally (boolean) }
  \item{init}{ Values of initial fit }
}
\details{
  Most of the code is the original code as given by the authors. The ability to
  sort by X-values has been removed and ability to handle multiple spectra
  has been added
}
\value{
  \item{baseline }{Matrix of baselines corresponding to spectra \code{spectra}}
  \item{corrected }{Matrix of baseline corrected spectra}
}
\references{ Andreas F. Ruckstuhl, Matthew P. Jacobson, Robert W. Field,
  James A. Dodd:
  Baseline subtraction using robust local regression estimation }
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
data(milk)
bc.rbe <- baseline(milk$spectra[1,, drop=FALSE], method='rfbaseline',
  span=NULL, NoXP=1000)
\dontrun{
	plot(bc.rbe)
}
}

\keyword{ baseline }
\keyword{ spectra }
