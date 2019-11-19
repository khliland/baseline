%%% $Id: baseline.irls.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline.irls}
\alias{baseline.irls}
\alias{irls}

\title{ Iterative Restricted Least Squares }
\description{
  An algorithm with primary smoothing and repeated baseline suppressions and
  regressions with 2nd derivative constraint
}
\usage{
baseline.irls(spectra, lambda1 = 5, lambda2 = 9, maxit = 200, wi = 0.05)
}
\arguments{
  \item{spectra}{ Matrix with spectra in rows }
  \item{lambda1}{ 2nd derivative constraint for primary smoothing}
  \item{lambda2}{ 2nd derivative constraint for secondary smoothing}
  \item{maxit}{ Maximum number of iterations }
  \item{wi}{ Weighting of positive residuals }
}
\value{
  \item{baseline }{Matrix of baselines corresponding to spectra \code{spectra}}
  \item{corrected }{Matrix of baseline corrected spectra}
  \item{smoothed }{Matrix of primary smoothed spectra}
}
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
data(milk)
bc.irls <- baseline(milk$spectra[1,, drop=FALSE], method='irls')
\dontrun{
	plot(bc.irls)
}
}
\keyword{ baseline }
\keyword{ spectra }
