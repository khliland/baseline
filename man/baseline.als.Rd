%%% $Id: baseline.als.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline.als}
\alias{baseline.als}
\alias{als}

\title{ Asymmetric Least Squares }
\description{
  Baseline correction by 2nd derivative constrained weighted regression.
  Original algorithm proposed by Paul H. C. Eilers and Hans F.M. Boelens
}
\usage{
baseline.als(spectra, lambda = 6, p = 0.05, maxit = 20)
}

\arguments{
  \item{spectra}{ Matrix with spectra in rows }
  \item{lambda}{ 2nd derivative constraint }
  \item{p}{ Weighting of positive residuals }
  \item{maxit}{ Maximum number of iterations }
}
\details{
  Iterative algorithm applying 2nd derivative constraints. Weights from previous
  iteration is \code{p} for positive residuals and \code{1-p} for negative residuals.
}
\value{
  \item{baseline }{Matrix of baselines corresponding to spectra \code{spectra}}
  \item{corrected }{Matrix of baseline corrected spectra}
  \item{wgts }{Matrix of final regression weights}
}
\references{ Paul H. C. Eilers and Hans F.M. Boelens:
  Baseline Correction with Asymmetric Least Squares Smoothing  }
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
data(milk)
bc.als <- baseline(milk$spectra[1,, drop=FALSE], lambda=10, method='als')
\dontrun{
plot(bc.als)
}
}

\keyword{ baseline }
\keyword{ spectra }
