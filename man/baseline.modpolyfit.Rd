%%% $Id: baseline.modpolyfit.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline.modpolyfit}
\alias{baseline.modpolyfit}
\alias{modpolyfit}

\title{ Modified polynomial fitting }
\description{
  An implementation of CHAD A. LIEBER and ANITA MAHADEVAN-JANSENs algorithm
  for polynomial fiting
}
\usage{
baseline.modpolyfit(spectra, t, degree = 4, tol = 0.001, rep = 100)
}
\arguments{
  \item{spectra}{ Matrix with spectra in rows }
  \item{t}{ Optional vector of spectrum abcissa }
  \item{degree}{ Degree of polynomial }
  \item{tol}{ Tolerance of difference between iterations }
  \item{rep}{ Maximum number of iterations }
}
\details{
  Polynomial fitting with baseline suppression relative to original spectrum
}
\value{
  \item{baseline }{Matrix of baselines corresponding to spectra \code{spectra}}
  \item{corrected }{Matrix of baseline corrected spectra}
}
\references{ CHAD A. LIEBER and ANITA MAHADEVAN-JANSEN:
  Automated Method for Subtraction of Fluorescence from
  Biological Raman Spectra }
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
data(milk)
bc.modpolyfit <- baseline(milk$spectra[1,, drop=FALSE], method='modpolyfit', deg=6)
\dontrun{
	plot(bc.modpolyfit)
}
}

\keyword{ baseline }
\keyword{ spectra }
