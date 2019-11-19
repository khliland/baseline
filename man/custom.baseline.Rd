\name{custom.baseline}
\alias{custom.baseline}
\title{
Customized baseline correction
}
\description{
This function rescales spectrum abscissa by use of \code{breaks} and
\code{gaps} before baseline correction. The effect is that the
chosen baseline correction algorithm and paramters will have varying
effects along the spectra, effectively giving local control of the amount
of rigidity/flexibility of the estimated baseline.
}
\usage{
custom.baseline(spectra, breaks, gaps, trans.win = NULL, just.plot = FALSE, method, ...)
}
\arguments{
  \item{spectra}{
		Matrix with spectra in rows.
}
  \item{breaks}{
		Vector of locations of break points between sections of varying
		baseline flexibility (given as abscissa numbers).
}
  \item{gaps}{
		Vector giving the abscissa spacing between each instance of
		\code{breaks} (and endpoints if not specified in \code{breaks}).
}
  \item{trans.win}{
		Optional width of transition window around break points used
		for smoothing rough breaks by LOWESS (default = NULL).
}
  \item{just.plot}{
		Plot the rescaled spectra instead of applying the customized
		baseline correction if \code{just.plot}=TRUE (default = FALSE).
}
  \item{method}{
		Baseline correction method to use (class character).
}
  \item{\dots}{
		Additional named arguments to be passed to the baseline correction method.
}
}
\details{
		This is an implementation of the customized baseline correction suggested
		by Liland et al. 2011 for local changes in baseline flexibility.
}
\value{
  \item{baseline }{Estimated custom baselines.}
  \item{corrected }{Spectra corrected by custom baselines.}
  \item{spectra.scaled }{Re-scaled spectra.}
  \item{baseline.scaled }{Estimated baselines of re-scaled spectra.}
}
\references{ Kristian Hovde Liland et al.:
	Customized baseline correction
}
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
data(milk)
spectrum1  <- milk$spectra[1,1:10000,drop=FALSE]
ordinary   <- baseline(spectrum1, method="als", lambda=6, p=0.01)
customized <- custom.baseline(spectrum1, 2900, c(1,20), trans.win=100, 
	just.plot=FALSE, method="als", lambda=6, p=0.01)
\dontrun{
plot(1:10000,spectrum1, type='l')
lines(1:10000,getBaseline(ordinary), lty=2, col=2, lwd=2)
lines(1:10000,customized$baseline, lty=3, col=3, lwd=2)
}
}
\keyword{ baseline }
\keyword{ spectra }
