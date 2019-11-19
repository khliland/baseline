%%% $Id: baseline.lowpass.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline.lowpass}
\alias{baseline.lowpass}
\alias{lowpass}

\title{ Low-pass FFT filter }
\description{
  An algorithm for removing baselines based on Fast Fourier Transform filtering
}
\usage{
baseline.lowpass(spectra, steep = 2, half = 5)
}

\arguments{
  \item{spectra}{ Matrix with spectra in rows }
  \item{steep}{ Steepness of filter curve }
  \item{half}{ Half-way point of filter curve }
}
\details{
  Since the scale of the spectra will be different after filtering, baselines
  will not be returned by the algorithm
}
\value{
  \item{baseline }{Matrix of baselines corresponding to spectra \code{spectra}}
  \item{corrected }{Matrix of baseline corrected spectra}
}
\references{ AHMET K. ATAKAN, W. E. BLASS, and D. E. JENNINGS:
  Elimination of Baseline Variations from a Recorded Spectrum
  by Ultra-low Frequency Filtering }
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
data(milk)
bc.lowpass <- baseline(milk$spectra[1,, drop=FALSE], method='lowpass')
\dontrun{
	plot(bc.lowpass)
}
}
\keyword{ baseline }
\keyword{ spectra }
