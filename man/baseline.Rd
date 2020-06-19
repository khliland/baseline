%%% $Id: baseline.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline}
\alias{baseline}
\title{ Baseline correction }
\description{
  Common framework for baseline correction
}
\usage{
baseline(spectra, method = "irls", ...)
}
\arguments{
  \item{spectra}{ Matrix with spectra in rows }
  \item{method}{ Baseline correction method }
  \item{\dots}{ Additional parameters, sent to the method }
}
\details{
  Estimates baselines for the \code{spectra}, using the algorithm named
  in \code{method}.
}
\value{
  An object of class \code{\linkS4class{baseline}}.
}
\references{
Kristian Hovde Liland, Trygve Almøy, Bjørn-Helge Mevik (2010), 
Optimal Choice of Baseline Correction for Multivariate Calibration of Spectra, 
Applied Spectroscopy 64, pp. 1007-1016.
}
\seealso{The functions implementing the baseline algorithms:
  \code{\link{baseline.als}}, \code{\link{baseline.fillPeaks}},
  \code{\link{baseline.irls}}, \code{\link{baseline.lowpass}},
  \code{\link{baseline.medianWindow}},
  \code{\link{baseline.modpolyfit}},
  \code{\link{baseline.peakDetection}},
  \code{\link{baseline.rfbaseline}}, \code{\link{baseline.rollingBall}},
  \code{\link{baseline.shirley}},  \code{\link{baseline.TAP}}
}
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
# Load data
data(milk)
# The baseline() function is an S4 wrapper for all the different 
# baseline correction methods. The default correction method
# is IRLS. Data must be organized as row vectors in a matrix
# or data.frame.
bc.irls <- baseline(milk$spectra[1,, drop=FALSE])
\dontrun{
  # Computationally heavy
	plot(bc.irls)
}

# Available extractors are:
# getBaseline(bc.irls)
# getSpectra(bc.irls)
# getCorrected(bc.irls)
# getCall(bc.irls)

# Correction methods and parameters can be specified through the wrapper.
bc.fillPeaks <- baseline(milk$spectra[1,, drop=FALSE], lambda=6,
	hwi=50, it=10, int=2000, method='fillPeaks')
\dontrun{
  # Computationally heavy
	plot(bc.fillPeaks)
}

# If a suitable gWidgets2 implementation is installed, a 
# graphical user interface is available for interactive
# parameter adaption.
\dontrun{
  # Dependent on external software
  baselineGUI(milk$spectra)
}
}

\keyword{ baseline }
\keyword{ spectra }
