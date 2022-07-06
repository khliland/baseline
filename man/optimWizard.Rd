%%% $Id: optimWizard.Rd 193 2012-06-24 21:13:42Z kristl $
\name{optimWizard}
\alias{optimWizard}
\alias{getOptim}
\alias{getOptimRes}

\title{ Visual tool for setting up optimization }
\description{
  Set up optimization through a graphical user interface. Optionally collecting values directly from 'baselineGUI'.
  Retrieve optimisation parameters and results with \code{getOptim} and \code{getOptimRes}, respectively.
}
\usage{
optimWizard(X, y, postproc, predictionTest, cvsegments)
getOptim()
getOptimRes()
}
\arguments{
  \item{X}{ Matrix with spectra in rows }
  \item{y}{ Response vector or matrix in analysis }
  \item{postproc}{ Custum function for post processing of spectra (optional) }
  \item{predictionTest}{ Custom prediction object (optional) }
  \item{cvsegments}{ Cross-validation segments (optional)}
}
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
\dontrun{
# Computationally intensive
data(milk)
X <- milk$spectra[,-1]
y <- milk$spectra[,1]
optimWizard(X,y)

# Retrieve optimisation
myResults <- getOptimRes()

# After optimisation is complete
plotOptim(myResults)
}
}
\keyword{ baseline }
\keyword{ spectra }