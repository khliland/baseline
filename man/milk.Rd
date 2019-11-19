%%% $Id: milk.Rd 193 2012-06-24 21:13:42Z kristl $
\name{milk}
\alias{milk}
\docType{data}
\title{ MALDI-TOF mass spectra}
\description{
  Matrix of 45 spectra of 21451 m/z values from MALDI-TOF on mixed milk samples.
}
\usage{data(milk)}
\format{
  A data frame with 45 observations on the following 2 variables.
  \describe{
    \item{\code{cow}}{a numeric vector}
    \item{\code{spectra}}{a matrix with 21451 columns}
  }
}
\details{
	\code{cow} is the concentration of cow milk
	in mixed samples of cow, goat, and ewe milk.
}
\references{
  Kristian Hovde Liland, Bjørn-Helge Mevik, Elling-Olav Rukke, Trygve
  Almøy, Morten Skaugen and Tomas Isaksson (2009)
  Quantitative whole spectrum analysis with MALDI-TOF MS, Part I:
  Measurement optimisation.
  \emph{Chemometrics and Intelligent Laboratory Systems}, \bold{96}(2),
  210--218.
}
\examples{
data(milk)
\dontrun{
plot(milk$spectra[1,], type = "l")
}
}
\keyword{datasets}
