%%% $Id: XPSdata.Rd 193 2012-06-24 21:13:42Z $
\name{XPSdata}
\alias{C1s}
\alias{O1s}
\docType{data}

\title{ XPS core line data}
\description{
  Matrix of x,y values from X-Ray Photoelectron Spectroscopy on test sample.\cr
  The data are about the \code{Carbon} and \code{Oxygen} element for
  \code{1s} shell.
}
\usage{
	data(C1s)
	data(O1s)
	}
\format{
  A matrix with the following 2 variables (rows).
  \describe{
    \item{\code{first row}}{is the abscissa, ( Binding Energy [eV] )}
    \item{\code{second row}}{is the Intensity, ( a.u. )}
  }
}
\seealso{
\link{baseline.shirley}
}
\examples{
data(C1s)
data(O1s)
plot(C1s[1,], C1s[2,], type = "l")
plot(O1s[1,], O1s[2,], type = "l")

}
\keyword{datasets}
