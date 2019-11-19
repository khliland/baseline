\name{getBaseline}
\alias{getBaseline}
\alias{getSpectra}
\alias{getCorrected}
\alias{getCall}
\title{
Functions to extract the components of a "baseline" object
}
\description{
The functions extract the \code{baseline}, \code{spectra},
\code{corrected} or \code{call} slot of a \code{\linkS4class{baseline}}
object; usually the result of a call to \code{\link{baseline}}.
}
\usage{
getBaseline(object)
getSpectra(object)
getCorrected(object)
getCall(object)
}
\arguments{
  \item{object}{A \code{\linkS4class{baseline}} object}
}
\value{
  \code{getCall} returns the \code{baseline} call used to create the
  object.  The other functions return a matrix with the original
  spectra, estimated baselines or corrected spectra.
}
\author{Bjørn-Helge Mevik and Kristian Hovde Liland}

\section{Warning}{In a future versoion, one of the slots might be
  removed from the class definition and calculated on the fly instead,
  in order to save space.  Therefore, \emph{do} use the extractor
  functions (\code{getSpectra}, \code{getBaseline} and
  \code{getCorrected}) instead of accessing the slots directly.}

\seealso{
  The function \code{\link{baseline}}, the class \code{\linkS4class{baseline}}
}
\examples{
data(milk)
bl <- baseline(milk$spectra[1:2,])
baseline  <- getBaseline(bl)
spectra   <- getSpectra(bl)
corrected <- getCorrected(bl)
call      <- getCall(bl)
}
\keyword{ spectra }
\keyword{ baseline }
