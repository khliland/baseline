%%% $Id: baseline-class.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline-class}
\Rdversion{1.1}
\docType{class}
\alias{baseline-class}
\alias{getBaseline,baseline-method}
\alias{getCall,baseline-method}
\alias{getCorrected,baseline-method}
\alias{getSpectra,baseline-method}

\title{Class "baseline"}
\description{Stores the result of estimating baselines for one or more spectra.}
\section{Objects from the Class}{
The normal way to create objects is with the function
\code{\link{baseline}}.  Several baseline algorithms are available.  See
\code{\link{baseline}} for details.  There is a plot method for the
class; see \code{\link{plot,baseline-method}}.
}
\section{Slots}{
  \describe{
    \item{\code{baseline}:}{A matrix with the estimated baselines}
    \item{\code{corrected}:}{A matrix with the corrected spectra}
    \item{\code{spectra}:}{A matrix with the original spectra}
    \item{\code{call}:}{The call to \code{\link{baseline}}}
  }
}
\section{Methods}{
  \describe{
    \item{getBaseline}{\code{signature(object = "baseline")}: Extract
      the estimated baselines}
    \item{getCall}{\code{signature(object = "baseline")}: Extract the
      call to \code{\link{baseline}} used to create the object}
    \item{getCorrected}{\code{signature(object = "baseline")}: Extract
      the corrected spectra}
    \item{getSpectra}{\code{signature(object = "baseline")}: Extract the
      original spectra}
	 }
}
\author{Bjørn-Helge Mevik and Kristian Hovde Liland}

\section{Warning}{In a future versoion, one of the slots might be
  removed from the class definition and calculated on the fly instead,
  in order to save space.  Therefore, \emph{do} use the extractor
  functions (\code{getSpectra}, \code{getBaseline} and
  \code{getCorrected}) instead of accessing the slots directly.}

\seealso{
  \code{\link{baseline}}, \code{\link{getBaseline}},
  \code{\link{getSpectra}}, \code{\link{getCorrected}},
  \code{\link{getCall}}
}
\examples{
showClass("baseline")
}
\keyword{classes}
