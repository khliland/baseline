%%% $Id: baselineAlgResult-class.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baselineAlgResult-class}
\Rdversion{1.1}
\docType{class}
\alias{baselineAlgResult-class}

\title{Class "baselineAlgResult"}
\description{
  A class describing the result of a baseline algorithm test
}
\section{Objects from the Class}{
Objects are typically created by running \code{runTest} on a
\code{\linkS4class{baselineAlgTest}} object.
}
\section{Slots}{
  \describe{
    \item{\code{param}:}{A named list with the parameter values that
      were tested.  This includes both the predictor parameters and the
      baseline algorithm parameters.  All combinations of values are tested.}
    \item{\code{qualMeas}:}{A matrix of quality measure values for the
      different combinations of parameter values.  Each row corresponds
      to one prediction parameter value, and each coloumn to one
      combination of baseline parameters.}
    \item{\code{qualMeas.ind.min}:}{The index in \code{qualMeas}
      of the minimum quality measure value}
    \item{\code{minQualMeas}:}{The minimum quality measure value}
    \item{\code{param.ind.min}:}{A vector of indices into the elemets of
      \code{param} of the parameter values corresponding to the minimum
      quality measure value}
    \item{\code{param.min}:}{A list of the parameter values
      corresponding to the minimum quality measure value}
    \item{\code{qualMeasName}:}{The name of the quality measure}
  }
}
\section{Methods}{
  \describe{
    \item{minQualMeas}{\code{signature(object = "baselineAlgResult")}:
      Extract the \code{minQualMeas} slot }
    \item{param}{\code{signature(object = "baselineAlgResult")}:
      Extract the \code{param} slot }
    \item{param.ind.min}{\code{signature(object = "baselineAlgResult")}:
      Extract the \code{param.ind.min} slot }
    \item{param.min}{\code{signature(object = "baselineAlgResult")}:
      Extract the \code{param.min} slot }
    \item{qualMeas}{\code{signature(object = "baselineAlgResult")}:
      Extract the \code{qualMeas} slot }
    \item{qualMeas.ind.min}{\code{signature(object = "baselineAlgResult")}:
      Extract the \code{qualMeas.ind.min} slot }
    \item{qualMeasName}{\code{signature(object = "baselineAlgResult")}:
      Extract the \code{qualMeasName} slot }
	 }
}
\author{Bjørn-Helge Mevik and Kristian Hovde Liland}
\seealso{
  Class \code{\linkS4class{baselineAlgTest}}, function \code{runTest}.
}
\examples{
showClass("baselineAlgResult")
}
\keyword{classes}
