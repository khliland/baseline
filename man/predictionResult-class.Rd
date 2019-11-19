%%% $Id: predictionResult-class.Rd 193 2012-06-24 21:13:42Z kristl $
\name{predictionResult-class}
\Rdversion{1.1}
\docType{class}
\alias{predictionResult-class}

\title{Class "predictionResult"}
\description{
  A class containing the result of running a
  \code{\linkS4class{predictionTest}}.
}
\section{Objects from the Class}{
  The normal way to create objects is by calling the method
  \code{runTest} for any object of subclass of
  \code{\linkS4class{predictionTest}}.
}
\section{Slots}{
  \describe{
    \item{\code{param}:}{Numeric vector.  The regression parameter values tested.}
    \item{\code{qualMeas}:}{Numeric vector.  The quality measure values
      for each of the values of the \code{param} slot}
    \item{\code{ind.min}:}{The index (into \code{qualMeas}) of the
      minimum quality measure value}
    \item{\code{minQualMeas}:}{The minimum quality measure value}
    \item{\code{param.min}:}{The value of the parameter value
      corresponding to the minimum quality measure value}
    \item{\code{qualMeasName}:}{The name of the quality measure}
    \item{\code{paramName}:}{The name of the regression parameter}
  }
}
\section{Methods}{
  \describe{
    \item{ind.min}{\code{signature(object = "predictionResult")}: Extract
      the \code{ind.min} slot }
    \item{minQualMeas}{\code{signature(object = "predictionResult")}: Extract the \code{minQualMeas} slot}
    \item{param}{\code{signature(object = "predictionResult")}: Extract the \code{param} slot}
    \item{param.min}{\code{signature(object = "predictionResult")}: Extract the \code{param.min} slot}
    \item{paramName}{\code{signature(object = "predictionResult")}: Extract the \code{paramName} slot}
    \item{qualMeas}{\code{signature(object = "predictionResult")}: Extract the \code{qualMeas} slot}
    \item{qualMeasName}{\code{signature(object = "predictionResult")}: Extract the \code{qualMeasName} slot}
	 }
}
\author{Bjørn-Helge Mevik and Kristian Hovde Liland}
\seealso{
  Function \code{\link{runTest}}, class
  \code{\linkS4class{predictionTest}}, subclasses 
  \code{\linkS4class{PLSRTest}} and
  \code{\linkS4class{ridgeRegressionTest}}
}
\examples{
showClass("predictionResult")
}
\keyword{classes}
