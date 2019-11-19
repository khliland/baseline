%%% $Id: param.Rd 193 2012-06-24 21:13:42Z kristl $
\name{param}
\alias{param}
\alias{param-methods}
\alias{param,baselineAlg-method}
\alias{param,baselineAlgResult-method}
\alias{param,baselineAlgTest-method}
\alias{param,predictionResult-method}
\title{Extract the "param" slot}
\description{
  Extracts the \code{param} slot of the object.
}
\usage{
param(object)
}
\arguments{
  \item{object}{An object of class \code{\linkS4class{baselineAlg}},
    \code{\linkS4class{baselineAlgTest}},
    \code{\linkS4class{baselineAlgResult}} or
    \code{\linkS4class{predictionResult}}.}
}
\value{
  The \code{param} slot of the object.  Usually a data frame, list or numeric.
}
\author{Bjørn-Helge Mevik and Kristian Hovde Liland}
\seealso{
  Classes \code{\linkS4class{baselineAlg}},
  \code{\linkS4class{baselineAlgTest}},
  \code{\linkS4class{baselineAlgResult}},
  \code{\linkS4class{predictionResult}}
}
\keyword{baseline}
\keyword{spectra}
\keyword{methods}
