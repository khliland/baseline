%%% $Id: qualMeas.Rd 193 2012-06-24 21:13:42Z kristl $
\name{qualMeas}
\alias{qualMeas}
\alias{qualMeas-methods}
\alias{qualMeas,baselineAlgResult-method}
\alias{qualMeas,predictionResult-method}
\alias{minQualMeas}
\alias{minQualMeas-methods}
\alias{minQualMeas,baselineAlgResult-method}
\alias{minQualMeas,predictionResult-method}
\alias{param.min}
\alias{param.min-methods}
\alias{param.min,baselineAlgResult-method}
\alias{param.min,predictionResult-method}
\alias{qualMeasName}
\alias{qualMeasName-methods}
\alias{qualMeasName,baselineAlgResult-method}
\alias{qualMeasName,predictionResult-method}
\title{Extraction functions for "predictionResult" or "baselineAlgResult"
  objects}
\description{
  Extract slots from objects of class
  \code{\linkS4class{predictionResult}} or
  \code{\linkS4class{baselineAlgResult}}.
}
\usage{
qualMeas(object, ...)
\S4method{qualMeas}{predictionResult}(object, ...)
\S4method{qualMeas}{baselineAlgResult}(object, ..., MIN, AVG,
  DEFAULT = c("all", "cond.min", "overall.min", "avg"))
minQualMeas(object)
param.min(object)
qualMeasName(object)
}
\arguments{
  \item{object}{An object of class \code{\linkS4class{predictionResult}} or
    \code{\linkS4class{baselineAlgResult}}}
  \item{MIN}{List or vector of parameter names to take the minimum
    over.  Not used if \code{DEFAULT} is \code{"cond.min"}.  See Details}
  \item{AVG}{List or vector of parameter names to take the average
    over.  Not used if \code{DEFAULT} is \code{"avg"}.  See Details}
  \item{DEFAULT}{Character string.  The default way to calculate the minimum
    (or average) for all parameters.  See Details}
  \item{\dots}{Other arguments. Selection of subsets of parameter levels.  See Details}
}
\details{
  The arguments to the \code{\linkS4class{baselineAlgResult}} method are
  interpreted in the following way:
  
  Subsets of parameters levels can be selected by supplying their names
    and specifying the level indices as vectors. Substituting a vector
	with \code{"all"} will return all levels of the corresponding parameter, and
	substituting it with \code{"overall"} will return the level corresponding
	to the overall minimum.
  Minimum and average values for selected parameters can be chosen using
	\code{MIN} and \code{AVG}, respectively, together with a vector of parameter names.
	
  \code{DEFAULT} specifies the action for each remaining parameters:
    If \code{"all"} (default): returns all levels.  If \code{"cond.min"}: 
    take minimum for each remaining parameter (MIN is not used).  If
    \code{"overall.min"}: set any remaining parameters to their value
    corresponding to the overall min. If \code{"avg"}: take average for
	each remaining parameter (AVG is not used).
}
\value{
  The \code{qualMeas} method for \code{\linkS4class{baselineAlgResult}} objects returns
  the subsets or minimum values of the \code{qualMeas} slot of the object as specified
  above. All other methods simply return the corresponding slot.
}
\author{Bjørn-Helge Mevik and Kristian Hovde Liland}

\seealso{
  Function \code{\link{runTest}}, classes
  \code{\linkS4class{baselineAlgResult}} and \code{\linkS4class{predictionResult}}
}
\keyword{methods}
\keyword{baseline}
\keyword{spectra}
