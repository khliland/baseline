%%% $Id: overall.min.Rd 193 2012-06-24 21:13:42Z kristl $
\name{overall.min}
\alias{overall.min}
\title{
  Extract the minimum from a baseline optimisation
}
\description{
  Takes the result of an optimisation (a call to \code{\link{doOptim}})
  and extracts the minimum quality measure value along with the
  parameters giving rise to the value.
}
\usage{
overall.min(results)
}
\arguments{
  \item{results}{Result of call to \code{\link{doOptim}}}
}
\value{
  A list with components
  \item{qualMeas}{The minimum quality measure value}
  \item{algorithm}{The name of the baseline algorithm corresponding to
    the minimum}
  \item{param}{A list with the parameter values corresponding to the
  minimum quality measure value}
}
\author{Bjørn-Helge Mevik and Kristian Hovde Liland}
\seealso{
  \code{\link{doOptim}}
}
\keyword{baseline}
\keyword{spectra}
