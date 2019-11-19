%%% $Id: baselineAlgTest-class.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baselineAlgTest-class}
\Rdversion{1.1}
\docType{class}
\alias{baselineAlgTest-class}

\title{Class "baselineAlgTest"}
\description{
  A class that describes a baseline algorithm test.  The test is
  performed with the function \code{runTest}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("baselineAlgTest", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{algorithm}:}{A \code{"baselineAlg"} object.  The
      baseline algorithm to test.}
    \item{\code{param}:}{A named list with parameter values to test.
      All combinations of parameters are tested.}
    \item{\code{extraArgs}:}{A named list of extra parameters to the
      baseline algorithm.  These will be held fixed during the testing.}
  }
}
\section{Methods}{
  \describe{
    \item{algorithm}{\code{signature(object = "baselineAlgTest")}:
      Extract the \code{algorithm} slot }
    \item{extraArgs}{\code{signature(object = "baselineAlgTest")}:
      Extract the \code{extraArgs} slot ... }
    \item{funcName}{\code{signature(object = "baselineAlgTest")}:
      Extract the \code{funcName} slot ... }
    \item{param}{\code{signature(object = "baselineAlgTest")}:
      Extract the \code{param} slot }
    \item{runTest}{\code{signature(object = "baselineAlgTest")}: Run the
      test.}
  }
}
\author{Bjørn-Helge Mevik and Kristian Hovde Liland}
\seealso{
  Classes \code{\linkS4class{baselineAlg}},
  \code{\linkS4class{baselineAlgResult}}.  Function \code{runTest}.
}
\examples{
showClass("baselineAlgTest")
}
\keyword{classes}
