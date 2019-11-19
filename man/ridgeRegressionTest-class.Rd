%%% $Id: ridgeRegressionTest-class.Rd 193 2012-06-24 21:13:42Z kristl $
\name{ridgeRegressionTest-class}
\Rdversion{1.1}
\docType{class}
\alias{ridgeRegressionTest-class}

\title{Class "ridgeRegressionTest"}
\description{
  A class describing a ridge regression test.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ridgeRegressionTest", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{lambda}:}{Numeric vector.  The smoothing parameter
      values to test}
  }
}
\section{Extends}{
Class \code{\linkS4class{predictionTest}}, directly.
}
\section{Methods}{
  \describe{
    \item{runTest}{\code{signature(object = "ridgeRegressionTest")}: Run
      the test}
  }
}
\author{Bjørn-Helge Mevik and Kristian Hovde Liland}
\seealso{
  The base class \code{\linkS4class{predictionTest}}.  The
  \code{\link{runTest}} function.
}
\examples{
showClass("ridgeRegressionTest")
}
\keyword{classes}
