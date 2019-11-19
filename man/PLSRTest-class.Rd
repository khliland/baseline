%%% $Id: PLSRTest-class.Rd 193 2012-06-24 21:13:42Z kristl $
\name{PLSRTest-class}
\Rdversion{1.1}
\docType{class}
\alias{PLSRTest-class}

\title{Class "PLSRTest"}
\description{
  A class describing a PLSR prediction test.  To run the test, the
  "pls" package must be installed.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PLSRTest",
  ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{ncomp}:}{Integer vector.  The number of PLSR components
      to test}
    \item{\code{cvsegments}:}{A list of the segments to use in the
      cross-validation}
  }
}
\section{Extends}{
Class \code{\linkS4class{predictionTest}}, directly.
}
\section{Methods}{
  \describe{
    \item{runTest}{\code{signature(object = "PLSRTest")}: Run the test}
  }
}
\author{Bjørn-Helge Mevik and Krisitan Hovde Liland}
\seealso{
  The base class \code{\linkS4class{predictionTest}}.  The
  \code{\link{runTest}} function.  The \code{\link[pls:mvr]{plsr}}
  function from the "pls" package.
}
\examples{
showClass("PLSRTest")
}
\keyword{classes}
