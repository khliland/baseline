%%% $Id: runTest.Rd 193 2012-06-24 21:13:42Z kristl $
\name{runTest}
\alias{runTest}
\alias{runTest-methods}
\alias{runTest,baselineAlgTest-method}
\alias{runTest,PLSRTest-method}
\alias{runTest,ridgeRegressionTest-method}
\title{Run a predictionTest or baselineAlgTest}
\description{
Runs the test defined in a \code{\linkS4class{predictionTest}} or
\code{\linkS4class{baselineAlgTest}} object
}
\usage{
runTest(object, X, y, ...)
\S4method{runTest}{PLSRTest}(object, X, y)
\S4method{runTest}{ridgeRegressionTest}(object, X, y)
\S4method{runTest}{baselineAlgTest}(object, X, y, predictionTest, postproc, verbose = FALSE)
}
\arguments{
  \item{object}{An object of class \code{\linkS4class{baselineAlgTest}}
    or subclass of \code{\linkS4class{predictionTest}} (currently
    \code{\linkS4class{PLSRTest}} or
    \code{\linkS4class{ridgeRegressionTest}}).  The object specify the
    test to be run}
  \item{X}{A matrix.  The spectra to use in the test}
  \item{y}{A vector or matrix.  The response(s) to use in the test}
  \item{predictionTest}{A \code{\linkS4class{predictionTest}} object,
    describing the prediction test to use for this baseline algorithm
    test}
  \item{postproc}{A function, used to postprocess the baseline corrected
    spectra prior to prediction testing.  The function should take a
    matrix of spectra as its only argument, and return a matrix of
    postprocessed spectra}
  \item{verbose}{Logical, specifying whether the test should print out
    progress information.  Default is \code{FALSE}}
  \item{\dots}{Other arguments.   Currently only used by the
    \code{\linkS4class{baselineAlgTest}} method.}
}
\section{Methods}{
\describe{

\item{\code{signature(object = "baselineAlgTest")}}{Baseline corrects
  the spectra, optionally postprocesses them, and runs a prediction test
  on the corrected spectra.}

\item{\code{signature(object = "PLSRTest")}}{Runs PLSR
  on the data and calculates the cross-validated RMSEP}

\item{\code{signature(object = "ridgeRegressionTest")}}{Runs ridge
  regression on the data and calculates the GCV}
}}

\value{
\code{runTest} returns an object of class
\code{\linkS4class{predictionResult}} or \code{\linkS4class{baselineAlgResult}}.
}
\author{Bjørn-Helge Mevik and Kristian Hovde Liland}
\seealso{
  \code{\linkS4class{baselineAlgTest}},
  \code{\linkS4class{predictionTest}}, \code{\linkS4class{PLSRTest}},
  \code{\linkS4class{ridgeRegressionTest}}
}
\keyword{spectra}
\keyword{baseline}
\keyword{methods}
