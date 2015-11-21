%%% $Id: doOptim.Rd 193 2012-06-24 21:13:42Z kristl $
\name{doOptim}
\alias{doOptim}
\alias{mvrValstats}
\title{
  Optimise several baseline algorithms on a data set
}
\description{
  Tests several baseline algorithms with one predictor for a given data
  set.  The baseline algorithms are represented as a list of
  \code{\linkS4class{baselineAlgTest}} objects, and the predictor as a
  \code{\linkS4class{predictionTest}} object.
}
\usage{
doOptim(baselineTests, X, y, predictionTest, postproc = NULL,
        tmpfile = "tmp.baseline", verbose = FALSE, cleanTmp = FALSE)
}
\arguments{
  \item{baselineTests}{a list of \code{\linkS4class{baselineAlgTest}}
    objects.  The baseline algorithms and parameter values to test}
  \item{X}{A matrix.  The spectra to use in the test}
  \item{y}{A vector or matrix.  The response(s) to use in the test}
  \item{predictionTest}{A \code{\linkS4class{predictionTest}} object.  The
    predictor and parameter values to use in the test}
  \item{postproc}{A function, used to postprocess the baseline corrected
    spectra prior to prediction testing.  The function should take a
    matrix of spectra as its only argument, and return a matrix of
    postprocessed spectra}
  \item{tmpfile}{The basename of the files used to store intermediate
    calculations for checkpointing.  Defaults to \code{"tmp.baseline"}}
  \item{verbose}{Logical, specifying whether the test should print out
    progress information.  Default is \code{FALSE}}
  \item{cleanTmp}{Logical, specifying whether the intermediate files should
    be deleted when the optimisation has finished.  Default is \code{FALSE}}
}
\details{
  The function loops through the baseline algorithm tests in
  \code{baselineTests}, testing each of them with the given data and
  prediction test, and collects the results.  The results of each
  baseline algorithm test is saved in a temporary file so that if the
  optimisation is interrupted, it can be re-run and will use the
  pre-calculated results.  If \code{cleanTmp} is \code{TRUE}, the temporary
  files are deleted when the whole optimisation has finished.
}
\value{
  A list with components
  \item{baselineTests}{The \code{baselineTests} argument}
  \item{results}{A list with the \code{baselineAlgResult} objects
    for each baseline test}
  \item{minQualMeas}{The minimum quality measure value}
  \item{baselineAlg.min}{The name of the baseline algorithm giving the
    minimum quality measure value}
  \item{param.min}{A list with the parameter values corresponding to the
  minimum quality measure value}
}
\author{Bjørn-Helge Mevik and Kristian Hovde Liland}
\seealso{
  \code{\linkS4class{baselineAlgTest}},\code{\linkS4class{predictionTest}}
}
\keyword{baseline}
\keyword{spectra}
