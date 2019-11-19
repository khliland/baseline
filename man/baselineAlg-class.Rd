%%% $Id: baselineAlg-class.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baselineAlg-class}
\Rdversion{1.1}
\docType{class}
\alias{baselineAlg-class}

\title{Class "baselineAlg"}
\description{
  A class that describes a baseline correction algorithm.  The idea is
  that it contains
  all information needed to use an algorithm with the optimisation
  framework and the graphical user interface (but see Notes below).
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("baselineAlg", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Short-name of the algorithm.  This must match
      the name of the object in the \code{baselineAlgorithms} list of
      algorithms, and is used throughout the code to identify the
      algorithm.  It should thus start with a letter and contain only
      letters, digits, underscores ("_") or dots (".").}
    \item{\code{description}:}{Description of the algorithm, typically
      the full name.  This will be used in the code to describe the
      algorith, so it should not be too long, and not contain newline characters.}
    \item{\code{funcName}:}{The name of the function used to estimate
      the baseline.  The function must take an argument
      \code{spectra}, and return a list with the estimated baselines
      (\code{baseline}) original spectra (\code{spectra}) and the
      corrected spectra (\code{corrected}).  It can also take other
      arguments (typically parameters) and return additional components
      in the list.}
    \item{\code{param}:}{A data frame with information about the
      parameters of the algorithm.  It should contain the following
      coloumns: \code{name} - the name of the parameter; \code{integer}
      - \code{TRUE} if the parameter only takes integer values,
      otherwise \code{FALSE}; \code{min} - the lower limit of allowed values;
      \code{incl.min} - \code{TRUE} if the lower limit is an allowed
      value, otherwise \code{FALSE}; \code{default} - the default value;
      \code{max} - the upper limit of allowed values; \code{incl.max} -
      \code{TRUE} if the upper limit is an allowed value, otherwise \code{FALSE}}
  }
}
\section{Methods}{
  \describe{
    \item{description}{\code{signature(object = "baselineAlg")}: Extract
      the \code{description} slot}
    \item{funcName}{\code{signature(object = "baselineAlg")}: Extract
      the \code{funcName} slot }
    \item{name}{\code{signature(object = "baselineAlg")}: Extract
      the \code{name} slot }
    \item{param}{\code{signature(object = "baselineAlg")}: Extract
      the \code{param} slot}
  }
}
\author{Bjørn-Helge Mevik and Kristian Hovde Liland}
\note{The goal is that the
  optimisation framework and the GUI code should get all information about
  available baseline algorithms through a list of \code{baselineAlg}
  objects.  This will make it relatively simple to add new baseline algorithms.

  Currenly, there is information about the algorithms spread around in the
  code.  We plan to move that information into the \code{baselineAlg}
  objects, and expand the class accordingly.
}
\examples{
showClass("baselineAlg")
}
\keyword{classes}
