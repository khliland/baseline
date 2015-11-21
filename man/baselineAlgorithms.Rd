\name{baselineAlgorithms}
\alias{baselineAlgorithms}
\docType{data}
\title{
  List of available baseline algorithms
}
\description{
  A list with descriptions of all baseline algorithms available through
  the optimisaiont framework and graphical user interface.  The elements
  of the list are \code{\linkS4class{baselineAlg}} objects.  The list is
  used by the code to extract names and information about the baseline
  algorithms.
}
\details{
  The list is not meant for usage by end-users, but is extendable
  and customizable, allowing for extra algorithms or removal of algoritms.

  The names of the list must match the \code{name} slot of the
  elements.
}
\examples{
## Get a list of all algorithms:
names(baselineAlgorithms)
## Show the descriptions
sapply(baselineAlgorithms, description)
## Add new algorithm
baseline.my.alg <- function(spectra, kappa=1, gamma=1){
   baseline  <- spectra-kappa+gamma
   corrected <- spectra-baseline
   list(baseline=baseline,corrected=corrected)
}

baselineAlgorithms$my.alg = new("baselineAlg",
     name = "my.alg",
     description = "A new baseline correction algorithm",
     funcName = "baseline.my.alg",
     param = data.frame(
        name = c("kappa","gamma"), # maxit
        integer = c(FALSE, FALSE),
        min = c(0, 0),
        incl.min = c(TRUE, TRUE),
        default = c(1, 1),
        max = c(Inf, 1),
        incl.max = c(FALSE, TRUE)
    ))
}
\keyword{baseline}
