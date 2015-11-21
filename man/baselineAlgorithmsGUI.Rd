\name{baselineAlgorithmsGUI}
\alias{baselineAlgorithmsGUI}
\docType{data}
\title{
  List of available baseline algorithms for GUI function
}
\description{
  A list with data.frames containing parameters, minimum and maximum
  values for GUIs, step lengths for sliders, default values and currently
  selected values, plus a short description of each parameter. The list
  is used by the GUIs, and is user customizable.
}
\details{
  The list is not meant for usage by end-users, but is extendable
  and customizable, allowing for extra algorithms, removal of algoritms
  or changing of parameter sets.
}
\examples{
## Get a list of all algorithms:
names(baselineAlgorithmsGUI)
## Add new algorithm:
baselineAlgorithmsGUI$my.alg <- as.data.frame(matrix(c(0,20,1,1, 0,20,1,1), 2,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$my.alg) <- list(par=c("kappa", "gamma"),
	val=c("min","max","step","default"))
baselineAlgorithmsGUI$my.alg$current <- c(1,1)
baselineAlgorithmsGUI$my.alg$name <- c("Subtractive constand", "Additive constant")
}
\keyword{baseline}
