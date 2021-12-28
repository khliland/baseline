

#' Extraction methods for "baselineAlgTest" objects
#' 
#' Extraction methods specifically for objects of class
#' \code{\linkS4class{baselineAlgTest}}
#' 
#' 
#' @aliases algorithm algorithm-methods algorithm,baselineAlgTest-method
#' extraArgs extraArgs-methods extraArgs,baselineAlgTest-method
#' @param object Object of class \code{\linkS4class{baselineAlgTest}}
#' @return The corresponding slot
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso \code{\linkS4class{baselineAlgTest}}
#' @keywords baseline spectra
NULL





#' Class "baselineAlg"
#' 
#' A class that describes a baseline correction algorithm.  The idea is that it
#' contains all information needed to use an algorithm with the optimisation
#' framework and the graphical user interface (but see Notes below).
#' 
#' 
#' @name baselineAlg-class
#' @docType class
#' @note The goal is that the optimisation framework and the GUI code should
#' get all information about available baseline algorithms through a list of
#' \code{baselineAlg} objects.  This will make it relatively simple to add new
#' baseline algorithms.
#' 
#' Currenly, there is information about the algorithms spread around in the
#' code.  We plan to move that information into the \code{baselineAlg} objects,
#' and expand the class accordingly.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("baselineAlg", ...)}.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @keywords classes
#' @examples
#' 
#' showClass("baselineAlg")
#' 
NULL





#' List of available baseline algorithms
#' 
#' A list with descriptions of all baseline algorithms available through the
#' optimisaiont framework and graphical user interface.  The elements of the
#' list are \code{\linkS4class{baselineAlg}} objects.  The list is used by the
#' code to extract names and information about the baseline algorithms.
#' 
#' The list is not meant for usage by end-users, but is extendable and
#' customizable, allowing for extra algorithms or removal of algoritms.
#' 
#' The names of the list must match the \code{name} slot of the elements.
#' 
#' @name baselineAlgorithms
#' @docType data
#' @keywords baseline
#' @examples
#' 
#' ## Get a list of all algorithms:
#' names(baselineAlgorithms)
#' ## Show the descriptions
#' sapply(baselineAlgorithms, description)
#' ## Add new algorithm
#' baseline.my.alg <- function(spectra, kappa=1, gamma=1){
#'    baseline  <- spectra-kappa+gamma
#'    corrected <- spectra-baseline
#'    list(baseline=baseline,corrected=corrected)
#' }
#' 
#' baselineAlgorithms$my.alg = new("baselineAlg",
#'      name = "my.alg",
#'      description = "A new baseline correction algorithm",
#'      funcName = "baseline.my.alg",
#'      param = data.frame(
#'         name = c("kappa","gamma"), # maxit
#'         integer = c(FALSE, FALSE),
#'         min = c(0, 0),
#'         incl.min = c(TRUE, TRUE),
#'         default = c(1, 1),
#'         max = c(Inf, 1),
#'         incl.max = c(FALSE, TRUE)
#'     ))
#' 
NULL





#' List of available baseline algorithms for GUI function
#' 
#' A list with data.frames containing parameters, minimum and maximum values
#' for GUIs, step lengths for sliders, default values and currently selected
#' values, plus a short description of each parameter. The list is used by the
#' GUIs, and is user customizable.
#' 
#' The list is not meant for usage by end-users, but is extendable and
#' customizable, allowing for extra algorithms, removal of algoritms or
#' changing of parameter sets.
#' 
#' @name baselineAlgorithmsGUI
#' @docType data
#' @keywords baseline
#' @examples
#' 
#' ## Get a list of all algorithms:
#' names(baselineAlgorithmsGUI)
#' ## Add new algorithm:
#' baselineAlgorithmsGUI$my.alg <- as.data.frame(matrix(c(0,20,1,1, 0,20,1,1), 2,4, byrow=TRUE))
#' dimnames(baselineAlgorithmsGUI$my.alg) <- list(par=c("kappa", "gamma"),
#' 	val=c("min","max","step","default"))
#' baselineAlgorithmsGUI$my.alg$current <- c(1,1)
#' baselineAlgorithmsGUI$my.alg$name <- c("Subtractive constand", "Additive constant")
#' 
NULL





#' Class "baselineAlgResult"
#' 
#' A class describing the result of a baseline algorithm test
#' 
#' 
#' @name baselineAlgResult-class
#' @docType class
#' @section Objects from the Class: Objects are typically created by running
#' \code{runTest} on a \code{\linkS4class{baselineAlgTest}} object.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso Class \code{\linkS4class{baselineAlgTest}}, function
#' \code{runTest}.
#' @keywords classes
#' @examples
#' 
#' showClass("baselineAlgResult")
#' 
NULL





#' Class "baselineAlgTest"
#' 
#' A class that describes a baseline algorithm test.  The test is performed
#' with the function \code{runTest}.
#' 
#' 
#' @name baselineAlgTest-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("baselineAlgTest", ...)}.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso Classes \code{\linkS4class{baselineAlg}},
#' \code{\linkS4class{baselineAlgResult}}.  Function \code{runTest}.
#' @keywords classes
#' @examples
#' 
#' showClass("baselineAlgTest")
#' 
NULL











#' Class "PLSRTest"
#' 
#' A class describing a PLSR prediction test.  To run the test, the "pls"
#' package must be installed.
#' 
#' 
#' @name PLSRTest-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("PLSRTest", ...)}.
#' @author Bjørn-Helge Mevik and Krisitan Hovde Liland
#' @seealso The base class \code{\linkS4class{predictionTest}}.  The
#' \code{\link{runTest}} function.  The \code{\link[pls:mvr]{plsr}} function
#' from the "pls" package.
#' @keywords classes
#' @examples
#' 
#' showClass("PLSRTest")
#' 
NULL





#' Class "predictionResult"
#' 
#' A class containing the result of running a
#' \code{\linkS4class{predictionTest}}.
#' 
#' 
#' @name predictionResult-class
#' @docType class
#' @section Objects from the Class: The normal way to create objects is by
#' calling the method \code{runTest} for any object of subclass of
#' \code{\linkS4class{predictionTest}}.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso Function \code{\link{runTest}}, class
#' \code{\linkS4class{predictionTest}}, subclasses
#' \code{\linkS4class{PLSRTest}} and \code{\linkS4class{ridgeRegressionTest}}
#' @keywords classes
#' @examples
#' 
#' showClass("predictionResult")
#' 
NULL





#' Class "predictionTest"
#' 
#' A virtual class for all predictor test subclasses.  Currently subclasses
#' \code{\linkS4class{PLSRTest}} and \code{\linkS4class{ridgeRegressionTest}}
#' are defined.
#' 
#' 
#' @name predictionTest-class
#' @docType class
#' @section Objects from the Class: A virtual Class: No objects may be created
#' from it.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso Subclasses \code{\linkS4class{PLSRTest}} and
#' \code{\linkS4class{ridgeRegressionTest}}.
#' @keywords classes
NULL





#' Class "ridgeRegressionTest"
#' 
#' A class describing a ridge regression test.
#' 
#' 
#' @name ridgeRegressionTest-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ridgeRegressionTest", ...)}.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso The base class \code{\linkS4class{predictionTest}}.  The
#' \code{\link{runTest}} function.
#' @keywords classes
#' @examples
#' 
#' showClass("ridgeRegressionTest")
#' 
NULL





#' Run a predictionTest or baselineAlgTest
#' 
#' Runs the test defined in a \code{\linkS4class{predictionTest}} or
#' \code{\linkS4class{baselineAlgTest}} object
#' 
#' 
#' @aliases runTest runTest-methods runTest,baselineAlgTest-method
#' runTest,PLSRTest-method runTest,ridgeRegressionTest-method
#' @param object An object of class \code{\linkS4class{baselineAlgTest}} or
#' subclass of \code{\linkS4class{predictionTest}} (currently
#' \code{\linkS4class{PLSRTest}} or \code{\linkS4class{ridgeRegressionTest}}).
#' The object specify the test to be run
#' @param X A matrix.  The spectra to use in the test
#' @param y A vector or matrix.  The response(s) to use in the test
#' @param predictionTest A \code{\linkS4class{predictionTest}} object,
#' describing the prediction test to use for this baseline algorithm test
#' @param postproc A function, used to postprocess the baseline corrected
#' spectra prior to prediction testing.  The function should take a matrix of
#' spectra as its only argument, and return a matrix of postprocessed spectra
#' @param verbose Logical, specifying whether the test should print out
#' progress information.  Default is \code{FALSE}
#' @param \dots Other arguments.  Currently only used by the
#' \code{\linkS4class{baselineAlgTest}} method.
#' @return \code{runTest} returns an object of class
#' \code{\linkS4class{predictionResult}} or
#' \code{\linkS4class{baselineAlgResult}}.
#' @section Methods: \describe{
#' 
#' \item{list("signature(object = \"baselineAlgTest\")")}{Baseline corrects the
#' spectra, optionally postprocesses them, and runs a prediction test on the
#' corrected spectra.}
#' 
#' \item{list("signature(object = \"PLSRTest\")")}{Runs PLSR on the data and
#' calculates the cross-validated RMSEP}
#' 
#' \item{list("signature(object = \"ridgeRegressionTest\")")}{Runs ridge
#' regression on the data and calculates the GCV} }
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso \code{\linkS4class{baselineAlgTest}},
#' \code{\linkS4class{predictionTest}}, \code{\linkS4class{PLSRTest}},
#' \code{\linkS4class{ridgeRegressionTest}}
#' @keywords spectra baseline methods
NULL

