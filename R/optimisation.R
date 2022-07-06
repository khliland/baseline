### optimisation.R:  Classes, methods and functions for searching for
### optimal baselines.
### $Id: optimisation.R 179 2011-01-09 14:29:07Z bhm $

## The function mvrValstats has been copied from package pls (overlapping authors)
## because eval(parent.frame()) seems to break call to pls::msep.


###
### Common generic functions and default methods
###


## A function for performing the testing.
## It will have methods for the predictionTest subclasses, and the
## baselineAlgTest class.
#' @name runTest
#' @aliases runTest
#' @title Run a predictionTest or baselineAlgTest
#' @description 
#' Runs the test defined in a \code{\linkS4class{predictionTest}} or  \code{\linkS4class{baselineAlgTest}} object
#' @param object An object of class \code{\linkS4class{baselineAlgTest}}
#'     or subclass of \code{\linkS4class{predictionTest}} (currently
#'     \code{\linkS4class{PLSRTest}} or
#'     \code{\linkS4class{ridgeRegressionTest}}).  The object specify the test to be run.
#' @param X A \code{matrix}. The spectra to use in the test.
#' @param y A \code{vector} or \code{matrix}.  The response(s) to use in the test
#' @param predictionTest A \code{\linkS4class{predictionTest}} object,
#'    describing the prediction test to use for this baseline algorithm test.
#' @param postproc A \code{function}, used to postprocess the baseline corrected
#'    spectra prior to prediction testing.  The function should take a
#'    matrix of spectra as its only argument, and return a matrix of
#'    postprocessed spectra.
#' @param verbose \code{Logical}, specifying whether the test should print out progress information.  Default is \code{FALSE}.
#' @param ... Other arguments. Currently only used by the \code{\linkS4class{baselineAlgTest}} method.
#' @section Methods:
#' \code{signature(object = "baselineAlgTest")}: Baseline corrects the spectra, optionally postprocesses them, and runs a prediction test on the corrected spectra.
#' \code{signature(object = "PLSRTest")}: Runs PLSR on the data and calculates the cross-validated RMSEP
#' \code{signature(object = "ridgeRegressionTest")}: Runs ridge regression on the data and calculates the GCV
#' @return 
#' \code{runTest} returns an object of class.  
#' \code{\linkS4class{predictionResult}} or \code{\linkS4class{baselineAlgResult}}.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso \code{\linkS4class{baselineAlgTest}}, \code{\linkS4class{predictionTest}}, \code{\linkS4class{PLSRTest}},  \code{\linkS4class{ridgeRegressionTest}}
#' @keywords spectra baseline methods
#' @export runTest
setGeneric("runTest", function(object, X, y, ...) standardGeneric("runTest"))

## A function for extracting the quality measures from objects.
## It will have methods for predictionResult and baselineAlgResult.
#' @name qualMeas
#' @aliases qualMeas minQualMeas param.min qualMeasName
#' @title Extraction functions for "predictionResult" or "baselineAlgResult" objects.
#' @description Extract slots from objects of class
#'    \code{\linkS4class{predictionResult}} or
#'    \code{\linkS4class{baselineAlgResult}}.
#' @param object An object of class \code{\linkS4class{predictionResult}} or \code{\linkS4class{baselineAlgResult}}
#' @param MIN \code{List} or \code{vector} of parameter names to take the minimum over.  Not used if \code{DEFAULT} is \code{"cond.min"}.  See Details.
#' @param AVG \code{List} or \code{vector} of parameter names to take the average over.  Not used if \code{DEFAULT} is \code{"avg"}.  See Details.
#' @param DEFAULT Character string.  The default way to calculate the minimum (or average) for all parameters.  See Details.
#' @param ... Other arguments. Selection of subsets of parameter levels.  See Details.
#' @details 
#' The arguments to the \code{\linkS4class{baselineAlgResult}} method are interpreted in the following way:
#'     
#' Subsets of parameters levels can be selected by supplying their names
#' and specifying the level indices as vectors. Substituting a vector
#' with \code{"all"} will return all levels of the corresponding parameter, and
#' substituting it with \code{"overall"} will return the level corresponding
#' to the overall minimum.
#' Minimum and average values for selected parameters can be chosen using
#' \code{MIN} and \code{AVG}, respectively, together with a vector of parameter names.
#' 
#' \code{DEFAULT} specifies the action for each remaining parameters:
#' If \code{"all"} (default): returns all levels.  If \code{"cond.min"}: 
#' take minimum for each remaining parameter (MIN is not used).  If
#' \code{"overall.min"}: set any remaining parameters to their value
#' corresponding to the overall min. If \code{"avg"}: take average for
#' each remaining parameter (AVG is not used).
#' @return 
#' The \code{qualMeas} method for \code{\linkS4class{baselineAlgResult}} objects returns
#' the subsets or minimum values of the \code{qualMeas} slot of the object as specified
#' above. All other methods simply return the corresponding slot.
#' @author  Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso Function \code{\link{runTest}}, classes \code{\linkS4class{baselineAlgResult}} and \code{\linkS4class{predictionResult}}.
#' @keywords methods baseline spectra
#' @export qualMeas
NULL

#' @rdname qualMeas
#' @export
setGeneric("qualMeas", function(object, ...) standardGeneric("qualMeas"))

## A function to extract parameter settings
## It will have methods for predictionResult, baselineAlgTest,
## baselineAlgResult and baselineAlg.
#' @name param
#' @aliases param
#' @title Extract the "param" slot
#' @description Extracts the \code{param} slot of the object.
#' @param object An object of class \code{\linkS4class{baselineAlg}},
#'   \code{\linkS4class{baselineAlgTest}},
#'   \code{\linkS4class{baselineAlgResult}} or
#'   \code{\linkS4class{predictionResult}}.
#' @return The \code{param} slot of the object.  Usually a data frame, list or numeric.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso Classes \code{\linkS4class{baselineAlg}}, \code{\linkS4class{baselineAlgTest}}, \code{\linkS4class{baselineAlgResult}}, \code{\linkS4class{predictionResult}}
#' @keywords baseline spectra methods
#' @export param
setGeneric("param", function(object) standardGeneric("param"))


###
### Classes and methods for prediction testing
###


## A virtual class for all prediction tests.  It will have subclasses for each
## type of predictor (PLSR, RR, ...).
#' @name predictionTest
#' @aliases  predictionTestClass
#' @title  Class "predictionTest"
#' @description
#' A virtual class for all predictor test subclasses.  Currently
#' subclasses \code{\linkS4class{PLSRTest}} and
#' \code{\linkS4class{ridgeRegressionTest}} are defined.
#' @section Objects from the Class:
#' A virtual Class: No objects may be created from it.
#' @section Methods:
#' No methods defined with class "predictionTest" in the signature.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland.
#' @seealso Subclasses \code{\linkS4class{PLSRTest}} and \code{\linkS4class{ridgeRegressionTest}}.
#' @keywords classes
#' @exportClass predictionTest
setClass("predictionTest")

#' @name predictionResult
#' @aliases  predictionResult
#' @title Class "predictionResult"
#' @description
#' A class containing the result of running a
#' \code{\linkS4class{predictionTest}}.
#' @section Objects from the Class:
#' The normal way to create objects is by calling the method
#' \code{runTest} for any object of subclass of
#' \code{\linkS4class{predictionTest}}.
#' @section Slots:
#' \code{param}: Numeric vector.  The regression parameter values tested.  
#' \code{qualMeas}: Numeric vector.  The quality measure values for each of the values of the \code{param} slot
#' \code{ind.min}: The index (into \code{qualMeas}) of the minimum quality measure value.
#' \code{minQualMeas}: The minimum quality measure value.
#' \code{param.min}: The value of the parameter value corresponding to the minimum quality measure value.
#' \code{qualMeasName}: The name of the quality measure.
#' \code{paramName}: The name of the regression parameter.
#' @section Methods:
#' ind.min: \code{signature(object = "predictionResult")}: Extract the \code{ind.min} slot
#' minQualMeas: \code{signature(object = "predictionResult")}: Extract the \code{minQualMeas} slot
#' param: \code{signature(object = "predictionResult")}: Extract the \code{param} slot
#' param.min: \code{signature(object = "predictionResult")}: Extract the \code{param.min} slot
#' paramName: \code{signature(object = "predictionResult")}: Extract the \code{paramName} slot
#' qualMeas: \code{signature(object = "predictionResult")}: Extract the \code{qualMeas} slot
#' qualMeasName: \code{signature(object = "predictionResult")}: Extract the \code{qualMeasName} slot
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso Function \code{\link{runTest}}, class \code{\linkS4class{predictionTest}}, subclasses \code{\linkS4class{PLSRTest}} and \code{\linkS4class{ridgeRegressionTest}}.
#' @examples
#' showClass("predictionResult")
#' @keywords classes
#' @exportClass predictionResult
setClass("predictionResult",
         representation(param = "numeric",
                        qualMeas = "numeric",
                        ind.min = "numeric", ##?
                        minQualMeas = "numeric", ##?
                        param.min = "numeric", ##?
                        qualMeasName = "character",
                        paramName = "character"
         ))

setMethod("param", "predictionResult", function(object) object@param)

# #' @rdname predictionResult
# #' @exportMethod qualMeas predictionResult
setMethod("qualMeas", "predictionResult", function(object, ...) object@qualMeas)

#' @name predictionResultMethods
#' @title Extraction methods specific for "predictionResult" objects
#' 
#' @description Extract information from objects of class
#' \code{\linkS4class{predictionResult}}.
#' 
#' @aliases ind.min paramName
#' @param object Object of class \code{\linkS4class{predictionResult}}
#' @return The corresponding slot of the object.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso \code{\linkS4class{predictionResult}}
#' @keywords methods
#' @export
setGeneric("ind.min", function(object) standardGeneric("ind.min"))
setMethod("ind.min", "predictionResult", function(object) object@ind.min)

#' @rdname predictionResult
#' @export
setGeneric("minQualMeas", function(object) standardGeneric("minQualMeas"))
setMethod("minQualMeas", "predictionResult", function(object) object@minQualMeas)

#' @rdname predictionResult
#' @export
setGeneric("param.min", function(object) standardGeneric("param.min"))
setMethod("param.min", "predictionResult", function(object) object@param.min)

#' @rdname predictionResult
#' @export
setGeneric("qualMeasName", function(object) standardGeneric("qualMeasName"))
setMethod("qualMeasName", "predictionResult", function(object) object@qualMeasName)

#' @rdname predictionResultMethods
#' @export
setGeneric("paramName", function(object) standardGeneric("paramName"))
setMethod("paramName", "predictionResult", function(object) object@paramName)

##
## PLSR
##

## The class
#' @name PLSRTestClass
#' @aliases PLSRTestClass
#' @title Class "PLSRTest"
#' @description
#' A class describing a PLSR prediction test.  To run the test, the "pls" package must be installed.
#' @section Objects from the Class:
#' Objects can be created by calls of the form \code{new("PLSRTest", ...)}.
#' @section Slots:
#' \code{ncomp}: Integer vector.  The number of PLSR components to test.
#' \code{cvsegments}: A list of the segments to use in the cross-validation.
#' @section Extends:
#' Class \code{\linkS4class{predictionTest}}, directly.
#' @section Methods:
#' \code{signature(object = "PLSRTest")}: Run the test
#' @author Bjørn-Helge Mevik and Krisitan Hovde Liland.
#' @seealso The base class \code{\linkS4class{predictionTest}}. The \code{\link{runTest}} function.  The \code{\link[pls:mvr]{plsr}} function from the "pls" package.
#' @examples
#' showClass("PLSRTest")
#' @keywords classes
#' @exportClass PLSRTest
setClass("PLSRTest", contains = "predictionTest",
         representation(ncomp = "numeric", cvsegments = "list"))

## The prediction test method
# #' @rdname runTestMethods
# #' @exportMethod runTest PLSRTest
setMethod("runTest", "PLSRTest",
          function(object, X, y) {
            if(requireNamespace("pls", quietly = TRUE)){
              ncomp <- object@ncomp
              cvsegments <- object@cvsegments
              ## FIXME: Perhaps use mvrCv directly
              res <- pls::plsr(y ~ X, ncomp = ncomp, validation = "CV",
                               segments = cvsegments)
              msep <- drop(pls::MSEP(res, estimate = "adjCV")$val)
              if (NCOL(y) == 1) {
                rmsep <- sqrt(msep)
              } else {
                ## For multiple responses, use the square root of the
                ## mean (over the responses) relative (to 0 components)
                ## MSEP:
                rmsep <- sqrt(colMeans(msep / msep[,1]))
              }
              ind.min <- which.min(rmsep)
              return(new("predictionResult",
                         param = 0:ncomp,
                         qualMeas = rmsep, ind.min = ind.min, minQualMeas = min(rmsep),
                         param.min = ind.min - 1,
                         paramName = "ncomp", qualMeasName = "RMSEP"))
            } else {
              warning('Package pls not installed')
              return(list())
            }
          })

##
## Ridge regression
##

## The class
#' @name ridgeRegressionTestClass
#' @aliases ridgeRegressionTest-class
#' @title Class "ridgeRegressionTest"
#' @description A class describing a ridge regression test.
#' @section Objects from the Class:
#' Objects can be created by calls of the form \code{new("ridgeRegressionTest", ...)}.
#' @section Slots:
#' \code{lambda}: Numeric vector.  The smoothing parameter values to test
#' @section Extends:
#' Class \code{\linkS4class{predictionTest}}, directly.
#' @section Methods:
#' runTest \code{signature(object = "ridgeRegressionTest")}: Run the test
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso The base class \code{\linkS4class{predictionTest}}.  The \code{\link{runTest}} function.
#' @examples
#' showClass("ridgeRegressionTest")
#' @keywords classes
#' @exportClass ridgeRegressionTest
setClass("ridgeRegressionTest", contains = "predictionTest",
         representation(lambda = "numeric"))

setMethod("runTest", "ridgeRegressionTest",
          function(object, X, y) {
            if(requireNamespace("MASS", quietly = TRUE)){
              lambda <- object@lambda
              res <- MASS::lm.ridge(y ~ X, lambda = lambda)
              ind.min <- which.min(res$GCV)
              return(new("predictionResult",
                         param = lambda,
                         qualMeas = res$GCV, ind.min = ind.min, minQualMeas = min(res$GCV),
                         param.min = res$lambda[ind.min],
                         paramName = "lambda", qualMeasName = "GCV"))
            } else {
              warning('Package MASS not installed')
              return(list())
            }
          })



###
### Classes and methods for describing baseline algorithms
### FIXME: Put in separate file!


#' @name baselineAlg
#' @title Class "baselineAlg"
#' 
#' @description A class that describes a baseline correction algorithm.  The idea is that it
#' contains all information needed to use an algorithm with the optimisation
#' framework and the graphical user interface (but see Notes below).
#' 
#' 
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
#' @exportClass baselineAlg
setClass("baselineAlg",
         representation(name = "character",
                        description = "character",
                        funcName = "character",
                        param = "data.frame"
         ),
         prototype(param = data.frame(
           name = NA, integer = NA, min = NA, incl.min = NA,
           default = NA, max = NA, incl.max = NA)[0,]
         ),
         validity = function(object) {
           if (!identical(names(object@param),
                          c("name","integer","min","incl.min","default","max","incl.max")))
             return("The param slot does not have the correct coloumn names")
           return(TRUE)
         }
)


#' @name funcName
#' @title Extract the "funcName" slot.
#' 
#' @description Extract the \code{funcName} slot from an object of class
#' \code{\linkS4class{baselineAlg}} or \code{\linkS4class{baselineAlgTest}}
#' 
#' 
#' @aliases funcName funcName-methods funcName,baselineAlg-method
#' funcName,baselineAlgTest-method
#' @param object An object of class \code{\linkS4class{baselineAlg}} or
#' \code{\linkS4class{baselineAlgTest}}
#' @return The \code{funcName} slot of the object.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso \code{\linkS4class{baselineAlg}},
#' \code{\linkS4class{baselineAlgTest}}
#' @keywords spectra baseline
#' @export
setGeneric("funcName", function(object) standardGeneric("funcName"))
setMethod("funcName", "baselineAlg", function(object) object@funcName)


#' @name baselineAlg
#' @title Extraction methods for "baselineAlg" objects
#' 
#' @description Extraction methods specifically for objects of class
#' \code{\linkS4class{baselineAlg}}
#' 
#' @aliases name name-methods name,baselineAlg-method description
#' description-methods description,baselineAlg-method
#' @param object Object of class \code{\linkS4class{baselineAlg}}
#' @return The methods return the corresponding slot of the object.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso \code{\linkS4class{baselineAlg}}, \code{\link{funcName}}.
#' @keywords baseline spectra
#' @export
setGeneric("name", function(object) standardGeneric("name"))
setMethod("name", "baselineAlg", function(object) object@name)

#' @rdname rdbaselineAlg
#' @export
setGeneric("description", function(object) standardGeneric("description"))
setMethod("description", "baselineAlg", function(object) object@description)

#' @rdname baselineAlg
#' @export
setMethod("param", "baselineAlg", function(object) object@param)


## A list with objects for each implemented algorithm:
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
#' @export
baselineAlgorithms <- list(
  
  als = new("baselineAlg",
            name = "als",
            description = "Asymmetric Least Squares",
            funcName = "baseline.als",
            param = data.frame(  # FIXME
              name = c("lambda","p"), # maxit
              integer = c(FALSE, FALSE),
              min = c(0, 0),
              incl.min = c(TRUE, TRUE),
              default = c(6, 0.05),
              max = c(Inf, 1),
              incl.max = c(FALSE, TRUE)
            )),
  fillPeaks = new("baselineAlg",
                  name = "fillPeaks",
                  description = "Iterateive baseline correction algorithm based on mean suppression",
                  funcName = "baseline.fillPeaks",
                  param = data.frame(  # FIXME
                    name = c("lambda","hwi","it", "int"),
                    integer = c(FALSE, TRUE, TRUE, TRUE),
                    min = c(0, 1, 1, 3),
                    incl.min = c(TRUE, TRUE, TRUE, TRUE),
                    default = c(NA, NA, NA, NA),
                    max = c(Inf, Inf, Inf, Inf),
                    incl.max = c(FALSE, FALSE, FALSE, FALSE)
                  )),
  irls = new("baselineAlg",
             name = "irls",
             description = "Iterative Restricted Least Squares",
             funcName = "baseline.irls",
             param = data.frame( # FIXME
               name = c("lambda1", "lambda2", "maxit", "wi"),
               integer = c(FALSE, FALSE, TRUE, TRUE),
               min = c(0, 0, 0, 0),
               incl.min = c(TRUE, TRUE, TRUE, TRUE),
               default = c(5, 9, 200, 0.05),
               max = c(Inf, Inf, Inf, 1),
               incl.max = c(FALSE, FALSE, FALSE, TRUE)
             )),
  ## FIXME: Not tested, because baseline.lowpass gives strange results!
  lowpass = new("baselineAlg",
                name = "lowpass",
                description = "Low-pass filter based on fast Fourier transform",
                funcName = "baseline.lowpass",
                param = data.frame(  # FIXME
                  name = c("steep","half"),
                  integer = c(FALSE, TRUE),
                  min = c(0, 1),
                  incl.min = c(TRUE, TRUE),
                  default = c(2, 5),
                  max = c(Inf, Inf),
                  incl.max = c(FALSE, FALSE)
                )),
  medianWindow = new("baselineAlg",
                     name = "medianWindow",
                     description = "Local medians",
                     funcName = "baseline.medianWindow",
                     param = data.frame(  # FIXME
                       name = c("hwm","hws"),         # end
                       integer = c(TRUE, TRUE),
                       min = c(0,1),
                       incl.min = c(TRUE, TRUE),
                       default = c(NA, NA),
                       max = c(Inf, Inf),
                       incl.max = c(FALSE, FALSE)
                     )),
  modpolyfit = new("baselineAlg",
                   name = "modpolyfit",
                   description = "Modified iterative polynomial fitting",
                   funcName = "baseline.modpolyfit",
                   param = data.frame(  # FIXME
                     name = c("degree", "tol", "rep"), # t
                     integer = c(TRUE, FALSE, TRUE),
                     min = c(1, 0, 0),
                     incl.min = c(TRUE, TRUE, TRUE),
                     default = c(4, 1e-3, 100),
                     max = c(Inf, Inf, Inf),
                     incl.max = c(FALSE, FALSE, FALSE)
                   )),
  ## FIXME: Transformed parameters??
  peakDetection = new("baselineAlg",
                      name = "peakDetection",
                      description = "Peak detection",
                      funcName = "baseline.peakDetection",
                      param = data.frame(  # FIXME
                        name = c("left","right","lwin", "rwin", "snminimum", "mono", "multiplier"),
                        integer = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
                        min = c(1,1,1,1,0,0,0),
                        incl.min = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
                        default = c(NA),
                        max = c(Inf,Inf,Inf,Inf,Inf,1,Inf),
                        incl.max = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
                      )),
  ## FIXME:  Perhaps easier to specify span than NoXP??
  rfbaseline = new("baselineAlg",
                   name = "rfbaseline",
                   description = "Robust Baseline Estimation by Ruckstuhl etal",
                   funcName = "baseline.rfbaseline",
                   param = data.frame(  # FIXME
                     name = c("NoXP","b"),        # Lots of arguments!
                     integer = c(TRUE, FALSE),
                     min = c(3, 0),
                     incl.min = c(TRUE, FALSE),
                     default = c(NA, 3.5),
                     max = c(Inf, Inf),
                     incl.max = c(FALSE, FALSE)
                   )),
  rollingBall = new("baselineAlg",
                    name = "rollingBall",
                    description = "Rolling Ball",
                    funcName = "baseline.rollingBall",
                    param = data.frame(  # FIXME
                      name = c("wm","ws"),
                      integer = c(TRUE, TRUE),
                      min = c(2, 2),
                      incl.min = c(TRUE, TRUE),
                      default = c(NA, NA),
                      max = c(Inf, Inf),
                      incl.max = c(FALSE, FALSE)
                    )),
  shirley = new("baselineAlg",
                name = "shirley",
                description = "A shirley baseline correction algorithm",
                funcName = "baseline.shirley",
                param = data.frame(
                  name = c("maxit", "err"), # maxit
                  integer = c(FALSE, FALSE),
                  min = c(1, 1e-8),
                  incl.min = c(TRUE, TRUE),
                  default = c(50, 1e-6),
                  max = c(Inf, 1e-3),
                  incl.max = c(FALSE, FALSE)
                )),
  TAP = new("baselineAlg",
                name = "TAP",
                description = "Kintetic analysis of derivative data",
                funcName = "baseline.TAP",
                param = data.frame(
                  name = c("interval", "tol"),
                  integer = c(TRUE, FALSE),
                  min = c(1, 1e-12),
                  incl.min = c(TRUE, TRUE),
                  default = c(15, 1e-3),
                  max = c(Inf, 1),
                  incl.max = c(FALSE, FALSE)
                ))
  
) ## End of baselineAlgorithms <- list(



###
### Classes and methods for baseline algorithm tests
###


#' Class "baselineAlgTest"
#' 
#' A class that describes a baseline algorithm test.  The test is performed
#' with the function \code{runTest}.
#' 
#' 
#' @name baselineAlgTest
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
#' @exportClass baselineAlgTest
setClass("baselineAlgTest",
         representation(algorithm = "baselineAlg",
                        param = "list",
                        extraArgs = "list"),
         prototype(extraArgs = list()))



#' Class "baselineAlgResult"
#' 
#' A class describing the result of a baseline algorithm test
#' 
#' 
#' @name baselineAlgResult
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
#' @exportClass baselineAlgResult
setClass("baselineAlgResult",
         representation(param = "list",
                        qualMeas = "matrix", ## or multidimensional array?
                        qualMeas.ind.min = "numeric",
                        minQualMeas = "numeric", ##?
                        param.ind.min = "numeric",
                        param.min = "list",
                        qualMeasName = "character"
         ))

## Accessor functions:
#' @name algorithm
#' @aliases algorithm extraArgs
#' @title Extraction methods for "baselineAlgTest" objects
#' @description
#' Extraction methods specifically for objects of class \code{\linkS4class{baselineAlgTest}}
#' @param object Object of class \code{\linkS4class{baselineAlgTest}}
#' @return The corresponding slot
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland.
#' @seealso \code{\linkS4class{baselineAlgTest}}
#' @keywords baseline spectra
#' @export
setGeneric("algorithm", function(object) standardGeneric("algorithm"))
setMethod("algorithm", "baselineAlgTest", function(object) object@algorithm)

## One class for all algorithms
#' @name baselineAlgTestMethods
#' @title Extraction methods for "baselineAlgTest" objects
#' 
#' @description Extraction methods specifically for objects of class
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
#' @export
setGeneric("extraArgs", function(object) standardGeneric("extraArgs"))
setMethod("extraArgs", "baselineAlgTest", function(object) object@extraArgs)

#' @name baselineAlgResultMethods
#' @title Extraction methods for "baselineAlgResult" objects
#' 
#' @description Extraction methods that are specific for objects of class
#' \code{\linkS4class{baselineAlgResult}}
#' 
#' @aliases param.ind.min qualMeas.ind.min
#' @param object Object of class \code{\linkS4class{baselineAlgResult}}
#' @return The corresponding slot
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso Class \code{\linkS4class{baselineAlgResult}}
#' @keywords baseline spectra
#' @export
setGeneric("qualMeas.ind.min", function(object) standardGeneric("qualMeas.ind.min"))
setMethod("qualMeas.ind.min", "baselineAlgResult", function(object) object@qualMeas.ind.min)

#' @rdname baselineAlgResultMethods
#' @export
setGeneric("param.ind.min", function(object) standardGeneric("param.ind.min"))
setMethod("param.ind.min", "baselineAlgResult", function(object) object@param.ind.min)

setMethod("param", "baselineAlgTest", function(object) object@param)
setMethod("funcName", "baselineAlgTest",
          function(object) funcName(object@algorithm))
setMethod("param", "baselineAlgResult", function(object) object@param)
setMethod("minQualMeas", "baselineAlgResult", function(object) object@minQualMeas)
setMethod("param.min", "baselineAlgResult", function(object) object@param.min)
setMethod("qualMeasName", "baselineAlgResult", function(object) object@qualMeasName)


## A method for doing the algorithm testing
# #' @rdname baselineAlgTest
# #' @export
setMethod("runTest", "baselineAlgTest",
          function(object, X, y, predictionTest, postproc, verbose = FALSE) {
            ## Build a data frame with all parameter combinations:
            params <- expand.grid(object@param, KEEP.OUT.ATTRS = FALSE)
            nparams <- nrow(params)
            ## Variables to accumulate results:
            qualMeas <- list()
            param.min <- minQualMeas <- regpar.ind.min <- numeric(nparams)
            if (verbose) cat("Looping through the", nparams,
                             "baseline parameter settings\n")
            for (i in seq_len(nparams)) {
              if (verbose) cat(i, ": correcting... ", sep = "")
              ## Do the baseline correction
              bl <- do.call("baseline",
                            c(list(spectra = X, method =  name(object@algorithm)),
                              params[i,, drop=FALSE], object@extraArgs))
              Xcorr <- getCorrected(bl)
              ## Perform any post processing:
              if (!missing(postproc) && !is.null(postproc)) {
                if (verbose) cat("postprocessing... ")
                Xcorr <- postproc(Xcorr)
              }
              ## Test the predictor on the baseline corrected data
              if (verbose) cat("prediction testing...")
              res <- runTest(predictionTest, Xcorr, y)
              qualMeas[[i]] <- qualMeas(res)
              regpar.ind.min[i] <- ind.min(res)
              minQualMeas[i] <- minQualMeas(res)
              param.min[i] <- param.min(res)
              if (verbose) cat("\n")
            }
            qualMeas <- do.call("cbind", qualMeas)
            ## Find and return the best results
            best.i <- which.min(minQualMeas)
            ## Join the regression parameter and the baseline parameters:
            param <- c(list(param(res)), object@param)
            names(param)[1] <- paramName(res)
            param.ind.min <-
              c(regpar.ind.min[best.i],
                unlist(expand.grid(lapply(object@param, seq_along))[best.i,, drop=FALSE]))
            names(param.ind.min)[1] <- paramName(res)
            param.min <- c(param.min[best.i], as.list(params[best.i,, drop=FALSE]))
            names(param.min)[1] <- paramName(res)
            return(new("baselineAlgResult",
                       param = param,
                       qualMeas = qualMeas,
                       qualMeas.ind.min = c(regpar.ind.min[best.i], best.i),
                       minQualMeas = minQualMeas[best.i],
                       param.ind.min = param.ind.min,
                       param.min = param.min,
                       qualMeasName = qualMeasName(res)
            ))
          })


###
### Method for extracting test result values from a baseline algorithm
### test
###
# #' @rdname baselineAlgResultMethods
# #' @export
setMethod("qualMeas", "baselineAlgResult",
          function(object, ..., MIN, AVG, DEFAULT = c("all", "cond.min", "overall.min", "avg")) {
            DEFAULT <- match.arg(DEFAULT)
            ## Extract needed information from object:
            res <- object@qualMeas
            params <- object@param
            param.ind.min <- object@param.ind.min
            
            ## Collect the named parameters:
            setparams <- list(...)
            
            ## 0) Substitute for any setparams == "overall"
            atoverall <- names(setparams)[setparams == "overall"]
            if (length(atoverall) > 0) {
              setparams[atoverall] <- param.ind.min[atoverall]
            }
            ## 0b) Substitute for any setparams == "all"
            useall <- names(setparams)[setparams == "all"]
            if (length(useall) > 0) {
              setparams[useall] <- lapply(params[useall], seq_along)
            }
            
            ## Figure out which parameters to take (conditional) min over:
            MINns <- AVGns <- character()
            if (DEFAULT == "cond.min") {
              ## Take min over all remaining parameters
              if(missing(AVG)) {
                MINns <- setdiff(names(params), names(setparams))
              } else {
                MINns <- setdiff(names(params), c(names(setparams), AVG))
              }
              ## Figure out which parameters to take average over:
            } else {
              if (DEFAULT == "avg") {
                ## Take average over all remaining parameters
                if(missing(MIN)) {
                  AVGns <- setdiff(names(params), names(setparams))
                } else {
                  AVGns <- setdiff(names(params), c(names(setparams), MIN)) }
              }
            }
            ## Figure out which parameters to take (conditional) min over:
            if (!missing(MIN) && length(MINns)==0) {
              ## Take min over parameters specified in MIN
              MIN <- substitute(MIN)
              wasList <- is.call(MIN)
              MINns <- sapply(MIN, as.character) # Convert to char, if
              # needed; also make it a vector
              ## If MIN was a list, the first element will be "list":
              if (wasList) MINns <- MINns[-1]
              ## Sanity check:
              if (!all(MINns %in% names(params)))
                stop("Non-existing parameter(s) specified in 'MIN'.")
            }
            ## Figure out which parameters to take average over:
            if (!missing(AVG) && length(AVGns)==0) {
              ## Take average over parameters specified in AVG
              AVG <- substitute(AVG)
              wasList <- is.call(AVG)
              AVGns <- sapply(AVG, as.character) # Convert to char, if
              # needed; also make it a vector
              ## If AVG was a list, the first element will be "list":
              if (wasList) AVGns <- AVGns[-1]
              ## Sanity check:
              if (!all(AVGns %in% names(params)))
                stop("Non-existing parameter(s) specified in 'AVG'.")
            }
            
            ## If DEFAULT is "overall.min", set any remaining parameters to their
            ## value corresponding to the overall min:
            if (DEFAULT == "overall.min") {
              nms <- setdiff(names(params), c(names(setparams), MINns, AVGns))
              for (nm in nms) {
                setparams[[nm]] <- param.ind.min[nm]
              }
            }
            
            ## 1) Convert the results into an array
            newdim <- sapply(params, length)
            dim(res) <- newdim
            dimnames(res) <- params
            
            ## 2) Reorder the results into the order given in params
            
            ## Internal function for applying without dropping dimensions
            kapply <- function(x, MARGIN, FUN, lab){
              dims <- dim(x)
              command <- "x["
              if(MARGIN==1){
                command <- "x[1"
                dimnames(x)[[1]][1] <- lab
              } else {
                command <- "x["
              }
              for(i in 2:length(dims)){
                if(MARGIN==i){
                  command <- paste(command,", 1", sep="")
                  dimnames(x)[[i]][1] <- lab
                } else {
                  command <- paste(command,", ", sep="")
                }
              }
              command <- paste(command, ", drop=FALSE]", sep="")
              y <- eval(parse(text=command))
              y[] <- apply(x, setdiff(1:length(dims),MARGIN), FUN)
              y
            }
            
            ## Internal function for subsetting without collapsing
            ksubset <- function(x, setparams){
              dn <- names(dimnames(x))
              sn <- names(setparams)
              dims <- dim(x)
              command <- "x["
              if(sn[1]%in%dn){
                command <- paste("x[setparams$", sn[1], sep="")
              } else {
                command <- "x["
              }
              for(i in 2:length(dims)){
                if(sn[i]%in%dn){
                  command <- paste(command, ", setparams$", sn[i], sep="")
                } else {
                  command <- paste(command,", ", sep="")
                }
              }
              command <- paste(command, ", drop=FALSE]", sep="")
              eval(parse(text=command))
            }
            
            ## 3) Extract the desired sub-array:
            if(length(setparams)>0)
              res <- ksubset(res, setparams)
            res.names <- names(dimnames(res))
            
            ## 4) Take the min over any parameters in MINns
            if( length(MINns) > 0){
              for( i in 1:length(MINns)){
                res <- kapply(res, which(res.names==MINns[i]), min, 'min')
              }
            }
            
            ## 5) Take the mean over any parameters in AVGns
            if( length(AVGns) > 0){
              for( i in 1:length(AVGns)){
                res <- kapply(res, which(res.names==AVGns[i]), mean, 'avg')
              }
            }
            
            return(res)
          })

###
### Function for testing several baseline algorithms
###



#' @name doOptim
#' @title Optimise several baseline algorithms on a data set
#' 
#' @description Tests several baseline algorithms with one predictor for a given data set.
#' The baseline algorithms are represented as a list of
#' \code{\linkS4class{baselineAlgTest}} objects, and the predictor as a
#' \code{\linkS4class{predictionTest}} object.
#' 
#' @details The function loops through the baseline algorithm tests in
#' \code{baselineTests}, testing each of them with the given data and
#' prediction test, and collects the results.  The results of each baseline
#' algorithm test is saved in a temporary file so that if the optimisation is
#' interrupted, it can be re-run and will use the pre-calculated results.  If
#' \code{cleanTmp} is \code{TRUE}, the temporary files are deleted when the
#' whole optimisation has finished.
#' 
#' @aliases doOptim mvrValstats
#' @param baselineTests a list of \code{\linkS4class{baselineAlgTest}} objects.
#' The baseline algorithms and parameter values to test
#' @param X A matrix.  The spectra to use in the test
#' @param y A vector or matrix.  The response(s) to use in the test
#' @param predictionTest A \code{\linkS4class{predictionTest}} object.  The
#' predictor and parameter values to use in the test
#' @param postproc A function, used to postprocess the baseline corrected
#' spectra prior to prediction testing.  The function should take a matrix of
#' spectra as its only argument, and return a matrix of postprocessed spectra
#' @param tmpfile The basename of the files used to store intermediate
#' calculations for checkpointing.  Defaults to \code{"tmp.baseline"}
#' @param verbose Logical, specifying whether the test should print out
#' progress information.  Default is \code{FALSE}
#' @param cleanTmp Logical, specifying whether the intermediate files should be
#' deleted when the optimisation has finished.  Default is \code{FALSE}
#' @return A list with components \item{baselineTests}{The \code{baselineTests}
#' argument} \item{results}{A list with the \code{baselineAlgResult} objects
#' for each baseline test} \item{minQualMeas}{The minimum quality measure
#' value} \item{baselineAlg.min}{The name of the baseline algorithm giving the
#' minimum quality measure value} \item{param.min}{A list with the parameter
#' values corresponding to the minimum quality measure value}
#' @examples
#' data(milk)
#' X <- milk$spectra[,-1]
#' y <- milk$spectra[,1]
#' opt <- doOptim(
#'     list(ALS = new("baselineAlgTest", 
#'                    algorithm = baselineAlgorithms$als, 
#'                    param=list(lambda=6:7,p=0.5))),
#'     X, y, 
#'     new("PLSRTest", 
#'         ncomp = 10, 
#'         cvsegments = cvsegments(N=45, k=5)))
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso
#' \code{\linkS4class{baselineAlgTest}},\code{\linkS4class{predictionTest}}
#' @keywords baseline spectra
#' @export
doOptim <- function(baselineTests, X, y, predictionTest,
                    postproc = NULL, tmpfile = "tmp.baseline",
                    verbose = FALSE, cleanTmp = FALSE) {
  nalgs <- length(baselineTests)
  ## Variables to collect return values:
  results <- param.min <- list()
  savefiles <- character(nalgs)
  minQualMeas <- numeric(nalgs)
  
  if (verbose) cat("Looping through the", nalgs, "baseline algorithm tests\n")
  for (i in seq_len(nalgs)) {
    fname <- funcName(baselineTests[[i]])
    blname <- names(baselineTests)[i]
    if (is.null(blname) || !nzchar(blname))
      blname <- name(algorithm(baselineTests[[i]]))
    if (verbose) cat(i, ": ", blname, ":\n", sep = "")
    savefiles[i] <- paste(tmpfile, blname, "RData", sep = ".")
    if (file.exists(savefiles[i])) {
      load(savefiles[i])
      if (verbose) cat(" Loaded pre-calculated results from file", savefiles[i], "\n")
    } else {
      res <- runTest(baselineTests[[i]], X, y, predictionTest, postproc, verbose)
      save(res, file = savefiles[i])
    }
    results[[i]] <- res
    minQualMeas[i] <- minQualMeas(res)
    param.min[[i]] <- param.min(res)
  }
  ## Optionally, delete temporary files:
  if (isTRUE(cleanTmp)) unlink(savefiles)
  ## Find and return the best results
  best.i <- which.min(minQualMeas)
  minQualMeas <- minQualMeas[best.i]
  param.min <- param.min[[best.i]]
  names(results) <- names(baselineTests)
  return(list(baselineTests = baselineTests,
              results = results,
              minQualMeas = minQualMeas,
              baselineAlg.min = name(algorithm(baselineTests[[best.i]])),
              param.min = param.min))
}

## Function to extract minimum from optimisation:


#' @name overall.min
#' @title Extract the minimum from a baseline optimisation
#' 
#' @description Takes the result of an optimisation (a call to \code{\link{doOptim}}) and
#' extracts the minimum quality measure value along with the parameters giving
#' rise to the value.
#' 
#' 
#' @param results Result of call to \code{\link{doOptim}}
#' @return A list with components \item{qualMeas}{The minimum quality measure
#' value} \item{algorithm}{The name of the baseline algorithm corresponding to
#' the minimum} \item{param}{A list with the parameter values corresponding to
#' the minimum quality measure value}
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso \code{\link{doOptim}}
#' @keywords baseline spectra
#' @export
overall.min <- function(results) {
  with(results, list(qualMeas = minQualMeas, algorithm = baselineAlg.min,
                     param = param.min))
}

## The function mvrValstats has been copied (2013-08-10) from package pls (overlapping authors)
## because eval(parent.frame()) seems to break call to pls::msep.
mvrValstats <- function (object, estimate, newdata, ncomp = 1:object$ncomp, 
                         comps, intercept = cumulative, se = FALSE, ...) 
{
  cumulative <- missing(comps) || is.null(comps)
  if (any(estimate == "CV")) {
    if (!cumulative) 
      stop("Cross-validation is not supported when `comps' is specified")
    if (is.null(object$validation)) 
      stop("`object' has no `validation' component")
  }
  nestimates <- length(estimate)
  nresp <- dim(fitted(object))[2]
  respnames <- dimnames(fitted(object))[[2]]
  SSE <- array(dim = c(nestimates, nresp, if (cumulative) 1 + 
                         length(ncomp) else 2), dimnames = list(estimate = estimate, 
                                                                response = respnames, model = if (cumulative) {
                                                                  c("(Intercept)", paste(ncomp, "comps"))
                                                                } else {
                                                                  c("(Intercept)", paste("(Intercept), Comp", paste(comps, 
                                                                                                                    collapse = ", ")))
                                                                }))
  SST <- array(dim = c(nestimates, nresp), dimnames = list(estimate = estimate, 
                                                           response = respnames))
  nobj <- numeric(nestimates)
  names(nobj) <- estimate
  for (i in seq(along = estimate)) {
    switch(estimate[i], train = {
      resp <- as.matrix(model.response(model.frame(object)))
      nobj[i] <- nrow(resp)
      if (inherits(object$na.action, "exclude")) {
        resp <- napredict(object$na.action, resp)
      }
      res <- if (cumulative) residuals(object, ...)[, , 
                                                    ncomp, drop = FALSE] else resp - predict(object, 
                                                                                             comps = comps, ...)
      SST[i, ] <- apply(resp, 2, var, na.rm = TRUE) * (nobj[i] - 
                                                         1)
      SSE[i, , ] <- cbind(SST[i, ], colSums(res^2, na.rm = TRUE))
    }, test = {
      if (missing(newdata)) stop("Missing `newdata'.")
      newdata <- model.frame(formula(object), data = newdata)
      resp <- as.matrix(model.response(newdata))
      pred <- if (cumulative) predict(object, ncomp = ncomp, 
                                      newdata = newdata, ...) else predict(object, 
                                                                           comps = comps, newdata = newdata, ...)
      nobj[i] <- nrow(newdata)
      SST[i, ] <- apply(resp, 2, var) * (nobj[i] - 1)
      SSE[i, , ] <- cbind(colSums(sweep(resp, 2, object$Ymeans)^2), 
                          colSums((pred - c(resp))^2))
    }, CV = {
      resp <- as.matrix(model.response(model.frame(object)))
      nobj[i] <- nrow(resp)
      SST[i, ] <- apply(resp, 2, var) * (nobj[i] - 1)
      SSE[i, , ] <- cbind(object$validation$PRESS0, object$validation$PRESS[, 
                                                                            ncomp, drop = FALSE])
    })
  }
  if (cumulative) 
    comps <- ncomp
  if (intercept) 
    comps <- c(0, comps)
  else SSE <- SSE[, , -1, drop = FALSE]
  return(list(SSE = SSE, SST = SST, nobj = nobj, comps = comps, 
              cumulative = cumulative))
}
