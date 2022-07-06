## $Id: baseline.R 192 2012-06-19 08:36:53Z kristl $
### Main baseline correction function, and definition of class baseline.

###
### Baseline class
###

#' @name baselineClass
#' @title Class "baseline"
#' 
#' @description Stores the result of estimating baselines for one or more spectra.
#' 
#' @aliases baselineClass
#' @docType class
#' @section Objects from the Class: The normal way to create objects is with
#' the function \code{\link{baseline}}.  Several baseline algorithms are
#' available.  See \code{\link{baseline}} for details.  There is a plot method
#' for the class; see \code{\link{plot,baseline-method}}.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso \code{\link{baseline}}, \code{\link{getBaseline}},
#' \code{\link{getSpectra}}, \code{\link{getCorrected}}, \code{\link{getCall}}
#' @keywords classes
#' @examples
#' 
#' showClass("baseline")
#' @importFrom stats approx dnorm fitted formula lm lowess median model.frame model.response mvfft napredict predict residuals runmed sd var
#' @importFrom methods new
#' @importFrom graphics axTicks axis box legend lines matplot par rect text
#' @importFrom grDevices dev.cur dev.list dev.new dev.set gray
#' @export baseline
#' @exportClass baseline
setClass("baseline",
         representation(baseline = "matrix", corrected = "matrix",
                        spectra = "matrix", call = "language")
         )

###
### Top level baseline correction function
###



#' @name baseline
#' @title Baseline correction
#' 
#' @description Common framework for baseline correction
#' 
#' @details Estimates baselines for the \code{spectra}, using the algorithm named in
#' \code{method}.
#' 
#' @param spectra Matrix with spectra in rows
#' @param method Baseline correction method
#' @param \dots Additional parameters, sent to the method
#' @return An object of class \code{\linkS4class{baseline}}.
#' @author Kristian Hovde Liland and Bjørn-Helge Mevik
#' @seealso The functions implementing the baseline algorithms:
#' \code{\link{baseline.als}}, \code{\link{baseline.fillPeaks}},
#' \code{\link{baseline.irls}}, \code{\link{baseline.lowpass}},
#' \code{\link{baseline.medianWindow}}, \code{\link{baseline.modpolyfit}},
#' \code{\link{baseline.peakDetection}}, \code{\link{baseline.rfbaseline}},
#' \code{\link{baseline.rollingBall}}, \code{\link{baseline.shirley}},
#' \code{\link{baseline.TAP}}
#' @references Kristian Hovde Liland, Trygve Almøy, Bjørn-Helge Mevik (2010),
#' Optimal Choice of Baseline Correction for Multivariate Calibration of
#' Spectra, Applied Spectroscopy 64, pp. 1007-1016.
#' @keywords baseline spectra
#' @examples
#' 
#' # Load data
#' data(milk)
#' # The baseline() function is an S4 wrapper for all the different 
#' # baseline correction methods. The default correction method
#' # is IRLS. Data must be organized as row vectors in a matrix
#' # or data.frame.
#' bc.irls <- baseline(milk$spectra[1,, drop=FALSE])
#' \dontrun{
#'   # Computationally heavy
#' 	plot(bc.irls)
#' }
#' 
#' # Available extractors are:
#' # getBaseline(bc.irls)
#' # getSpectra(bc.irls)
#' # getCorrected(bc.irls)
#' # getCall(bc.irls)
#' 
#' # Correction methods and parameters can be specified through the wrapper.
#' bc.fillPeaks <- baseline(milk$spectra[1,, drop=FALSE], lambda=6,
#' 	hwi=50, it=10, int=2000, method='fillPeaks')
#' \dontrun{
#'   # Computationally heavy
#' 	plot(bc.fillPeaks)
#' }
#' 
#' # If a suitable gWidgets2 implementation is installed, a 
#' # graphical user interface is available for interactive
#' # parameter adaption.
#' \dontrun{
#'   # Dependent on external software
#'   baselineGUI(milk$spectra)
#' }
#' @export
baseline <- function (spectra, method = "irls", ...) {
    ## Get baseline algorithm function name:
	if(exists("baselineAlgorithms",envir=.GlobalEnv)){
		bA <- get("baselineAlgorithms",envir=.GlobalEnv)
	} else {
		bA <- baselineAlgorithms
	}
    method <- match.arg(method, names(bA))
    baseFunc <- funcName(bA[[method]])
    
    ## Strip AsIs
    if(inherits(spectra,'AsIs')){
      spectra <- unclass(spectra)
    }
    ## Check class
    if(!inherits(spectra,'matrix')){
      stop("'spectra' must be of class 'matrix'")
    }

    ## Run baseline algorithm:
    res <- do.call(baseFunc, list(spectra, ...))

    ## Build and return the object:
    new("baseline",
        baseline = res$baseline,
        corrected = res$corrected,
        spectra = spectra,
        call = match.call()
        )
}


###
### Extraction methods
###

#' @name getSpectra
#' @title Functions to extract the components of a "baseline" object
#' 
#' @description The functions extract the \code{baseline}, \code{spectra}, \code{corrected}
#' or \code{call} slot of a \code{\linkS4class{baseline}} object; usually the
#' result of a call to \code{\link{baseline}}.
#' 
#' 
#' @aliases getBaseline getSpectra getCorrected getCall
#' @param object A \code{\linkS4class{baseline}} object
#' @return \code{getCall} returns the \code{baseline} call used to create the
#' object.  The other functions return a matrix with the original spectra,
#' estimated baselines or corrected spectra.
#' @section Warning: In a future versoion, one of the slots might be removed
#' from the class definition and calculated on the fly instead, in order to
#' save space.  Therefore, \emph{do} use the extractor functions
#' (\code{getSpectra}, \code{getBaseline} and \code{getCorrected}) instead of
#' accessing the slots directly.
#' @author Bjørn-Helge Mevik and Kristian Hovde Liland
#' @seealso The function \code{\link{baseline}}, the class
#' \code{\linkS4class{baseline}}
#' @keywords spectra baseline
#' @examples
#' 
#' data(milk)
#' bl <- baseline(milk$spectra[1:2,])
#' baseline  <- getBaseline(bl)
#' spectra   <- getSpectra(bl)
#' corrected <- getCorrected(bl)
#' call      <- getCall(bl)
#' @export
setGeneric("getSpectra", function(object) standardGeneric("getSpectra"))
setMethod("getSpectra", "baseline", function(object) object@spectra)

#' @rdname getSpectra
#' @export
setGeneric("getCorrected", function(object) standardGeneric("getCorrected"))
setMethod("getCorrected", "baseline", function(object) object@corrected)

#' @rdname getSpectra
#' @export
setGeneric("getBaseline", function(object) standardGeneric("getBaseline"))
setMethod("getBaseline", "baseline", function(object) object@baseline)

#' @rdname getSpectra
#' @export
setGeneric("getCall", function(object) standardGeneric("getCall"))
setMethod("getCall", "baseline", function(object) object@call)
