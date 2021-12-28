#' Baseline correction
#' 
#' A common framework with implementations of several baseline correction
#' methods
#' 
#' \tabular{ll}{ Package: \tab baseline\cr Type: \tab Package\cr License: \tab GPL-2\cr } Use function
#' baseline for baseline correction. This function takes matrices of spectra, a
#' method name and parameters needed for the specific method. See helpfiles for
#' details.
#' 
#' @name baseline-package
#' @docType package
#' @author Kristian Hovde Liland and Bjørn-Helge Mevik
#' 
#' Maintainer: Kristian Hovde Liland <kristian.liland@nmbu.no>
#' @references Andreas F. Ruckstuhl, Matthew P. Jacobson, Robert W. Field,
#' James A. Dodd: Baseline subtraction using robust local regression
#' estimation; CHAD A. LIEBER and ANITA MAHADEVAN-JANSEN: Automated Method for
#' Subtraction of Fluorescence from Biological Raman Spectra; Mark S.
#' Friedrichs: A model-free algorithm for the removal of baseline artifacts;
#' AHMET K. ATAKAN, W. E. BLASS, and D. E. JENNINGS: Elimination of Baseline
#' Variations from a Recorded Spectrum by Ultra-low Frequency Filtering; M.A.
#' Kneen, H.J. Annegarn: Algorithm for fitting XRF, SEM and PIXE X-ray spectra
#' backgrounds; K.H. Liland, B.-H. Mevik, E.-O. Rukke, T.  Almøy, M. Skaugen
#' and T. Isaksson (2009) Quantitative whole spectrum analysis with MALDI-TOF
#' MS, Part I: Measurement optimisation.  \emph{Chemometrics and Intelligent
#' Laboratory Systems}, \bold{96}(2), 210--218.
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
#' 
NULL
