## $Id: baseline.rfbaseline.R 182 2011-01-09 21:05:18Z kristl $


#' @title Robust Baseline Estimation
#' 
#' @description Wrapper for Andreas F. Ruckstuhl, Matthew P. Jacobson, Robert W. Field,
#' James A. Dodd's algorithm based on LOWESS and weighted regression
#' 
#' @details Most of the code is the original code as given by the authors. The ability
#' to sort by X-values has been removed and ability to handle multiple spectra
#' has been added
#' 
#' @aliases baseline.rfbaseline rfbaseline
#' @param spectra Matrix with spectra in rows
#' @param span Amount of smoothing (by fraction of points)
#' @param NoXP Amount of smoothing (by number of points)
#' @param maxit Maximum number of iterations in robust fit
#' @param b Tuning constant in the biweight function
#' @param weight Optional weights to be given to individual observations
#' @param Scale S function specifying how to calculate the scale of the
#' residuals
#' @param delta Nonnegative parameter which may be used to save computation.
#' (See \code{rfbaseline}
#' @param SORT Boolean variable indicating whether x data must be sorted.
#' @param DOT Disregard outliers totally (boolean)
#' @param init Values of initial fit
#' @return \item{baseline }{Matrix of baselines corresponding to spectra
#' \code{spectra}} \item{corrected }{Matrix of baseline corrected spectra}
#' @author Kristian Hovde Liland and Bjørn-Helge Mevik
#' @references Andreas F. Ruckstuhl, Matthew P. Jacobson, Robert W. Field,
#' James A. Dodd: Baseline subtraction using robust local regression estimation
#' @keywords baseline spectra
#' @examples
#' 
#' data(milk)
#' bc.rbe <- baseline(milk$spectra[1,, drop=FALSE], method='rfbaseline',
#'   span=NULL, NoXP=1000)
#' \dontrun{
#' 	plot(bc.rbe)
#' }
#' @export
baseline.rfbaseline <- function(spectra, span=2/3, NoXP=NULL, maxit=c(2,2), b=3.5,
                                weight=NULL, Scale=function(r) median(abs(r))/0.6745,
                                delta=NULL, SORT=FALSE, DOT=FALSE, init=NULL){
  
  if(requireNamespace("IDPmisc", quietly = TRUE)){
    
    np <- dim(spectra)
    baseline  <- matrix(0,np[1],np[2])
    X <- 1:np[2]
    
    # Sends one spectrum at the time to rfbaseline
    for(i in 1:np[1]){
      rbe <- IDPmisc::rfbaseline(x=X, y=spectra[i,], span=span, NoXP=NoXP, maxit=maxit, b=b,
                        weight=weight, Scale=Scale,
                        delta=delta, SORT=SORT, DOT=DOT, init=init)
      baseline[i,]  <- rbe$fit
    }
    return(list(baseline = baseline, corrected = spectra - baseline))
  } else {
    warning('Package IDPmisc not installed')
    return(list())
  }
}
