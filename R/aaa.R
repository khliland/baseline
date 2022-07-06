### $Id: aaa.R 151 2011-01-02 15:26:27Z bhm $

###
### Start-up setup
###
#.onLoad <- function(...){
    ## To avoid warnings in R CMD check:
    #if(!exists(".baseline.current"))
    #    assign(".baseline.current", list(), .GlobalEnv)
#}

.baselineEnv <- new.env(parent=emptyenv())


#' @name baselineEnv
#' @title Baseline environment
#' 
#' @description Methods to access the baseline environment.
#' 
#' 
#' @aliases baselineEnv putBaselineEnv getBaselineEnv
#' @param x Name of object to put/get.
#' @param mode Mode of object to get.
#' @param value Object to put.
#' @return \code{getBaseline} retrieves an object.
#' @author Kristian Hovde Liland and Bjørn-Helge Mevik
#' @seealso The functions implementing the baseline algorithms:
#' \code{\link{baseline.als}}, \code{\link{baseline.fillPeaks}},
#' \code{\link{baseline.irls}}, \code{\link{baseline.lowpass}},
#' \code{\link{baseline.medianWindow}}, \code{\link{baseline.modpolyfit}},
#' \code{\link{baseline.peakDetection}}, \code{\link{baseline.rfbaseline}},
#' \code{\link{baseline.rollingBall}}
#' @keywords baseline environment
#' @examples
#' 
#' putBaselineEnv('fish', '<==x-<')
#' getBaselineEnv('fish')
#' @export
NULL

#' @rdname baselineEnv
#' @export
baselineEnv <- function() .baselineEnv

#' @rdname baselineEnv
#' @export
putBaselineEnv <- function(x, value) assign(x, value, envir=baselineEnv())

#' @rdname baselineEnv
#' @export
getBaselineEnv <- function(x, mode="any") get(x, envir=baselineEnv(), mode=mode, inherits=FALSE)

putBaselineEnv("baseline.result", list())
putBaselineEnv("baseline.current", list())
