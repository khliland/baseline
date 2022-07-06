#' @title Customized baseline correction
#' 
#' @description This is an implementation of the customized baseline correction suggested by
#' Liland et al. 2011 for local changes in baseline flexibility.
#' 
#' @details This function rescales spectrum abscissa by use of \code{breaks} and
#' \code{gaps} before baseline correction. The effect is that the chosen
#' baseline correction algorithm and paramters will have varying effects along
#' the spectra, effectively giving local control of the amount of
#' rigidity/flexibility of the estimated baseline.
#' 
#' @param spectra Matrix with spectra in rows.
#' @param breaks Vector of locations of break points between sections of
#' varying baseline flexibility (given as abscissa numbers).
#' @param gaps Vector giving the abscissa spacing between each instance of
#' \code{breaks} (and endpoints if not specified in \code{breaks}).
#' @param trans.win Optional width of transition window around break points
#' used for smoothing rough breaks by LOWESS (default = NULL).
#' @param just.plot Plot the rescaled spectra instead of applying the
#' customized baseline correction if \code{just.plot}=TRUE (default = FALSE).
#' @param method Baseline correction method to use (class character).
#' @param \dots Additional named arguments to be passed to the baseline
#' correction method.
#' @return \item{baseline }{Estimated custom baselines.} \item{corrected
#' }{Spectra corrected by custom baselines.} \item{spectra.scaled }{Re-scaled
#' spectra.} \item{baseline.scaled }{Estimated baselines of re-scaled spectra.}
#' @author Kristian Hovde Liland and Bjørn-Helge Mevik
#' @references Kristian Hovde Liland et al.: Customized baseline correction
#' @keywords baseline spectra
#' @examples
#' 
#' data(milk)
#' spectrum1  <- milk$spectra[1,1:10000,drop=FALSE]
#' ordinary   <- baseline(spectrum1, method="als", lambda=6, p=0.01)
#' customized <- custom.baseline(spectrum1, 2900, c(1,20), trans.win=100, 
#' 	just.plot=FALSE, method="als", lambda=6, p=0.01)
#' \dontrun{
#' plot(1:10000,spectrum1, type='l')
#' lines(1:10000,getBaseline(ordinary), lty=2, col=2, lwd=2)
#' lines(1:10000,customized$baseline, lty=3, col=3, lwd=2)
#' }
#' @export
custom.baseline <- function(spectra, breaks, gaps, trans.win=NULL, just.plot=FALSE, method, ...){
	np     <- dim(spectra)

	# Create new x scale
	new.x  <- numeric()
	breaks <- unique(c(1,breaks,np[2]))
	for(i in 1:length(gaps)){
		tmp1   <- seq(breaks[i], breaks[i+1], by=gaps[i])
		new.x  <- append(new.x, tmp1)
	}
	new.x  <- unique(c(1,new.x,np[2]))
	p.scaled <- length(new.x)

	# Scaled spectra
	spectra.scaled <- matrix(NA, np[1], p.scaled)
	for(i in 1:np[1]){
		spectra.scaled[i,] <- approx(1:np[2], spectra[i,], new.x)$y
	}
	
	if(just.plot){
		print(paste("Spectrum length: ", np[2], " -> ", p.scaled, sep=""))
		par(mfrow=c(2,1))
		matplot(t(spectra),type='l',lty=1,col=1, main="Original", xlab='X scale', ylab='Intensity')
		matplot(t(spectra.scaled),type='l',lty=1,col=1, main="Rescaled", xlab='Temporary X xcale', ylab='Intensity')
	} else {
		# Baseline correction
		baseline.scaled <- getBaseline(baseline(spectra=spectra.scaled, method=method, ...))
				
		# Rescaled baselines
		baseline <- matrix(NA, np[1], np[2])
		for(i in 1:np[1]){
			baseline[i,] <- approx(new.x, baseline.scaled[i,], 1:np[2])$y
#			baseline[i,] <- spline(1:p.scaled, baseline.scaled[i,], xout=back.x)$y #, method="natural"
			if(!is.null(trans.win)){
				for(j in 2:(length(breaks)-1)){
					interval <- max(1,round(breaks[j]-trans.win/2)):min(np[2],round(breaks[j]+trans.win/2))
					baseline[i,interval] <- lowess(baseline[i,interval], f=1/2, iter=0,delta=0)$y
				}
			}
		}
		corrected <- spectra-baseline
		list(baseline=baseline, corrected=corrected, spectra.scaled=spectra.scaled, baseline.scaled=baseline.scaled)
	}
}

#snv <- function(object){
#	for(i in 1:dim(object)[1]){
#		object[i,] <- object[i,]-mean(object[i,])
#		object[i,] <- object[i,]/sd(object[i,])
#	}
#	object
#}
