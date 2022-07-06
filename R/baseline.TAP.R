# (i) A first approximation of the baseline equation was selected as the straight line between points A and B (see Fig. 2).
# (ii) Based on the first approximation of the baseline equation, the phase change progress parameter was calculated using Eq. (2).
# (iii) An updated equation of the baseline was calculated using Eq. (3) and the phase change progress parameter equation from step (ii).
# (iv) The baseline equation from step (iii) was compared (point by point) with the one from the previous iteration. If the convergence criterion was met (the difference between every baseline value corresponding to two successive iterations was less than 0.1%) the procedure was stopped and the final baseline equation was selected. If the convergence criterion was not fulfilled then a new iteration was carried out from step (ii) until convergence was achieved 

# Tangential area proportional method


#' @title TAP
#' 
#' @description An implementation of Roman Svoboda and Jirí Málek's algorithm for baseline
#' identification in kinetic anlaysis of derivative kinetic data.
#' 
#' @details (i) A first approximation of the baseline equation is selected as the
#' straight line between start and end of the curve. (ii) Based on the first
#' approximation of the baseline equation, the phase change progress parameter
#' is calculated. (iii) An updated equation of the baseline is calculated and
#' the phase change progress parameter equation from step (ii). (iv) The
#' baseline equation from step (iii) is compared (point by point) with the one
#' from the previous iteration. If the convergence criterion is met (the
#' difference between every baseline value corresponding to two successive
#' iterations was less than 0.1\%) the procedure is stopped and the final
#' baseline equation is selected. If the convergence criterion is not fulfilled
#' then a new iteration is carried out from step (ii) until convergence was
#' achieved.
#' 
#' @aliases baseline.TAP TAP
#' @param spectra Matrix with spectra in rows
#' @param t Optional vector of spectrum abcissa
#' @param interval Distance from spectrum end to starting points for the TAP
#' (default = 15)
#' @param tol Tolerance of difference between iterations (default = 0.001)
#' @return \item{baseline }{Matrix of baselines corresponding to spectra
#' \code{spectra}} \item{corrected }{Matrix of baseline corrected spectra}
#' @author Kristian Hovde Liland
#' @references Roman Svoboda and Jirí Málek: Importance of proper baseline
#' identification for the subsequent kinetic analysis of derivative kinetic
#' data, Journal of Thermal Analysis and Calorimetry.
#' @keywords baseline spectra
#' @examples
#' 
#' # My T
#' myT <- 40:170
#' 
#' # My artifical curve
#' myAlpha <- c(seq(0.01, 0.02, length.out=40),
#'              dnorm(seq(-3,3,length.out=51))/2+(0:50)/2000+0.02)
#' myAlpha <- c(myAlpha,
#'              seq(myAlpha[90]-0.001, 0.01, length.out=40))
#' myAlpha <- myAlpha - min(myAlpha)
#' myAlpha <- cumsum(dadt <- myAlpha/sum(myAlpha))
#' 
#' # Discrete derivative
#' mydAlpha <- c(0,diff(myAlpha)); mydAlpha <- matrix(mydAlpha, ncol=length(mydAlpha))
#' rm(myAlpha) # Throw away myAlpha
#' 
#' # Compute baseline from T and derivative
#' B <- baseline(mydAlpha, t=myT, method="TAP")
#' 
#' # Plot 
#' plot(B, xlab = "T", ylab = "da/dT")
#' @export
baseline.TAP <- function(spectra, t = NULL, interval = 15, tol = 0.001){
  dims <- dim(spectra)
  if(is.null(t)){
    t <- 1:dims[2]
  }
  out <- matrix(0.0, dims[1],dims[2])
  for(i in 1:dims[1]){
    out[i,] <- .TAP(t, spectra[i,], interval, tol)
  }
  list(baseline = out, corrected = spectra - out)
}

.TAP <- function(T, dAlpha, interval, tol){
  p     <- length(T)
  # Tangents at start and end
  zr  <- lm(dAlpha[c(1,interval+1)]~ T[c(1,interval+1)])$coef
  zp  <- lm(dAlpha[c(p-interval,p)]~ T[c(p-interval,p)])$coef
  
  # Baseline and alpha
  Bprev <- alpha <- seq(0,1, length.out=p)
  for(i in 1:100){
    B <- (1 - alpha) * (zr[1] + zr[2] * T) + 
      alpha * (zp[1] + zp[2] * T) #(T[p]-T))
    # Estimate of baseline corrected dAlpha
    dAB   <- dAlpha-B
    dAB   <- dAB-min(dAB) # Force positive
    # Estimate of alpha
    alpha <- cumsum(dAB)
    alpha <- alpha - alpha[1] # Starting at 0
    alpha <- alpha/alpha[p]   # Ending at 1
    # Test for convergence
    if(sd(B-Bprev)/sd(Bprev) < tol) # Not sure if this is how they calculated 0.1%
      break
    Bprev <- B
  }
  return(B)
}

