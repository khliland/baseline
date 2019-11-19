# (i) A first approximation of the baseline equation was selected as the straight line between points A and B (see Fig. 2).
# (ii) Based on the first approximation of the baseline equation, the phase change progress parameter was calculated using Eq. (2).
# (iii) An updated equation of the baseline was calculated using Eq. (3) and the phase change progress parameter equation from step (ii).
# (iv) The baseline equation from step (iii) was compared (point by point) with the one from the previous iteration. If the convergence criterion was met (the difference between every baseline value corresponding to two successive iterations was less than 0.1%) the procedure was stopped and the final baseline equation was selected. If the convergence criterion was not fulfilled then a new iteration was carried out from step (ii) until convergence was achieved 

# Tangential area proportional method
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

