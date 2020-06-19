baseline.irls2 <- function(spectra, lambda1=5, lambda2=9, maxit=200, wi=0.05, returnIterations=TRUE){
  ## Iterative restricted least squares with iteration breaking
  ## By Kristian Hovde Liland
  ## $Id: baseline.irls.R 170 2011-01-03 20:38:25Z bhm $
  
  # INPUT:
  # spectra - rows of spectra
  # lambda1 - 2nd derivative constraint for primary smoothing
  # lambda2 - 2nd derivative constraint for secondary smoothing
  # maxit   - maximum number of iterations
  # wi      - weighting of positive residuals
  #
  # OUTPUT:
  # baseline  - proposed baseline
  # corrected - baseline corrected spectra
  # smoothed  - primary smoothed baseline
  
  mn       <- dim(spectra)
  baseline <- matrix(0,mn[1],mn[2])
  
  # Diagonal sparse matrix in compact format (5 x m)
  DD <- baseline:::.create_DD(mn[2])
  
  # Empty matrix (5 x m)
  W <- matrix(0.0, 5, mn[2])
  W[3,] <- 1.0
  
  # Primary and secondary smoother
  U1 <- W+DD*10^lambda1
  U2 <- W+DD*10^lambda2
  
  # Primary smoothing
  smoothed <- t(limSolve::Solve.banded(U1,2,2,t(spectra)))
  
  # Collect iterations
  if(returnIterations && mn[1] == 1)
    iterations <- spectra
  
  # Iterate through spectra
  for(i in 1:mn[1]){
    xn <- smoothed[i,]
    xb <- numeric(mn[2])
    std <- sd(xn)
    
    # Iterate restriction and weighting
    for(it in 1:maxit){
      if(returnIterations && mn[1] == 1)
        iterations <- rbind(iterations, matrix(xn,nrow=1))

      d <- xn - xb
      
      # Break if little change from last iteration
      if(it>1 && sum(d^2) < std) break;
      
      xb <- xn
      dif <- smoothed[i,]-xn
      
      # Suppress baseline by weighted difference
      f <- dif[dif >= 0]
      if(length(f)>0)
        dif[dif >= 0] <- f*wi
      xn <- xn + dif
      
      # Secondary smoothing
      xn <- limSolve::Solve.banded(U2,2,2,xn)
    }
    baseline[i,] <- xn
  }
  corrected <- spectra-baseline
  if(returnIterations && mn[1] == 1)
    return(list(baseline=baseline, corrected=corrected, smoothed=smoothed, iterations=iterations))
  else
    return(list(baseline=baseline, corrected=corrected, smoothed=smoothed))
}

# Example:
library(baseline)
data(milk)
res <- baseline.irls2(milk$spectra[1,,drop=FALSE], lambda2=11, returnIterations = TRUE)
matplot(t(res$iterations), type='l')
