#' @title Iterative Restricted Least Squares
#' 
#' @description An algorithm with primary smoothing and repeated baseline suppressions and
#' regressions with 2nd derivative constraint
#' 
#' @aliases baseline.irls irls
#' @param spectra Matrix with spectra in rows
#' @param lambda1 2nd derivative constraint for primary smoothing
#' @param lambda2 2nd derivative constraint for secondary smoothing
#' @param maxit Maximum number of iterations
#' @param wi Weighting of positive residuals
#' @return \item{baseline }{Matrix of baselines corresponding to spectra
#' \code{spectra}} \item{corrected }{Matrix of baseline corrected spectra}
#' \item{smoothed }{Matrix of primary smoothed spectra}
#' @author Kristian Hovde Liland and Bjørn-Helge Mevik
#' @keywords baseline spectra
#' @examples
#' 
#' data(milk)
#' bc.irls <- baseline(milk$spectra[1,, drop=FALSE], method='irls')
#' \dontrun{
#' 	plot(bc.irls)
#' }
#' @export
baseline.irls <- function(spectra, lambda1=5, lambda2=9, maxit=200, wi=0.05){
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
  DD <- .create_DD(mn[2])
  
  # Empty matrix (5 x m)
  W <- matrix(0.0, 5, mn[2])
  W[3,] <- 1.0

  # Primary and secondary smoother
  U1 <- W+DD*10^lambda1
  U2 <- W+DD*10^lambda2

  # Primary smoothing
  smoothed <- t(Solve.banded(U1,2,2,t(spectra)))

  # Iterate through spectra
  for(i in 1:mn[1]){
    xn <- smoothed[i,]
    xb <- numeric(mn[2])
    std <- sd(xn)
    
    # Iterate restriction and weighting
    for(it in 1:maxit){
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
      xn <- Solve.banded(U2,2,2,xn)
    }
    baseline[i,] <- xn
  }
  corrected <- spectra-baseline
  list(baseline=baseline, corrected=corrected, smoothed=smoothed)
}

.baseline.irls_old <- function(spectra, lambda1=5, lambda2=9, maxit=200, wi=0.05){
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

  # Sparse empty matrix (m x m)
  speye <- as.matrix.csr(0,mn[2],mn[2])

  # Diagonal sparse matrix (m x m)
  diag(speye) <- 1
  D <- diff(speye,differences=2)
  w <- rep.int(1, mn[2])
  W <- as.matrix.csr(0,mn[2],mn[2])
  diag(W) <- w

  # Primary and secondary smoother
  U1 <- chol(W+10^lambda1*t(D)%*%D)
  U2 <- chol(W+10^lambda2*t(D)%*%D)

  # Primary smoothing
  smoothed <- t(backsolve(U1,t(spectra)))

  # Iterate through spectra
  for(i in 1:mn[1]){
    xn <- smoothed[i,]
    xb <- numeric(mn[2])
    std <- sd(xn)

    # Iterate restriction and weighting
    for(it in 1:maxit){
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
      xn <- backsolve(U2,xn)
    }
    baseline[i,] <- xn
  }
  corrected <- spectra-baseline
  list(baseline=baseline, corrected=corrected, smoothed=smoothed)
}
