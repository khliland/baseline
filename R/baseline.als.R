#' @title Asymmetric Least Squares
#' 
#' @description Baseline correction by 2nd derivative constrained weighted regression.
#' Original algorithm proposed by Paul H. C. Eilers and Hans F.M. Boelens
#' 
#' @details Iterative algorithm applying 2nd derivative constraints. Weights from
#' previous iteration is \code{p} for positive residuals and \code{1-p} for
#' negative residuals.
#' 
#' @aliases baseline.als als
#' @param spectra Matrix with spectra in rows
#' @param lambda 2nd derivative constraint
#' @param p Weighting of positive residuals
#' @param maxit Maximum number of iterations
#' @return \item{baseline }{Matrix of baselines corresponding to spectra
#' \code{spectra}} \item{corrected }{Matrix of baseline corrected spectra}
#' \item{wgts }{Matrix of final regression weights}
#' @author Kristian Hovde Liland and Bjørn-Helge Mevik
#' @references Paul H. C. Eilers and Hans F.M. Boelens: Baseline Correction
#' with Asymmetric Least Squares Smoothing
#' @keywords baseline spectra
#' @importFrom SparseM as.matrix.csr
#' @examples
#' 
#' data(milk)
#' bc.als <- baseline(milk$spectra[1,, drop=FALSE], lambda=10, method='als')
#' \dontrun{
#' plot(bc.als)
#' }
#' @export
baseline.als <- function(spectra, lambda=6, p=0.05, maxit = 20){
  ## Eilers baseline correction for Asymmetric Least Squares
  ## Migrated from MATLAB original by Kristian Hovde Liland
  ## $Id: baseline.als.R 170 2011-01-03 20:38:25Z bhm $
  #
  # INPUT:
  # spectra - rows of spectra
  # lambda  - 2nd derivative constraint
  # p       - regression weight for positive residuals
  #
  # OUTPUT:
  # baseline  - proposed baseline
  # corrected - baseline corrected spectra
  # wgts      - final regression weights
  
  mn       <- dim(spectra)
  baseline <- matrix(0,mn[1],mn[2])
  wgts     <- matrix(0,mn[1],mn[2])
  
  # Empty matrix (5 x m)
  W <- matrix(0.0, 5, mn[2])
  
  # Diagonal sparse matrix in compact format (5 x m)
  DD <- .create_DD(mn[2])*10^lambda
  
  # Iterate through spectra
  for(i in 1:mn[1]){
    w  <- rep.int(1, mn[2])
    y  <- spectra[i,]
    
    # Iterate restriction and weighting
    for(it in 1:maxit){
      W[3,] <- w
      
      # Restricted regression
      z <- .s2band(W+DD, w*y)
      w_old <- w
      
      # Weights for next regression
      w <- p * (y > z) + (1 - p) * (y < z)
      sw <- sum(w_old != w)
      
      # Break if no change from last iteration
      if(sw == 0)
        break;
    }
    baseline[i,] <- z
    wgts[i,] <- w
  }
  corrected <- spectra-baseline
  list(baseline=baseline,corrected=corrected,wgts=wgts)
}

# Compact representation of band-diagonal second derivative operator matrix squared
.create_DD <- function(p){
  to_band <- rbind(rep(0,5),rep(0,5),crossprod(diff(diag(5), differences=2)),rep(0,5),rep(0,5))
  the_band <- diag(to_band)
  for(i in 1:4){
    to_band  <- to_band[-1,]
    the_band <- c(the_band,diag(to_band))
  }
  the_band <- matrix(the_band,nrow=5,byrow=TRUE)
  the_band[,c(1,2,rep(3,p-4),4,5)]
}


.baseline.als_old <- function(spectra, lambda=6, p=0.05, maxit = 20){
## Eilers baseline correction for Asymmetric Least Squares
## Migrated from MATLAB original by Kristian Hovde Liland
## $Id: baseline.als.R 170 2011-01-03 20:38:25Z bhm $
#
# INPUT:
# spectra - rows of spectra
# lambda  - 2nd derivative constraint
# p       - regression weight for positive residuals
#
# OUTPUT:
# baseline  - proposed baseline
# corrected - baseline corrected spectra
# wgts      - final regression weights

  mn       <- dim(spectra)
  baseline <- matrix(0,mn[1],mn[2])
  wgts     <- matrix(0,mn[1],mn[2])

  # Sparse empty matrix (m x m)
  empt     <- as.matrix.csr(0,mn[2],mn[2])

  # Diagonal sparse matrix (m x m)
  speye    <- empt
  diag(speye) <- 1
  D  <- diff(speye,differences=2)
  DD <- 10^lambda*t(D)%*%D

  # Iterate through spectra
  for(i in 1:mn[1]){
    w  <- rep.int(1, mn[2])
    y  <- spectra[i,]

    # Iterate restriction and weighting
    for(it in 1:maxit){
      W <- empt
      diag(W) <- w

      # Restricted regression
      z <- solve(W+DD,w*y)
      w_old <- w

      # Weights for next regression
      w <- p * (y > z) + (1 - p) * (y < z)
      sw <- sum(w_old != w)

      # Break if no change from last iteration
      if(sw == 0)
        break;
    }
    baseline[i,] <- z
    wgts[i,] <- w
  }
  corrected <- spectra-baseline
  list(baseline=baseline,corrected=corrected,wgts=wgts)
}

