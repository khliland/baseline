%%% $Id: baseline.TAP.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline.TAP}
\alias{baseline.TAP}
\alias{TAP}

\title{ TAP }
\description{
An implementation of Roman Svoboda and Jirí Málek's algorithm for baseline identification in kinetic anlaysis of derivative kinetic data.
}
\usage{
baseline.TAP(spectra, t, interval = 15, tol = 0.001)
}
\arguments{
  \item{spectra}{ Matrix with spectra in rows }
  \item{t}{ Optional vector of spectrum abcissa }
  \item{interval}{ Distance from spectrum end to starting points for the TAP (default = 15) }
  \item{tol}{ Tolerance of difference between iterations (default = 0.001) }
}
\details{
(i) A first approximation of the baseline equation is selected as the straight line between start and end of the curve.
(ii) Based on the first approximation of the baseline equation, the phase change progress parameter is calculated.
(iii) An updated equation of the baseline is calculated and the phase change progress parameter equation from step (ii).
(iv) The baseline equation from step (iii) is compared (point by point) with the one from the previous iteration. If the convergence criterion is met (the difference between every baseline value corresponding to two successive iterations was less than 0.1\%) the procedure is stopped and the final baseline equation is selected. If the convergence criterion is not fulfilled then a new iteration is carried out from step (ii) until convergence was achieved.
}
\value{
  \item{baseline }{Matrix of baselines corresponding to spectra \code{spectra}}
  \item{corrected }{Matrix of baseline corrected spectra}
}
\references{ Roman Svoboda and Jirí Málek:
  Importance of proper baseline identification for the subsequent kinetic analysis of derivative kinetic data, Journal of Thermal Analysis and Calorimetry. }
\author{ Kristian Hovde Liland }
\examples{
# My T
myT <- 40:170

# My artifical curve
myAlpha <- c(seq(0.01, 0.02, length.out=40),
             dnorm(seq(-3,3,length.out=51))/2+(0:50)/2000+0.02)
myAlpha <- c(myAlpha,
             seq(myAlpha[90]-0.001, 0.01, length.out=40))
myAlpha <- myAlpha - min(myAlpha)
myAlpha <- cumsum(dadt <- myAlpha/sum(myAlpha))

# Discrete derivative
mydAlpha <- c(0,diff(myAlpha)); mydAlpha <- matrix(mydAlpha, ncol=length(mydAlpha))
rm(myAlpha) # Throw away myAlpha

# Compute baseline from T and derivative
B <- baseline(mydAlpha, t=myT, method="TAP")

# Plot 
plot(B, xlab = "T", ylab = "da/dT")
}

\keyword{ baseline }
\keyword{ spectra }
