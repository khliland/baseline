%%% $Id: baseline.peakDetection.Rd 193 2012-06-24 21:13:42Z kristl $
\name{baseline.peakDetection}
\alias{baseline.peakDetection}
\alias{peakDetection}

\title{ Simultaneous Peak Detection and Baseline Correction }
\description{
  A translation from Kevin R. Coombes et al.'s MATLAB code for detecting peaks and removing baselines
}
\usage{
baseline.peakDetection(spectra, left, right, lwin, rwin, snminimum,
mono=0, multiplier=5, left.right, lwin.rwin)
}
\arguments{
  \item{spectra}{ Matrix with spectra in rows }
  \item{left}{ Smallest window size for peak widths }
  \item{right}{ Largest window size for peak widths }
  \item{lwin}{ Smallest window size for minimums and medians in peak removed spectra }
  \item{rwin}{ Largest window size for minimums and medians in peak removed spectra }
  \item{snminimum}{ Minimum signal to noise ratio for accepting peaks }
  \item{mono}{ Monotonically decreasing baseline if \code{mono}>0 }
  \item{multiplier}{ Internal window size multiplier }
  \item{left.right}{ Sets eflt and right to value of \code{left.right} }
  \item{lwin.rwin}{ Sets lwin and rwin to value of \code{lwin.rwin} }
}
\details{
  Peak detection is done in several steps sorting out real peaks through different criteria.
  Peaks are removed from spectra and minimums and medians are used to smooth the remaining parts of the spectra.
  If \code{snminimum} is omitted, y3, midspec, y and y2 are not returned (faster)
}
\value{
  \item{baseline }{Matrix of baselines corresponding to spectra \code{spectra}}
  \item{corrected }{Matrix of baseline corrected spectra}
  \item{peaks}{Final list of selected peaks}
  \item{sn}{List signal to noise ratios for peaks}
  \item{y3}{List of peaks prior to singal to noise selection}
  \item{midspec}{Mid-way baseline estimation}
  \item{y}{First estimate of peaks}
  \item{y2}{Second estimate of peaks}
}
\references{ KEVIN R. COOMBES et al.:
  Quality control and peak finding for proteomics data collected from nipple aspirate fluid
  by surface-enhanced laser desorption and ionization. }
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\examples{
data(milk)
bc.peakDetection <- baseline(milk$spectra[1,, drop=FALSE], method='peakDetection',
	left=300, right=300, lwin=50, rwin=50)
\dontrun{
	plot(bc.peakDetection)
}
}

\keyword{ baseline }
\keyword{ spectra }
