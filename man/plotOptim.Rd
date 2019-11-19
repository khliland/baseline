%%% $Id: plotOptim.Rd 193 2012-06-24 21:13:42Z kristl $
\name{plotOptim}
\alias{plotOptim}

\title{ Plotting tool for result objects from optimization }
\description{
  A graphical user interface for plotting optimisation results, either
  one algorithm at the time or comparing algorithms.
}
\usage{
plotOptim(results)
}
\arguments{
  \item{results}{ Result list from optimization }
}
\details{
  \code{plotOptim} creates a user interface based on the supplied results.
  Curve and level plots from single algorithms or comparison of algorithms
  is avilable.
  
  For single algorithms subsets, levels corresponding to local or global
  minima, and averages can be extracted for plotting. For comparison of
  algorithms levels corresponding to local or global minima can be used,
  or levels corresponding to the minimum when averaging over selected
  values of the regression parameter, e.g. selected components in PLSR.
}
\author{ Kristian Hovde Liland and Bjørn-Helge Mevik }
\keyword{ baseline }
\keyword{ spectra }