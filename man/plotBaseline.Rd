%%% $Id: plotBaseline.Rd 193 2012-06-24 21:13:42Z kristl $
\name{plotBaseline}
\alias{plotBaseline}
\alias{plot,baseline-method}
\title{
  Plot method for "baseline" objects
}
\description{
  Plot the original spectrum, the estimated baseline, and the corrected
  spectrum.  Optionally zoom and pan plot, either with arguments or
  interactively.
}
\usage{
\S4method{plot}{baseline}(x, y, specNo = 1, grid = FALSE, labels = 1:n, rev.x = FALSE,
     zoom = NULL, ...)
plotBaseline(x, y, specNo = 1, grid = FALSE, labels = 1:n, rev.x = FALSE,
             zoom = list(xz = 1, yz = 1, xc = 0, yc = 0), ...)
}

\arguments{
  \item{x}{The \code{baseline} object to be plotted}
  \item{y}{Unused.  Ignored with a warning}
  \item{specNo}{The row number of the spectrum and baseline to plot.
  Defaults to 1}
  \item{grid}{Logical.  Whether to show a grid or not.  Defaults to \code{FALSE}}
  \item{labels}{Vector.  Labels for the x tick marks.  Defaults to \code{1:n}}
  \item{rev.x}{Logical.  Whether the spectrum should be reversed.
  Defaults to \code{FALSE}}
  \item{zoom}{Either \code{TRUE} (only for the plot method), which turns
  on the interactive zoom controls, or a list with components \code{xz},
  \code{xc}, \code{yz} and \code{yc}, which specifies the desired zoom
  and pan.  Defaults to no zoom or pan}
  \item{\dots}{Other arguments.  Currently ignored}
}
\details{
  The normal way to plot \code{baseline} objects is to use the
  \code{plot} method.  The \code{plotBaseline} function is the
  underlying work-horse function, and is not meant for interactive use.
}
\author{
  Kristian Hovde Liland and Bjørn-Helge Mevik
}
\note{
  Because the argument list of any plot method must start with \code{x},
  \code{y}, and the plot method for the \code{baseline} class does not
  use the \code{y} argument, all arguments except \code{x} must be named
  explicitly.  Positional matching will not work.
}
\seealso{
  \code{\link{baseline}}, \code{\linkS4class{baseline}},
  \code{\link{baselineGUI}}
}
\examples{
data(milk)
bl <- baseline(milk$spectra[1,, drop=FALSE])
\dontrun{
  # Computationally intensive
  plot(bl)
  plot(bl, zoom = TRUE)}
}
\keyword{baseline}
\keyword{plot}
