#' @title MALDI-TOF mass spectra
#' 
#' @description Matrix of 45 spectra of 21451 m/z values from MALDI-TOF on mixed milk
#' samples.
#' 
#' \code{cow} is the concentration of cow milk in mixed samples of cow, goat,
#' and ewe milk.
#' 
#' @name milk
#' @docType data
#' @format A data frame with 45 observations on the following 2 variables.
#' \describe{ \item{list("cow")}{a numeric vector} \item{list("spectra")}{a
#' matrix with 21451 columns} }
#' @references Kristian Hovde Liland, Bjørn-Helge Mevik, Elling-Olav Rukke,
#' Trygve Almøy, Morten Skaugen and Tomas Isaksson (2009) Quantitative whole
#' spectrum analysis with MALDI-TOF MS, Part I: Measurement optimisation.
#' \emph{Chemometrics and Intelligent Laboratory Systems}, \bold{96}(2),
#' 210--218.
#' @keywords datasets
#' @examples
#' 
#' data(milk)
#' \dontrun{
#' plot(milk$spectra[1,], type = "l")
#' }
NULL


#' @title XPS core line data
#' 
#' @description Matrix of x,y values from X-Ray Photoelectron Spectroscopy on test
#' sample.\cr The data are about the \code{Carbon} and \code{Oxygen} element
#' for \code{1s} shell.
#' 
#' @name XPSdata
#' @aliases C1s O1s
#' @docType data
#' @format A matrix with the following 2 variables (rows).  \describe{
#' \item{list("first row")}{is the abscissa, ( Binding Energy [eV] )}
#' \item{list("second row")}{is the Intensity, ( a.u. )} }
#' @seealso \link{baseline.shirley}
#' @keywords datasets
#' @examples
#' 
#' data(C1s)
#' data(O1s)
#' plot(C1s[1,], C1s[2,], type = "l")
#' plot(O1s[1,], O1s[2,], type = "l")
NULL

