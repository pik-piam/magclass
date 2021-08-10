#' Get regions
#'
#' Extracts regions of a MAgPIE-object
#'
#'
#' @aliases getRegions getRegions<-
#' @param x MAgPIE object
#' @param value Vector containing the new region names of the MAgPIE objects.
#' If you also want to change the mapping of regions to cell please use
#' \code{\link{getRegionList}} instead.
#' @return Regions of the MAgPIE-object
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getYears}}, \code{\link{getNames}},
#' \code{\link{getCPR}}, \code{\link{read.magpie}}, \code{\link{write.magpie}},
#' \code{"\linkS4class{magpie}"}
#' @examples
#'
#' # a <- read.magpie("example.mz")
#' # getRegions(a)
#' @export
getRegions <- function(x) {
  return(getItems(x, dim = 1.1))
}

#' @describeIn getRegions overwrite region names
#' @export
"getRegions<-" <- function(x, value) { #nolint
  .Deprecated("getItems")
  getCells(x) <- value
  return(x)
}
