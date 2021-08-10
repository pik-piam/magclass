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
  if (sum(substr(dimnames(x)[[1]], 4, 4) != ".") > 0) { # not all regions have 3-character names (use slow method)
    output <- unique(as.vector(as.matrix(cbind.data.frame(strsplit(dimnames(x)[[1]], "\\."))[1, ])))
  } else {  # region names all have 3 characters -> fast method
    output <- unique(substr(dimnames(x)[[1]], 1, 3))
  }
  if (length(output) == 0) return(NULL)
  return(output)
}

#' @describeIn getRegions overwrite region names
#' @export
"getRegions<-" <- function(x, value) { #nolint
  .Deprecated("getItems")
  getCells(x) <- value
  return(x)
}
