#' Count elements
#' 
#' Functions to count the number of cells/years/datasets/regions of an
#' MAgPIE-object
#' 
#' 
#' @aliases ncells nyears ndata nregions
#' @param x A MAgPIE-object
#' @param fulldim specifies, how the object is treated. In case of FALSE, it is
#' assumed that x is 3 dimensional and dimnames(x)[[3]] is returned. In case of
#' TRUE, the dimnames of the real third dimension namesare returned
#' @return \item{value}{The number of cells/years/datasets/regions of \code{x}}
#' @author Jan Philipp Dietrich
#' @examples
#' 
#'   a <- is.magpie(NULL)
#'   ncells(a)
#'   nyears(a)
#'   ndata(a)
#'   nregions(a)
#'
#' @export
ncells <- function(x) {
  return(dim(x)[1])
}
