#' Count elements
#'
#' Functions to count the number of cells/years/datasets/regions of an
#' MAgPIE-object
#'
#' @aliases ncells nyears ndata nregions
#' @param x A MAgPIE-object
#' @return \item{value}{The number of cells/years/datasets/regions of \code{x}}
#' @author Jan Philipp Dietrich
#' @examples
#'
#' a <- is.magpie(NULL)
#' ncells(a)
#' nyears(a)
#' ndata(a)
#' nregions(a)
#' @export
ncells <- function(x) {
  return(dim(x)[1])
}
