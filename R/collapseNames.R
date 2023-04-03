#' Collapse dataset names
#'
#' This function has been superseded by \code{\link{collapseDim}} which is a
#' more generalized version of this function. Please use this one instead!
#'
#' This function will remove names in the data dimension which are the same for
#' each element (meaning that this data dimension contains exactly one element)
#'
#'
#' @param x MAgPIE object
#' @param collapsedim If you want to remove the names of particular dimensions
#' provide the dimensions here. Since the function only works in the third dimension,
#' you have to count from there on (e.g. dim = 3.2 refers to collapsedim = 2).
#' Alternatively, you can also specify the name of the dimension. Default: NULL.
#' CAUTION with parameter collapsedim! You could also force him to remove dimnames,
#' which are NOT the same for each element and so create duplicates in dimnames.
#' @param preservedim If you want to remove the name of particular dimensions except some,
#' you can specify the dimension(s) to preserve here. See collapsedim for naming convention.
#' Note that preservedim will be ignored in the case, of a specified collapsedim
#' @return The provided MAgPIE object with collapsed names
#' @author Jan Philipp Dietrich, David Klein, Xiaoxi Wang
#' @seealso \code{\link{collapseDim}}, \code{\link{getItems}},
#' \code{"\linkS4class{magpie}"}
#' @export collapseNames

collapseNames <- function(x, collapsedim = NULL, preservedim = NULL) {
  .dimCodeExtra <- function(dim, x) {
    if (is.null(dim)) return(NULL)
    if (is.character(dim)) return(dimCode(dim, x))
    if (all(dim > 3 & round(dim) == 3)) return(dim)
    # convert old dim convention in collapseNames (just specificantion of
    # sub-dimension) to new convention (maindim.subdim)
    return(dim / 10 + 3)
  }
  if (is.null(collapsedim)) return(collapseDim(x, keepdim = c(1:2, .dimCodeExtra(preservedim, x))))
  return(collapseDim(x, dim = .dimCodeExtra(collapsedim, x)))
}
