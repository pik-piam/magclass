#' Count sub-dimensions
#'
#' Functions to count the subdimensions of an
#' MAgPIE-object
#'
#' @param x A MAgPIE-object
#' @param dim main dimension in which the sub-dimensions should be counted. If NULL
#' the sum of all subdimensions is returned
#' @return Number of subdimensions
#' @author Jan Philipp Dietrich
#' @examples
#'
#'   a <- maxample("animal")
#'   ndim(a)
#'   ndim(a,1)
#'   ndim(a,2)
#'   ndim(a,3)
#'
#' @export
ndim <- function(x, dim = NULL) {
  if (is.null(dim)) return(ndim(x, 1) + ndim(x, 2) + ndim(x, 3))
  if (!(dim %in% 1:3)) stop("Invalid dim selection (Allowed: 1, 2 or 3).")
  tmp <- dimnames(x)[[dim]][1]
  if (is.null(tmp)) {
    # fallback to set information
    tmp <- names(dimnames(x))[dim]
  }
  if (is.null(tmp)) return(0)
  ndim <- nchar(gsub("[^.]", "", tmp)) + 1
  return(ndim)
}
