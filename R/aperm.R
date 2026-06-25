#' aperm method for magpie objects
#'
#' Permutes the dimensions of a magpie object and returns a plain array.
#' Since the magpie class has fixed dimension semantics (spatial, temporal,
#' data), permuting the main dimensions produces an object that is no longer
#' a valid magpie. This method therefore always returns a plain array.
#'
#' Use \code{\link{dimOrder}} to reorder sub-dimensions within a single main
#' dimension while preserving the magpie class.
#'
#' @param a A magpie object.
#' @param perm An integer vector giving the new permutation of dimensions,
#'   or NULL for reverse order. See \code{\link[base]{aperm}}.
#' @param ... Further parameters passed on to \code{\link[base]{aperm}}.
#' @return A plain array (magpie class attribute dropped).
#' @author Patrick Rein
#' @seealso \code{\link{dimOrder}}, \code{\link[base]{aperm}}
#' @examples
#' p <- maxample("pop")
#' a <- aperm(p, c(2, 1, 3))
#' class(a) # "array", not "magpie"
#' @export
aperm.magpie <- function(a, perm = NULL, ...) {
  aperm(as(a, "array"), perm = perm, ...)
}
