#' Unwrap
#'
#' Creates a higher dimensional array by separating all subdimensions in
#' the third dimension of a MAgPIE object and returning them as
#' separate dimension.
#'
#'
#' @param x A MAgPIE object
#' @param sep deprecated, please do not use anymore
#' @return An array with the full dimensionality of the original data
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{wrap}},\code{\link{fulldim}}
#' @examples
#'
#' a <- as.magpie(array(1:6, c(3, 2), list(c("bla", "blub", "ble"), c("up", "down"))))
#' unwrap(a)
#' @export unwrap
unwrap <- function(x, sep = NULL) {
  if (!is.null(sep)) warning("\"sep\" argument is not supported anymore.")
  if (!is.magpie(x)) stop("Input is not a MAgPIE object. unwrap works only for MAgPIE objects")
  dim3 <- unname(getItems(x, dim = 3, split = TRUE))
  dim <- c(unname(dimnames(x)[1:2]), dim3)
  ndim3 <- sapply(dim3, length)
  if (length(ndim3) == 1 && ndim3 == 0) ndim3 <- dim(x)[3]
  ndim <- c(dim(x)[1:2], ndim3)
  if (anyDuplicated(as.data.table(getItems(x, dim = 3)))) stop("Malformed MAgPIE object. Duplicated names detected!")
  if (prod(ndim3) != dim(x)[3]) {
     stop("MAgPIE object is sparse in 3rd dimension but needs to be complete in order to be unwrapped!")
  }
  reorder <- dimnames(wrap(array(NA, ndim, dim), list(1, 2, NA), sep = "."))[[3]]
  if (!is.null(reorder)) x <- x[, , reorder]
  return(array(as.vector(x), ndim, dim))
}
