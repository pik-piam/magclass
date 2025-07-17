#' mcalc
#'
#' magclass-aware pmin, which calculates the parallel minima of the input values
#'
#' \code{\link{pmin}} does not care about the order of items in a magclass object's dim.
#' mpmin reoders items in each dim so they are in the same order.
#' @param a A magpie object
#' @param b Another magpie object, with the same dimensions as \code{a}
#' @return A new magpie object that contains the minimum of values between \code{a} and \code{b}
#' @author Pascal Sauer
#'
#' @export
mpmin <- function(a, b) {
  if (!is.magpie(a) || !is.magpie(b)) stop("mpmin expects two magpie objects")

  if (identical(dimnames(a), dimnames(b))) {
    return(pmin(a, b))
  }

  if (!identical(dim(a), dim(b))) stop("mpmin expects magpie objects with equal dimensions")

  itemsWarning <- "mpmin expects magpie objects with equal items in dimensions"
  if (dim(a)[1] > 1) {
    if (!setequal(getItems(a, 1), getItems(b, 1))) stop(itemsWarning)
    b <- b[getItems(a, 1), , ]
  }
  if (dim(a)[2] > 1) {
    if (!setequal(getItems(a, 2), getItems(b, 2))) stop(itemsWarning)
    b <- b[, getItems(a, 2), ]
  }
  if (dim(a)[3] > 1) {
    if (!setequal(getItems(a, 3), getItems(b, 3))) stop(itemsWarning)
    b <- b[, , getItems(a, 3)]
  }

  return(pmin(a, b))
}