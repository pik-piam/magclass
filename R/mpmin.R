#' mpmin/mpmax
#'
#' magclass-aware pmin/pmax, which calculate the parallel minima/maxima of the input values
#'
#' \code{\link{pmin}}/\code{\link{pmax}} do not care about the order of items in a magclass
#' object's dim. mpmin/mpmax reoder items in each dim so they are in the same order.
#' @param a A magpie object
#' @param b Another magpie object, with the same dimensions as \code{a}
#' @return A new magpie object that contains the minimum/maximum of values between \code{a} and \code{b}
#' @author Pascal Sauer, Patrick Rein
#'
#' @export
mpmin <- function(...) {
  return(withAlignedDims(pmin, "mpmin", ...))
}