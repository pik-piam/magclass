#' Summation over dimensions
#'
#' This function sums over any (sub-)dimension of a magpie object
#'
#' @param x A MAgPIE-object
#' @param dim The dimensions(s) to sum over. A vector of dimension codes or dimension names.
#' See \code{\link{dimCode}} for more information
#' @param na.rm logical. Should missing values (including NaN) be omitted from
#' the calculations?
#' @return A MAgPIE object with values summed over the specified dimensions
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{rowSums}}, \code{\link{getItems}}, \code{\link{dimCode}}
#' @examples
#' a <- maxample("animal")
#' dimSums(a, dim = c(1, 2, 3.2))
#' dimSums(a, dim = c("x", "y", "cell", "month"))
#' @export
dimSums <- function(x, dim = 3, na.rm = FALSE) { #nolint
  return(magpply(X = x, FUN = sum, DIM = dim, na.rm = na.rm))
}
