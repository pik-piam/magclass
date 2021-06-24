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
  if (!is.magpie(x)) stop("Input is not a MAgPIE object!")
  dim <- sort(dimCode(dim, x), decreasing = TRUE)
  if (any(dim == 0)) stop("Invalid dimension(s) specified")
  for (d in dim) getItems(x, dim = d, raw = TRUE) <- NULL
  noNames <- which(sapply(dimnames(x), is.null)) #nolint
  for (i in noNames) {
      getItems(x, dim = i) <- rep("dummy", dim(x)[i])
  }
  xd <- as.data.frame.table(x)
  out <- new("magpie", tapply(xd[[4]], xd[1:3], sum, na.rm = na.rm))
  for (i in noNames) {
      getItems(out, dim = i) <- NULL
  }
  return(out)
}
