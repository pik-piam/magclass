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
#' @family Aggregation
#' @export
dimSums <- function(x, dim = 3, na.rm = FALSE) { # nolint: object_name_linter.
  if (!is.magpie(x)) {
    stop("Input is not a MAgPIE object!")
  }
  dim <- sort(dimCode(dim, x), decreasing = TRUE)
  if (any(dim == 0)) {
    stop("Invalid dimension(s) specified")
  }
  if (length(x) == 0) {
    return(NULL)
  }

  for (d in dim) {
    getItems(x, dim = d, raw = TRUE) <- NULL
  }
  noNames <- which(vapply(dimnames(x), is.null, logical(1)))
  for (i in noNames) {
    getItems(x, dim = i) <- rep("dummy", dim(x)[i])
  }

  # sum each requested (sub-)dimension one axis at a time via a compiled
  # aperm + reshape + rowsum, instead of melting the whole array into a
  # data.frame and grouping with tapply (which is O(N) with a very large
  # constant and dominates both time and memory on large objects)
  for (d in 1:3) {
    xdim <- dim(x)
    dn <- dimnames(x)
    labels <- dn[[d]]
    groups <- unique(labels)
    if (length(groups) == xdim[d]) {
      next
    }
    gidx <- match(labels, groups)
    perm <- c(d, setdiff(1:3, d))
    xp <- aperm(x, perm)
    dim(xp) <- c(xdim[d], prod(xdim) / xdim[d])
    reduced <- rowsum(xp, group = gidx, reorder = FALSE, na.rm = na.rm)
    dim(reduced) <- c(length(groups), xdim[-d])
    x <- aperm(reduced, order(perm))
    dn[[d]] <- groups
    dimnames(x) <- dn
  }

  x <- new("magpie", x)
  for (i in noNames) {
    getItems(x, dim = i) <- NULL
  }
  return(x)
}
