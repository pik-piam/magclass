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
  # sum() coerces logical to integer, rowsum() below rejects it outright
  if (is.logical(x)) {
    storage.mode(x@.Data) <- "integer"
  }
  # rowsum() only handles double and integer, so fall back to the generic
  # implementation for anything else (e.g. complex)
  if (!is.numeric(x)) {
    return(magpply(X = x, FUN = sum, DIM = dim, na.rm = na.rm))
  }

  for (axis in dim) {
    getItems(x, dim = axis, raw = TRUE) <- NULL
  }
  dimNames <- dimnames(x)
  x <- as(x, "array")

  # sum each requested (sub-)dimension one axis at a time via a compiled
  # reshape + rowsum, instead of melting the whole array into a data.frame
  # and grouping with tapply (which is O(N) with a very large constant and
  # dominates both time and memory on large objects). Items sharing a label
  # after the removal above form the groups to be summed; an axis left
  # without labels by that removal collapses to a single group.
  for (axis in 1:3) {
    axisLengths <- dim(x)
    if (is.null(dimNames[[axis]])) {
      if (axisLengths[axis] == 1) {
        next
      }
      groupLabels <- NULL
      nGroups <- 1L
      groupIndex <- rep(1L, axisLengths[axis])
    } else {
      groupLabels <- unique(dimNames[[axis]])
      nGroups <- length(groupLabels)
      if (nGroups == axisLengths[axis]) {
        next
      }
      groupIndex <- match(dimNames[[axis]], groupLabels)
    }
    # rowsum groups along rows, so axis has to lead; for axis = 1 it already
    # does and both permutations degenerate to a (free) reshape
    permOrder <- c(axis, setdiff(1:3, axis))
    reshaped <- if (axis == 1) x else aperm(x, permOrder)
    dim(reshaped) <- c(axisLengths[axis], prod(axisLengths[-axis]))
    summed <- rowsum(reshaped, group = groupIndex, reorder = FALSE, na.rm = na.rm)
    dim(summed) <- c(nGroups, axisLengths[-axis])
    x <- if (axis == 1) summed else aperm(summed, order(permOrder))
    dimNames[axis] <- list(groupLabels)
  }
  dimnames(x) <- dimNames

  return(new("magpie", x))
}
