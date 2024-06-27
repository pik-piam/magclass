#' match object dimensions of a magclass object to the dimensions of a reference object
#'
#' A helper that restricts and expands a magclass object x to the size of a magclass object ref.
#' Dimension names not present in x are added and set to the value provided by 'fill'.
#' Dimension names not present in ref are cropped.
#'
#' @param x a magclass object to be modified
#' @param ref a magclass object used as a reference for the modification
#' @param fill value to be set in new dimensions
#' @param dim subset of dimensions for which the matching should be done.
#' Can be either a number between 1 and 3 or a vector of these.
#' Set to NULL (default) for matching all dimensions.
#' @author Falk Benke
#'
#' @export
matchDim <- function(x, ref, dim = NULL, fill = NA) {

  if (is.null(dim)) {
    dim <- c(1, 2, 3)
  } else if (length(setdiff(dim, c(1, 2, 3))) > 0) {
    stop("argument 'dim' can only contain numbers between 1 and 3")
  }

  if (3 %in% dim && ndim(x, dim = 3) != ndim(ref, dim = 3)) {
    stop(
      "Unsupported case: magclass objects x and ref have different number of ",
      "subdimensions in third dimension."
    )
  }

  # extend new object to the union of both objects
  d1 <- if (1 %in% dim) union(getItems(x, dim = 1), getItems(ref, dim = 1)) else getItems(x, dim = 1)
  d2 <- if (2 %in% dim) union(getItems(x, dim = 2), getItems(ref, dim = 2)) else getItems(x, dim = 2)
  d3 <- if (3 %in% dim) union(getItems(x, dim = 3), getItems(ref, dim = 3)) else getItems(x, dim = 3)

  r <- new.magpie(
    cells_and_regions = d1,
    years = d2,
    names = d3,
    fill = fill,
    sets = names(dimnames(ref))
  )

  # copy over values from x
  r[getItems(x, dim = 1), getItems(x, dim = 2), getItems(x, dim = 3)] <- x

  # restrict object to dimensions of ref
  if (1 %in% dim) r <- r[getItems(ref, dim = 1), , ]
  if (2 %in% dim) r <- r[, getItems(ref, dim = 2), ]
  if (3 %in% dim) r <- r[, , getItems(ref, dim = 3)]

  return(r)
}
