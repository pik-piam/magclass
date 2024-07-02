#' Match dimensions of a magpie object to those of a reference object
#'
#' A helper that restricts and expands a magpie object `x` to the size of a
#' magpie object `ref`.  Dimension names not present in `x` are added and set to
#' the value provided by `fill`.  Dimension names not present in `ref` are
#' cropped.
#'
#' @md
#' @param x A `magpie` object to be modified.
#' @param ref A `magpie` object used as a reference for the modification.
#'   Returns `x` if `ref` is `NULL`.
#' @param fill Value to be set in new dimensions.
#' @param dim Subset of dimensions for which the matching should be done.  Can
#'   be either a number between 1 and 3 or a vector of these.  Defaults to all
#'   dimensions (i.e. `1:3`).
#'
#' @return The modified `magpie` object.
#'
#' @author Falk Benke
#'
#' @export
matchDim <- function(x, ref, dim = 1:3, fill = NA) {

  if (is.null(ref)) {
    return(x)
  }

  if (!is.magpie(x)) {
    stop("`x` must be a magpie object, not `", paste(class(x), collapse = ", "),
         "`")
  }

  if (!is.magpie(ref)) {
    stop("`ref` must be a magpie object or NULL, not `",
         paste(class(ref), collapse = ", "), "`")
  }

  if (!is.numeric(dim) || any(!dim %in% 1:3)) {
    stop("`dim` must be a numeric vector containing only numbers between 1 ",
         "and 3, not `", utils::capture.output(dput(dim)), "`")
  }

  for (i in dim) {
    if (ndim(x, dim = i) != ndim(ref, dim = i)) {
      stop(
        paste0(
          "Unsupported case: magclass objects x and ref have different number ",
          "of subdimensions in dimension ", i
        )
      )
    }
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
