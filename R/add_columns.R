#' add_columns
#'
#' Function adds new columns to the existing magpie object.
#'
#' @param x MAgPIE object which should be extended.
#' @param dim The (sub)dimension to be filled either identified via
#' name or dimension code (see \code{\link{dimCode}} for more information)
#' @param addnm The new elements that should be added to the (sub)dimension
#' @param fill fill value of length 1 for the newly added columns (NA by default)
#' @return The extended MAgPIE object
#' @author Jan Philipp Dietrich, Benjamin Bodirsky
#' @seealso \code{\link{add_dimension}},\code{\link{dimCode}}
#' @examples
#' a <- maxample("animal")
#' a2 <- add_columns(a, addnm = c("horse", "magpie"), dim = "species", fill = 42)
#' getItems(a2, dim = 3)
#' getItems(a2, dim = 3, split = TRUE)
#' head(a2[, , "magpie"])
#' @export
add_columns <- function(x, addnm = "new", dim = 3.1, fill = NA) { #nolint
  if (length(dim) != 1) stop("dim must be a single (sub)dimension!")
  if (length(fill) != 1) stop("fill value must be of length 1")
  if (!dimExists(dim, x)) stop("dim \"", dim, "\" does not exist")
  x <- clean_magpie(x, what = "sets")
  if (length(addnm) == 0) return(x)
  dim <- dimCode(dim, x)
  add <- add_dimension(dimSums(x, dim = dim), dim = dim, nm = addnm)
  add[, , ] <- fill
  getSets(add) <- getSets(x)
  return(mbind(x, add))
}
