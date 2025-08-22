#' commonYears
#'
#' Given (a list of) magclass objects, return the years (getYears) they have in common,
#' ignoring magclass objects with 0 or 1 years.
#'
#' @param ... One or more magclass objects, or alternatively a list of magclass objects.
#' @return The years (as returned by getYears) the input objects have in common.
#'
#' @author Pascal Sauer
#' @export
commonYears <- function(...) {
  x <- list(...)
  if (length(x) == 1 && is.list(x[[1]])) {
    x <- x[[1]]
  }
  stopifnot(vapply(x, is.magpie, logical(1)))
  x <- Filter(function(i) nyears(i) > 1, x)
  return(Reduce(intersect, lapply(x, getYears)))
}
