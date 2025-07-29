#' sameDims
#'
#' Checks whether magpie objects have the same items in every dimension (ignoring order).
#'
#' @param ... Various magpie objects
#'
#' @returns A logical indicating whether the magpie objects have the same items in every dimension.
#' @author Pascal Sauer, Patrick Rein
#' @export
sameDims <- function(...) {
  magpies <- list(...)
  if (!all(vapply(magpies, is.magpie, logical(1)))) {
    stop("sameDims cannot handle non-magpie objects")
  }

  if (length(magpies) < 2) {
    return(TRUE)
  }

  .allSetequal <- function(vs) all(vapply(vs[-1], function(x) setequal(vs[[1]], x), logical(1)))
  return(.allSetequal(lapply(magpies, function(m) getItems(m, 1))) &&
           .allSetequal(lapply(magpies, function(m) getItems(m, 2))) &&
           .allSetequal(lapply(magpies, function(m) getItems(m, 3))))
}
