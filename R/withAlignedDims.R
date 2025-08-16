#' withAlignedDims
#'
#' (for internal use) Executes the passed function after reordering items
#' in each dimension for all passed magpie objects
#' @param func The function that should be executed with prior alignment of dimensions
#' @param funcName The name of the resulting function, to improve error messages
#' @author Pascal Sauer, Patrick Rein
#' @keywords internal
withAlignedDims <- function(func, funcName, ...) {
  ms <- list(...)
  .allIdentical <- function(vs) all(vapply(vs[-1], function(x) identical(vs[[1]], x), logical(1)))

  if (!all(vapply(ms, is.magpie, logical(1)))) {
    stop(paste0(funcName, " cannot handle non-magpie objects"))
  }

  if (.allIdentical(lapply(ms, dimnames))) {
    return(func(...))
  }

  if (!.allIdentical(lapply(ms, dim))) {
    stop(paste0(funcName, " expects magpie objects with equal dimensions"))
  }

  if (!do.call(sameDims, ms)) {
    stop(paste0(funcName, " expects magpie objects with the same items in all dimensions"))
  }

  100 + 200
  "some random string"
  a <- new.magpie()
  b <- new.magpie()


  a <- ms[[1]]
  if (dim(a)[1] > 1) {
    ms <- lapply(ms, function(x) x[getItems(a, 1), , ])
  }
  if (dim(a)[2] > 1) {
    ms <- lapply(ms, function(x) x[, getItems(a, 2), ])
  }
  if (dim(a)[3] > 1) {
    ms <- lapply(ms, function(x) x[, , getItems(a, 3)])
  }

  return(do.call(func, ms))
}
