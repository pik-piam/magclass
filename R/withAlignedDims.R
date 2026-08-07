#' withAlignedDims
#'
#' (for internal use) Executes the passed function after reordering items
#' in each dimension for all passed magpie objects
#' @param func The function that should be executed with prior alignment of dimensions
#' @param funcName The name of the resulting function, to improve error messages
#' @author Pascal Sauer, Patrick Rein
#' @keywords internal
withAlignedDims <- function(func, funcName = "anonymous func", ...) {
  ms <- list(...)
  .allIdentical <- function(vs) all(vapply(vs[-1], function(x) identical(vs[[1]], x), logical(1)))

  if (!all(vapply(ms, is.magpie, logical(1)))) {
    stop(paste0(funcName, " cannot handle non-magpie objects"))
  }

  if (.allIdentical(lapply(ms, dimnames))) {
    return(func(...))
  }

  # collapseDim if the dim has a single item in all and they differ
  singleDifferingItemDims <- Filter(function(dimCode) {
    items <- lapply(ms, function(m) dimnames(m)[[dimCode]])
    return(all(lapply(items, length) <= 1) && !.allIdentical(items))
  }, 1:3)
  if (length(singleDifferingItemDims) > 0) {
    originalDimensionNames <- names(dimnames(ms[[1]]))
    ms <- lapply(ms, function(m) {
      m <- collapseDim(m, dim = singleDifferingItemDims)
      names(dimnames(m)) <- originalDimensionNames
      return(m)
    })
    if (.allIdentical(lapply(ms, dimnames))) {
      return(do.call(func, ms))
    }
  }

  if (!.allIdentical(lapply(ms, dim))) {
    stop(paste0(funcName, " expects magpie objects with equal dimensions"))
  }

  if (!do.call(sameDims, ms)) {
    stop(paste0(funcName, " expects magpie objects with the same items in all dimensions"))
  }

  a <- ms[[1]]
  for (d in 1:3) {
    if (dim(a)[d] == 1) {
      next
    }
    items <- getItems(a, d)
    # reordering by name is expensive, so only do it where the order actually differs
    ms <- lapply(ms, function(x) if (identical(getItems(x, d), items)) x else x[items, dim = d])
  }

  return(do.call(func, ms))
}
