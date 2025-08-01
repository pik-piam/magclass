#' cbind method for MAgPIE objects
#'
#' cbind method for MAgPIE-objects. Issues a warning that mbind should be used
#' if all objects are magpie objects and continues afterwards.
#'
#' @aliases cbind.magpie
#' @param ... see \link[base]{cbind}
#' @param deparse.level see \link[base]{cbind}
#' @author Patrick Rein
#' @export

cbind.magpie <- function(..., deparse.level = 1) { # nolint: object_name_linter.
  warning("Using cbind on magpie objects is not adviced. Use mbind instead.")
  objectsToCbind <- lapply(list(...), function(o) {
    if (inherits(o, "magpie")) {
      return(o@.Data)
    } else {
      return(o)
    }
  })
  return(do.call(cbind, objectsToCbind, deparse.level))
}
