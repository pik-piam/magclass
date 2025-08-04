#' cbind method for MAgPIE objects
#'
#' cbind method for MAgPIE-objects. Issues a warning that mbind should be used
#' if all objects are magpie objects and continues afterwards.
#'
#' @aliases cbind.magpie
#' @param ... see \link[base]{cbind}
#' @param deparse.level see \link[base]{cbind}
#' @return A matrix as if cbind was applied to the internal vector of the MAgPIE objects
#' @author Patrick Rein
#' @export

cbind.magpie <- function(..., deparse.level = 1) { # nolint: object_name_linter.
  warning("Using cbind on magpie objects is not adviced as it is not merging based on element names. Use mbind instead.") # nolint: line_length_linter.
  objectsToCbind <- lapply(list(...), function(o) {
    if (inherits(o, "magpie")) {
      # Unwrap MAgPie objects so we only pass the internal vectors
      # to the cbind below
      return(o@.Data)
    } else {
      return(o)
    }
  })
  return(do.call(cbind, objectsToCbind, deparse.level))
}
