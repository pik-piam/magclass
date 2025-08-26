#'
#' @title pmin/pmax
#' @name magpie-pmin-pmax
#' @aliases pmin pmax pmin,magpie-method pmin,ANY-method pmax,magpie-method pmax,ANY-method
#' @description magclass-aware pmin/pmax, which calculate the parallel minima/maxima of the input values
#' @details
#' \code{\link[base]{pmin}}/\code{\link[base]{pmax}} do not care about the order of items in a magclass
#' object's dim. magclass pmin/pmax reorder items in each dim so they are in the same order.
#' @param ... Multiple magpie objects with dimensions that each are either the same or only
#' contain one or no item among all objects.
#' @param na.rm Passed to \code{\link[base]{pmin}}
#' @return A new magpie object that contains the minimum/maximum of values between all the passed magpie objects.
#' @usage NULL
#' @docType methods
#' @keywords methods
#' @author Pascal Sauer, Patrick Rein
NULL

#
# Start Preamble to set up generic pmin
#
existingPminGeneric <- getGeneric("pmin")
if (!is.null(existingPminGeneric)) {
  if (any(existingPminGeneric@signature != "...")) {
    warning("Redefining pmin generic function with new signature \"...\", ",
            "while there already is a pmin generic with signature: ",
            paste0(existingPminGeneric@signature, collapse = ", "))
  }
}
setGeneric("pmin", signature = "...")
#
# End Preamble
#

#' @rdname magpie-pmin-pmax
#' @exportMethod pmin
setMethod("pmin", "magpie", function(..., na.rm = FALSE) { # nolint: object_name_linter.
  return(withAlignedDims(function(...) base::pmin(..., na.rm = na.rm),
                         "pmin",
                         ...))
})
