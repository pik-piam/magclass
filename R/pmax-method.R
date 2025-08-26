#
# Start Preamble to set up generic pmax
#
existingPmaxGeneric <- getGeneric("pmax")
if (!is.null(existingPmaxGeneric)) {
  if (any(existingPmaxGeneric@signature != "...")) {
    warning("Redefining pmax generic function with new signature \"...\", ",
            "while there already is a pmax generic with signature: ",
            paste0(existingPmaxGeneric@signature, collapse = ", "))
  }
}
setGeneric("pmax", signature = "...")
#
# End Preamble
#

#' @rdname magpie-pmin-pmax
#' @exportMethod pmax
setMethod("pmax", "magpie", function(..., na.rm = FALSE) { # nolint: object_name_linter.
  return(withAlignedDims(function(...) base::pmax(..., na.rm = na.rm),
                         "pmax",
                         ...))
})
