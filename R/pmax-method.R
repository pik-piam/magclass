#
# Start Preamble to set up generic pmax
#
existingPmaxGeneric <- getGeneric("pmax")
if (!is.null(existingPmaxGeneric)) {
  if (!all(existingPmaxGeneric@signature == c("..."))) {
    warning(paste0("Redefining pmax generic function with new signature \"...\", ",
                   "while there already is a pmax generic with signature: ", existingPmaxGeneric@signature))
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
