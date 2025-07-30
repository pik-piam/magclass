#' @rdname mpmin
#' @export
mpmax <- function(...) {
  return(withAlignedDims(pmax, "mpmax", ...))
}
