#' @describeIn ncells count regions
#' @export
nregions <- function(x) {
  return(length(getRegions(x)))
}