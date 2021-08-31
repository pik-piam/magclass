#' @describeIn ncells count regions
#' @export
nregions <- function(x) {
  regions <- getItems(x, dim = 1.1)
  if (is.null(regions)) return(dim(x)[1])
  return(length(regions))
}
