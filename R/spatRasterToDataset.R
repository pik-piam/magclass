#' spatRasterToDataset
#'
#' Convert a SpatRaster to a SpatRasterDataset.
#'
#' @param x SpatRaster with names of the form "y[0-9]+..[varname]"
#' @return SpatRasterDataset
#' @author Pascal Sauer
#' @export
spatRasterToDataset <- function(x) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("The package \"terra\" is required!")
  }

  if (all(grepl("^y[0-9]+\\.\\.", names(x)))) {
    varnames <- unique(sub("^y[0-9]+\\.\\.", "", names(x)))
    datasets <- lapply(varnames, function(varname) {
      spatRaster <- x[paste0("\\.\\.", varname, "$")]
      terra::varnames(spatRaster) <- varname
      return(spatRaster)
    })
  } else {
    varnames <- names(x)
    datasets <- lapply(varnames, function(varname) {
      spatRaster <- x[varname]
      terra::varnames(spatRaster) <- varname
      return(spatRaster)
    })
  }
  x <- terra::sds(datasets)
  names(x) <- varnames
  return(x)
}
