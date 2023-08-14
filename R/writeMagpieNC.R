#' writeMagpieNC
#'
#' Write a magpie object to a NetCDF file using the "terra" package.
#'
#' \code{\link{write.magpie}} can also write a magpie object to a NetCDF file, but
#' it is using the packages ncdf4 and raster, while this function uses terra.
#'
#' @param x a magpie object
#' @param filename the name of the NetCDF file to be created
#' @param ... additional arguments passed to terra::writeCDF
#' @return invisibly the SpatRasterDataset created in the process of writing the NetCDF file
#' @author Pascal Sauer
#' #export
writeMagpieNC <- function(x, filename, overwrite = TRUE, ..., compression = 4) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("The package \"terra\" is required!")
  }
  return(invisible(terra::writeCDF(as.SpatRasterDataset(x), filename, overwrite = overwrite,
                                   ..., compression = compression)))
}
