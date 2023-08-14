#' writeMagpieNC
#'
#' Write a magpie object to a NetCDF file using the "terra" package.
#'
#' \code{\link{write.magpie}} can also write a magpie object to a NetCDF file, but
#' it is using the packages ncdf4 and raster, while this function uses terra.
#'
#' @param x a magpie object
#' @param filename the name of the NetCDF file to be created
#' @param overwrite whether to overwrite an existing file
#' @param zname the name of the time dimension
#' @param ... additional arguments passed to terra::writeCDF
#' @param compression the compression level to be used (1 = no compression, 9 = maximum compression)
#' @return invisibly the SpatRasterDataset created in the process of writing the NetCDF file
#' @author Pascal Sauer
#' #export
writeMagpieNC <- function(x, filename, overwrite = TRUE, zname = "time", ..., compression = 4) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("The package \"terra\" is required!")
  }
  spatRasterDataset <- as.SpatRasterDataset(x)
  # terra::writeCDF does not set the "axis" attribute for the time dimension, which triggers a warning
  suppressSpecificWarnings({
    terra::writeCDF(spatRasterDataset, filename, overwrite = overwrite,
                    ..., compression = compression)
  }, paste0("GDAL Message 1: dimension #0 (", zname, ") is not a Time or Vertical dimension."), fixed = TRUE)

  # set the "axis" attribute to "T" for the time dimension to prevent further warnings when reading the file
  nc <- ncdf4::nc_open(filename, write = TRUE)
  ncdf4::ncatt_put(nc, zname, "axis", "T")
  ncdf4::nc_close(nc)

  return(invisible(spatRasterDataset))
}
