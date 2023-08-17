#' as.SpatRasterDataset
#'
#' Convert magclass object to a SpatRasterDataset object. Requires the terra package.
#'
#' Calls \code{\link{as.SpatRaster}} and then \code{\link{spatRasterToDataset}}.
#'
#' @param ... arguments passed to as.SpatRaster
#' @return A SpatRasterDataset object
#' @author Pascal Sauer
#' @export
as.SpatRasterDataset <- function(...) { # nolint: object_name_linter.
  return(spatRasterToDataset(as.SpatRaster(...)))
}
