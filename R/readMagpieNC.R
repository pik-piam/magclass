#' readMagpieNC
#'
#' Create a magpie object from a NetCDF file using the "terra" package.
#'
#' \code{\link{read.magpie}} can also create a magpie object from a NetCDF file, but
#' it is using the packages ncdf4 and raster, while this function uses terra.
#'
#' @param filename the name of the NetCDF file to be read
#' @return the newly created magpie object
#' @author Pascal Sauer
#' @export
readMagpieNC <- function(filename) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("The package \"terra\" is required!")
  }
  x <- terra::sds(filename)
  x <- do.call(rbind, lapply(names(x), function(name) {
    years <- terra::time(x[name])
    a <- as.data.frame(x[name], na.rm = TRUE, xy = TRUE) # creates a data frame in wide format
    # convert to long format
    a <- do.call(rbind, lapply(seq_along(years), function(j) {
      b <- a[, c(1:2, j + 2)] # always select x and y, plus a single-year column
      names(b)[3] <- "value"
      b["year"] <- years[j]
      return(b)
    }))
    a["data"] <- name
    return(a)
  }))
  x <- x[, c("x", "y", "year", "data", "value")]
  x$x <- sub("\\.", "p", x$x)
  x$y <- sub("\\.", "p", x$y)
  x <- tidy2magpie(x, spatial = c("x", "y"), temporal = "year")
  return(x)
}
