#' as.SpatVector
#'
#' Convert magclass object to a SpatVector object. Requires the terra package and
#' requires the magclass object to provide the geometry of the spatial entities
#' as "geometry" attribute in "WKT" format. (see object "m" in example).
#'
#' @param x MAgPIE object
#' @return A SpatVector object
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{as.SpatRaster}}
#' @examples
#'
#' if (requireNamespace("terra", quietly = TRUE)) {
#'    r <- terra::rast(ncols = 360, nrows = 180, nl = 4)
#'    r[85:89, 176:179] <- (1:20 %*% t(1:4))
#'    r[15:19, 76:79] <-   (10 + 1:20 %*% t(1:4))
#'    names(r) <- c("y2000..bla", "y2001..bla", "y2000..blub", "y2001..blub")
#'    v <- terra::as.polygons(r)
#'    m <- as.magpie(v)
#'    attr(m, "geometry")
#'    attr(m, "crs")
#'    v2 <- as.SpatVector(m)
#' }
#' @export

as.SpatVector <- function(x) { # nolint: object_name_linter
  if (!is.magpie(x)) stop("Input is not a magpie object")
  if (!requireNamespace("terra", quietly = TRUE)) stop("The package \"terra\" is required!")
  if (is.null(attr(x, "geometry"))) {
    stop("x must have an attribute geometry containing the polygon structure in WKT format!")
  }
  out <- terra::vect(attr(x, "geometry"), crs = attr(x, "crs"))
  spatDims <- getItems(x, dim = 1, split = TRUE, full = TRUE)
  names(spatDims) <- paste0(".", names(spatDims))
  data <- as.data.frame(magclass::wrap(as.array(x), list(1, 2:3), sep = ".."))
  terra::values(out) <- cbind(spatDims, data)
  return(out)
}
