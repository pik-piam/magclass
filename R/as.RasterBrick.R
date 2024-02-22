#' as.RasterBrick
#'
#' Convert magclass object to a RasterBrick object
#'
#'
#' @param x MAgPIE object
#' @param res spatial data resolution. If not provided it will be guessed.
#' @return A RasterBrick object
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getCoords}}
#' @examples
#' \dontrun{
#' if (requireNamespace("raster", quietly = TRUE)) {
#'    r <- raster::brick(ncols = 36, nrows = 18, nl = 4)
#'    r[14:18, 25:28] <- (1:20 %*% t(1:4))
#'    names(r) <- c("y2000..bla", "y2001..bla", "y2000..blub", "y2001..blub")
#'    m <- as.magpie(r)
#'    r2 <- as.RasterBrick(m)
#' }
#' }
#' @export


as.RasterBrick <- function(x, res = NULL) { # nolint
  if (!is.magpie(x)) stop("Input is not a magpie object")
  if (!requireNamespace("raster", quietly = TRUE)) stop("The package \"raster\" is required!")

  if (!hasCoords(x) && dimExists(1.2, x)) {
    items <- getItems(x, dim = 1.2)
    if (length(items) == 59199 && all(items == seq_len(59199))) {
      # special treatment for data with 59199 cells as this
      # is the originally used 0.5deg resolution in magclass
      # before it had been generalized
      getCoords(x) <- magclassdata$half_deg[c("lon", "lat")]
    }
  }
  xy <- getCoords(x)
  if (is.null(res)) res <- guessResolution(xy)
  out <- raster::brick(ncols = 360 / res, nrows = 180 / res, nl = nyears(x) * ndata(x))
  m   <- wrap(as.array(x), list(1, 2:3), sep = "..")
  layerNames <- colnames(m)
  colnames(m) <- NULL
  raster::values(out)[raster::cellFromXY(out, xy), ] <- m
  names(out) <- layerNames
  return(out)
}
