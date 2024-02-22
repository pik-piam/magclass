#' as.SpatRaster
#'
#' Convert magclass object to a SpatRaster object. Requires the terra package.
#'
#'
#' @param x MAgPIE object
#' @param res spatial data resolution. If not provided it will be guessed.
#' @return A SpatRaster object
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getCoords}}
#' @examples
#'
#' if (requireNamespace("terra", quietly = TRUE)) {
#'    r <- terra::rast(ncols = 360, nrows = 180, nl = 4)
#'    r[85:89, 176:179] <- (1:20 %*% t(1:4))
#'    r[15:19, 76:79] <-   (10 + 1:20 %*% t(1:4))
#'    names(r) <- c("y2000..bla", "y2001..bla", "y2000..blub", "y2001..blub")
#'    m <- as.magpie(r)
#'    r2 <- as.SpatRaster(m)
#' }
#' @export


as.SpatRaster <- function(x, res = NULL) { # nolint: object_name_linter.
  if (!is.magpie(x)) stop("Input is not a magpie object")
  if (!requireNamespace("terra", quietly = TRUE)) stop("The package \"terra\" is required!")

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

  m <- wrap(as.array(x), list(1, 2:3), sep = "..")
  xyz <- cbind(xy, m)

  target <- terra::rast(ncols = 360 / res, nrows = 180 / res, nlyrs = nyears(x) * ndata(x))
  out <- terra::rast(xyz, crs = terra::crs(target))
  names(out) <- colnames(m)
  if (all(grepl("^y[0-9]+\\.\\.", names(out)))) {
    terra::time(out, tstep = "years") <- as.integer(substr(names(out), 2, 5))
  }
  return(out)
}
