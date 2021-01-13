#' Has Coordinates
#' 
#' Checks, whether object contains coordinates.
#' 
#' 
#' @param x MAgPIE object
#' @param xlab label of x-dimension
#' @param ylab label of y-dimension
#' @return Boolean indicating whether coordinates were found or not
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getCoords}}
#' @examples
#' 
#' hasCoords(population_magpie)
#' 
#' if (requireNamespace("raster", quietly = TRUE)) {
#'    r <- raster::brick(ncols=360,nrows=180, nl=4)
#'    r[85:89,176:179] <- (1:20 %*% t(1:4))
#'    names(r) <- c("y2000..bla","y2001..bla","y2000..blub","y2001..blub")
#'    m <- as.magpie(r)
#'    hasCoords(m)
#' } 
#' 
#' @export
hasCoords <- function(x, xlab="x", ylab="y") {
  return(all(dimExists(c(xlab,ylab),x)))
}
