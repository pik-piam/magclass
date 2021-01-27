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
#' hasCoords(maxample("pop"))
#' hasCoords(maxample("animal"))
#' 
#' 
#' @export
hasCoords <- function(x, xlab="x", ylab="y") {
  return(all(dimExists(c(xlab,ylab),x)))
}
