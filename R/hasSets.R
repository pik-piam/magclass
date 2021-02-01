#' Has Sets
#' 
#' Checks, whether set names have been set
#' 
#' 
#' @param x MAgPIE object
#' @return Boolean indicating whether coordinates were found or not
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getCoords}}
#' @examples
#' 
#' hasSets(maxample("pop"))
#' hasSets(maxample("animal"))
#' 
#' 
#' @export
hasSets <- function(x) {
  return(!is.null(names(dimnames(x))))
}
