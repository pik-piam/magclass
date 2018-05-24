#' withMetadata (!experimental!)
#' 
#' Convenience function to (de-)activate metadata handling in magpie objects and
#' to return current setting
#' 
#' @param set MAgPIE object
#' @return boolean indicating the current metadata setting (switched on or off)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getMetadata}}
#' @examples
#'  withMetadata()
#'  withMetadata(TRUE)
#'  a <- as.magpie(1)
#'  getMetadata(a)
#'  withMetadata(FALSE)
#' @export

withMetadata <- function(set=NULL) {
  if(is.null(set)) {
    status <- getOption("magclass_metadata")
    if(is.null(status)) status <- FALSE
    return(status)
  } else if(is.logical(set) & length(set)==1) {
    if(isTRUE(set))  tmp <- suppressWarnings(Sys.setlocale("LC_ALL","en_US.UTF-8"))
    options(magclass_metadata=set)
    return(set)
  } else {
    stop("set must be either NULL or boolean!")
  }
}