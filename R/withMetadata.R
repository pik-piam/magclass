#' withMetadata (!experimental!)
#' 
#' Convenience function to (de-)activate metadata handling in magpie objects and
#' to return current setting
#' 
#' @param set boolean to switch metadata on/off or NULL to leave the option as is.
#' @param verbosity Integer to set the verbosity level of calcHistory tracking. 0 = no calcHistory tracking, 
#' 1 = only the core functions are tracked (e.g. calcOutput, readSource), 2 (default) = most magclass functions 
#' and toolAggregate are also tracked, 3 = virtually all functions are tracked.
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

withMetadata <- function(set=NULL,verbosity=NULL) {
  if (is.null(verbosity)) {
    if (is.null(getOption("calcHistory_verbosity")))  verbosity <- 2
    else  verbosity <- getOption("calcHistory_verbosity")
  }
  if (verbosity>=0)  options(calcHistory_verbosity=verbosity)
  else {
    verbosity <- 2
    options(calcHistory_verbosity=verbosity)
    warning("verbosity must be a non-negative integer! Setting verbosity to default setting ",verbosity)
  }
  if(is.null(set)) {
    status <- getOption("magclass_metadata")
    if(is.null(status)) status <- FALSE
    return(status)
  } else if(is.logical(set) & length(set)==1) {
    options(magclass_metadata=set)
    return(set)
  } else {
    stop("set must be either NULL or boolean!")
  }
}