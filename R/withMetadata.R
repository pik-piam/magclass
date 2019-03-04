#' withMetadata (!experimental!)
#' 
#' Convenience function to (de-)activate metadata handling in magpie objects and
#' to return current setting
#' 
#' @param set boolean to switch metadata on/off or NULL to leave the option as is.
#' @param verbosity Integer to set the verbosity of the calcHistory tree. 2 = all relevant functions are tracked.
#' 1 = only the core functions are tracked (e.g. calcOutput, readSource).
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

withMetadata <- function(set=NULL,verbosity=getOption("metadata_verbosity")) {
  if (is.null(verbosity))  verbosity <- 2
  if (verbosity %in% c(1,2))  options(metadata_verbosity=verbosity)
  else  warning("Currently only verbosity levels 1 and 2 are supported! Verbosity level set to 2.")
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