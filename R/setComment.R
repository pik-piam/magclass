#' @describeIn getComment set comment
#' @export
setComment <- function(object,nm=NULL) {
  getComment(object) <- nm
  return(object)
}