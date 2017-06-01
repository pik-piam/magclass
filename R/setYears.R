#' @describeIn getYears set years
#' @export
setYears <- function(object,nm=NULL) {
  getYears(object) <- nm
  return(object)
}