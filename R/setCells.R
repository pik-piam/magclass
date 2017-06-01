#' @describeIn getCells set cell names
#' @export
setCells <- function(object,nm="GLO.1") {
  getCells(object) <- nm
  return(object)
}