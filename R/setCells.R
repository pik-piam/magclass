#' @describeIn getCells set cell names
#' @export
setCells <- function(object,nm="GLO") {
  getCells(object) <- nm
  return(object)
}