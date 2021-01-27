#' Set Items
#' 
#' Set items of a given (sub-)dimension of a MAgPIE-object
#' 
#' 
#' @param x MAgPIE object
#' @param dim Dimension for which the items should be returned. Either number or 
#' name of dimension or a vector of these. See \code{\link{dimCode}} for more details.
#' @param maindim main dimension the data should be added to (does not need to be set if \code{dim} exists
#' in the data. Should be set if \code{dim} might not exist, or if \code{dim} might potentially exist
#' in a different main dimension than the one anticipated).
#' @param value a vector with the length of the main dimension the dimnames should be replaced in / added to. 
#' If set to NULL the corresponding dimension will be removed.
#' @return the manipulated MAgPIE object
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getItems}}
#' @examples
#' x <- maxample("pop")
#' setItems(x, "i", paste0("REG",1:ncells(x)))
#' 
#' @export
setItems <- function(x,dim,value,maindim=NULL) {
  getItems(x, dim=dim, maindim=maindim) <- value
  return(x)
}
