#' Unwrap
#' 
#' Reconstruct the full dimensionality of a MAgPIE object
#' 
#' 
#' @param x A MAgPIE object
#' @param sep A character separating joined dimension names
#' @return An array with the full dimensionality of the original data
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{wrap}},\code{\link{fulldim}}
#' @examples
#' 
#'   a <- as.magpie(array(1:6,c(3,2),list(c("bla","blub","ble"),c("up","down"))))
#'   fulldim(a)
#'   unwrap(a)
#' 
#' @export unwrap
unwrap <- function(x,sep=".") {
  if(!is.magpie(x)) stop("Input is not a MAgPIE object. unwrap works only for MAgPIE objects")
  dim <- fulldim(x,sep)
  if(anyDuplicated(as.data.table(getNames(x)))) stop("Malformed MAgPIE object. Duplicated names detected!")
  if(prod(dim[[1]])!=prod(dim(x))) stop("Malformed MAgPIE object. Different number of entries in original and unwrapped object! (prod(dim(in))!=prod(dim(out)))")
  reorder <- dimnames(wrap(array(NA,dim[[1]],dim[[2]]),list(1,2,NA),sep=sep))[[3]]
  if(!is.null(reorder)) x <- x[,,reorder]
  return(array(as.vector(x),dim[[1]],dim[[2]]))
}




