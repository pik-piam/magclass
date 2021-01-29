#' head/tail
#' 
#' head and tail methods for MAgPIE objects to extract the head or tail of an
#' object
#' 
#' 
#' @aliases head.magpie tail.magpie
#' @param x MAgPIE object
#' @param n1,n2,n3 number of lines in first, second and third dimension that
#' should be returned. If the given number is higher than the length of the
#' dimension all entries in this dimension will be returned.
#' @param ... arguments to be passed to or from other methods.
#' @return head returns the first n1 x n2 x n3 entries, tail returns the last
#' n1 x n2 x n3 entries.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link[utils]{head}}, \code{\link[utils]{tail}}
#' @examples
#' 
#'  pop <- maxample("pop")
#'  head(pop)
#'  tail(pop,2,4,1)
#' 
#' @importFrom utils head
#' @export 
head.magpie <- function(x, n1=3L, n2=6L, n3=2L, ...) {
  if(dim(x)[1]<n1) n1 <- dim(x)[1]
  if(dim(x)[2]<n2) n2 <- dim(x)[2]
  if(dim(x)[3]<n3) n3 <- dim(x)[3]
  return(x[1:n1,1:n2,1:n3])  
}
