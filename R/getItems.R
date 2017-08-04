#' Get Items
#' 
#' Extract items of a given (sub-)dimension of a MAgPIE-object
#' 
#' 
#' @param x MAgPIE object
#' @param dim Dimension for which the items should be returned. Either number or 
#' name of dimension. See \code{\link{dimCode}} for more details.
#' @return items of the requested dimension in the MAgPIE-object
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{dimCode}}
#' @examples
#' getItems(population_magpie,"scenario")
#' getItems(population_magpie,3.1)
#' @export
getItems <- function(x,dim) {
  dim <- dimCode(dim,x, missing = "stop")
  if(dim==round(dim)) return(dimnames(x)[[dim]])
  tmp <- dimnames(x)[[as.integer(dim)]]
  dim <- as.integer(strsplit(as.character(dimCode(dim)),split="\\.")[[1]][2])
  reg <- paste0(rep("([^\\.]*)",dim),collapse="\\.")
  return(unique(sub(paste0("^",reg,".*$"),paste0("\\",dim),tmp)))
}