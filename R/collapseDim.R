#' Collapse dataset dimensions
#' 
#' This function will remove names in the data dimension which are the same for
#' each element (meaning that this data dimension contains exactly one element)
#' or, if forced, remove any other subdimension. It is a generalized version
#' of the function \code{\link{collapseNames}}
#' 
#' 
#' @param x MAgPIE object
#' @param dim Dimension for which the items should be returned. Either number or 
#' name of dimension or a vector of these. See \code{\link{dimCode}} for more details.
#' CAUTION: If specified this can lead to duplicate entries. 
#' If set to NULL all single entry subdimensions will be removed.
#' @param keepdim If you want to remove the name of particular dimensions except some, 
#' you can specify the dimension(s) to keep here. See \code{\link{dimCode}} for naming convention.
#' Note that \code{keepdim} will be ignored if \code{dim} has been specified.
#' @return The provided MAgPIE object with collapsed dimensions
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getItems}}
#' \code{"\linkS4class{magpie}"}
#' @examples
#' 
#'  x <- new.magpie(c("GLO.1","GLO.2"),2000,c("bla.a","bla.b"))  
#'  collapseDim(x)
#'  collapseDim(x,keepdim=1:2)
#'  collapseDim(x,dim=1.1)
#'  collapseDim(x,dim=3.2)
#' 
#' @export

collapseDim <- function(x, dim=NULL, keepdim=NULL) {
  
  if(is.null(x)) return(NULL)
  
  if (is.null(dim)) {
    tmp <- sapply(unlist(getItems(x, split=TRUE),recursive = FALSE),length)
    dim <- dimCode(names(tmp)[tmp==1],x)
    if(!is.null(keepdim)) {
      dim <- setdiff(dim, dimCode(keepdim,x))
      dim <- dim[!(floor(dim) %in% keepdim)]
    }
  } else {
    if(!is.null(keepdim)) warning("keepdim argument is ignored as dim has been specified!")
    dim <- dimCode(dim,x)
  }
  
  if(any(dim==0)) {
    warning("Some dimensions could not be found in the object and will be ignored!")
    dim <- dim[dim>0]
  }
  
  for(d in dim) getItems(x,dim=d) <- NULL
  return(updateMetadata(x))
}
