#' Collapse dataset dimensions
#' 
#' This function will remove names in the data dimension which are the same for
#' each element (meaning that this data dimension contains exactly one element)
#' or, if forced, remove any other subdimension. It is a generalized version
#' of the function \code{\link{collapseNames}}
#' 
#' 
#' @param x MAgPIE object
#' @param dim Either NULL, dimension code or 
#' name of dimension or a vector of these. If set to NULL all single entry subdimensions will 
#' be removed as they are irrelevant to uniquely identfy a data element. If specified, only the specified
#' subdimensions will be removed (See \code{\link{dimCode}} for more details how to specify a subdimension).
#' CAUTION: The function also allows to specify subdimensions which are otherwise needed to clearly identify 
#' an entry. By removing these subdimensions duplicates in the data will be created potentially causing
#' problems in the further use of the data set. Be careful in removing subdimensions. 
#' @param keepdim (only considered if \code{dim} is not specified) Can be used to converse
#' single element subdimension which otherwise would get deleted. If \code{dim} is specified
#' this setting will not have any effect.
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
  
  if (is.null(x)) return(NULL)
  
  if (is.null(dim)) {
    tmp <- sapply(unlist(getItems(x, split = TRUE),recursive = FALSE),length)
    dim <- dimCode(names(tmp)[tmp == 1],x)
    if (!is.null(keepdim)) {
      dim <- setdiff(dim, dimCode(keepdim,x))
      dim <- dim[!(floor(dim) %in% keepdim)]
    }
  } else {
    if (!is.null(keepdim)) warning("keepdim argument is ignored as dim has been specified!")
    dim <- dimCode(dim,x)
  }
  
  if (any(dim == 0)) {
    warning("Some dimensions could not be found in the object and will be ignored!")
    dim <- dim[dim > 0]
  }
  
  for (d in sort(dim, decreasing = TRUE)) getItems(x, dim = d) <- NULL
  return(updateMetadata(x))
}
