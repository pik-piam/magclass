#' Get Items
#' 
#' Extract items of a given (sub-)dimension of a MAgPIE-object
#' 
#' 
#' @param x MAgPIE object
#' @param dim Dimension for which the items should be returned. Either number or 
#' name of dimension. See \code{\link{dimCode}} for more details.
#' @param split Boolean which determines whether a main dimension should be split in subdimensions.
#' Only applicable to main dimensions (1,2,3) and ignored for all other.
#' @return items of the requested dimension in the MAgPIE-object. If split=TRUE and applied to a 
#' main dimension (1,2,3) a list of items for each sub-dimension.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{dimCode}}
#' @examples
#' getItems(population_magpie,"scenario")
#' getItems(population_magpie,3.1)
#' @export
getItems <- function(x,dim,split=FALSE) {
  dim <- dimCode(dim,x, missing = "stop")
  if(dim==round(dim) && !split) return(dimnames(x)[[dim]])
  if(dim==round(dim) && split) {
    tmp <- as.list(as.data.frame(t(matrix(unlist(strsplit(dimnames(x)[[dim]],"\\.")),ncol=dim(x)[dim])),stringsAsFactors=FALSE))
    tmp <- lapply(tmp,unique)
    if(!is.null(getSets(x))) {
      tmp2 <- strsplit(getSets(x,fulldim=FALSE)[dim],"\\.")[[1]]
      if(length(tmp2)==length(tmp)) names(tmp) <- tmp2
    }
    return(tmp)
  }
  tmp <- dimnames(x)[[as.integer(dim)]]
  subdim <- as.integer(strsplit(as.character(dimCode(dim)),split="\\.")[[1]][2])
  maxsubdim <- nchar(gsub("[^\\.]*","",tmp[1]))+1
  if(length(maxsubdim)==0) maxsubdim <- 1
  if(subdim>maxsubdim) stop("Subdimension ",dimCode(dim), " does not exist (maxsubdim = ",maxsubdim,")!")
  if(is.null(tmp)) return(NULL)
  reg <- paste0(rep("([^\\.]*)",subdim),collapse="\\.")
  return(unique(sub(paste0("^",reg,".*$"),paste0("\\",subdim),tmp)))
}