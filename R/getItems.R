#' Get Items
#' 
#' Extract items of a given (sub-)dimension of a MAgPIE-object
#' 
#' 
#' @param x MAgPIE object
#' @param dim Dimension for which the items should be returned. Either number or 
#' name of dimension or a vector of these. See \code{\link{dimCode}} for more details.
#' @param split Boolean which determines whether a main dimension should be split in subdimensions.
#' Only applicable to main dimensions (1,2,3) and ignored for all other.
#' @param full if TRUE dimension names are returned as they are (including repetitions), if FALSE only
#' the dimension elements (unique list of entries) are returned.
#' @return items of the requested dimension in the MAgPIE-object. If split=TRUE and applied to a 
#' main dimension (1,2,3) a list of items for each sub-dimension.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{dimCode}}
#' @examples
#' getItems(population_magpie,"scenario")
#' getItems(population_magpie,3.1)
#' @export
getItems <- function(x,dim=NULL,split=FALSE,full=FALSE) {
  if(is.null(dim)) dim <- 1:3
  dim <- dimCode(dim,x, missing = "stop")
  if(setequal(dim,1:3) && !split) return(dimnames(x))
  if(length(dim)>1) {
    out <- list()
    for(i in dim) out[[as.character(i)]] <- getItems(x,dim=i,split=split,full=full)
    if(all(dim==round(dim))) {
      names(out) <- NULL
    } else if(!is.null(getSets(x))) {
      sets <- c(d1="",d2="",d3="",getSets(x))
      names(out) <- sets[paste0("d",dim)]
    }
    return(out)
  }

  if(dim==round(dim) && !split) return(dimnames(x)[[dim]])
  if(dim==round(dim) && split) {
    if(is.null(dimnames(x)[[dim]])) {
      out <- list(NULL)
      if(!is.null(getSets(x))) {
        names(out) <- getSets(x,fulldim=FALSE)[dim]
      }
      return(out)
    } 
    tmp <- as.list(as.data.frame(t(matrix(unlist(strsplit(dimnames(x)[[dim]],"\\.")),ncol=dim(x)[dim])),stringsAsFactors=FALSE))
    if(!isTRUE(full)) tmp <- lapply(tmp,unique)
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
  out <- sub(paste0("^",reg,".*$"),paste0("\\",subdim),tmp) 
  if(isTRUE(full)) return(out)
  return(unique(out))
}