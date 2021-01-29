#' dimExists
#' 
#' Function checks whether a dimension exsist in a
#' MAgPIE objects
#' 
#' 
#' @param dim A vector of dimension numbers or dimension names which should be
#' checked for
#' @param x MAgPIE object in which the dimensions should be searched for.
#' @param sep  A character separating joined dimension names
#' @return Boolean indicating whether dimension exists or not
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{dimCode}}
#' @examples
#' 
#' pop <- maxample("pop")
#' dimExists(c("t","scenario","blablub"),pop)
#' 
#' @export
dimExists <- function(dim, x, sep="."){
  if(is.null(dim)) return(FALSE)
  if(length(dim)>1) return(sapply(dim,dimExists,x,sep=sep))
  
  d <- dimCode(dim,x,sep=sep)
  if(d==0) return(FALSE)
  if(d %in% c(1:3,(1:3)+0.1)) return(TRUE)
  
  maindim <- as.integer(d)
  subdim <- as.integer(substring(d,3))
  
  maxsubdim <- nchar(gsub(paste0("[^\\",sep,"]*"),"",dimnames(x)[[maindim]][1]))+1
  return(subdim<=maxsubdim)
}