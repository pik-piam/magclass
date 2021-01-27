#' getDim
#' 
#' Function which tries to detect the dimension to which the given elems belong
#' 
#' 
#' @param elems A vector of characters containing the elements that should be
#' found in the MAgPIE object
#' @param x MAgPIE object in which elems should be searched for.
#' @param fullmatch If enabled, only dimensions which match exactly the elements
#' provided will be returned. Otherwise, it is sufficient if elems contains a subset
#' of the dimension.
#' @param dimCode If enabled, the dimCode will be returned, otherwise the name
#' of the dimension.
#' @return The name or dimCode of the dimensions in which elems were found.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{mcalc}},\code{\link{dimCode}}
#' @examples
#' 
#'  pop <- maxample("pop")
#'  getDim(c("AFR","CPA"),pop)
#'  getDim(c("AFR","CPA"),pop,fullmatch=TRUE)
#'  getDim(c("AFR","CPA"),pop,dimCode=FALSE)
#'  
#' @export
getDim <- function(elems,x,fullmatch=FALSE,dimCode=TRUE) {

  tmpfun <- function(x,elems) {
    return(all(elems %in% x)) 
  }
  tmpfun2 <- function(x,elems,fullmatch,dimCode) {
    if(fullmatch) {
      tmp <- sapply(x,setequal,elems)
    } else {
      tmp <- sapply(x,tmpfun,elems)
    }
    if(dimCode) names(tmp) <- 1:length(tmp)
    return(tmp)
  }
  tmp <- getItems(x,split=TRUE) 
  tmp2 <- lapply(tmp,tmpfun2,elems,fullmatch,dimCode)
  if(dimCode) {
    names(tmp2) <- 1:length(tmp2)
    tmp2 <- unlist(tmp2)
    out <- as.numeric(names(tmp2)[tmp2])
  } else {
    tmp2 <- unlist(tmp2)
    out <- names(tmp2)[tmp2]
  }
  if(length(out)==0) {
    tmp <- getItems(x,split=FALSE)
    tmp2 <- tmpfun2(tmp,elems,fullmatch,dimCode)
    out <- names(tmp2)[tmp2]
    if(dimCode) out <- as.numeric(out)
  }
  return(out)
}
