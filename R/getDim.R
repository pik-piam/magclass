#' getDim
#' 
#' Function which tries to detect the dimension to which the given elems belong
#' 
#' 
#' @param elems A vector of characters containing the elements that should be
#' found in the MAgPIE object
#' @param x MAgPIE object in which elems should be searched for.
#' @return The name of the dimension in which elems were found.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{mcalc}},\code{\link{dimCode}}
#' @examples
#' 
#'  data(population_magpie)
#'  magclass:::getDim(c("AFR","CPA"),population_magpie)
#' 
getDim <- function(elems, x){
  r <- sapply(elems,grepl,fulldim(x)[[2]],fixed=TRUE)
  if(any(colSums(r)==0)) stop("An element was not found in the given data set (",paste(colnames(r)[colSums(r)==0],collapse=", "),")!")
  if(any(colSums(r)>1)) stop("An element was found in more than one dimension in the given data set (",paste(colnames(r)[colSums(r)>1],collapse=", "),"). Please specify the dim to use!")
  if(!any(rowSums(r)==length(elems))) stop("Used elements belong to different dimensions!")
  dim <- which(rowSums(r)==length(elems))
  return(names(fulldim(x)[[2]])[dim])
}
