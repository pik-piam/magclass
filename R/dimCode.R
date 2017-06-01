#' dimCode
#' 
#' Function converts a dimension name or number to a dimension Code used for
#' MAgPIE objects
#' 
#' 
#' @param dim A vector of dimension numbers or dimension names which should be
#' translated
#' @param x MAgPIE object in which the dimensions should be searched for.
#' @param missing Either a value to which a dimension should be set in case
#' that it is not found (default is 0), or "stop" indicating that the function
#' should throw an error in these cases.
#' @return A dimension code identifying the dimension. Either a integer which
#' represents the main dimensions (1=spatial, 2=temporal, 3=data) or a numeric,
#' representing the subdimensions of a dimension (e.g. 3.2 for the second data
#' dimension).
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{mselect}}, \code{\link{getDim}}
#' @examples
#' 
#' data(population_magpie)
#' dimCode(c("t","scenario","blablub"),population_magpie)
#' 
#' @export dimCode
dimCode <- function(dim,x,missing=0){
  #function to translate dim to dim code
  if(is.character(dim)) {
    dnames <- dim
    dim <- match(dim,getSets(x),nomatch=0)
    if(length(dim)>length(dnames)) stop('One or more elements were found more than once in x!')
    names(dim) <- dnames
    
    #translate sub-datadimensions to 3.1, 3.2,...
    dim[dim>=3] <- 3 + (dim[dim>=3]-2)/10
  }
  if(any(dim>=4) | any(dim<1)) {
    if(missing=="stop") stop("illegal dimension. Use either dimension 1, 2, or 3, or if you want to address subdimensions use 3.1, 3.2, ...")
    dim[dim>=4 | dim<1] <- missing
  }
  return(dim)
}
