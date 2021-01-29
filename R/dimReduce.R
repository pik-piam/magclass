#' dimReduce
#' 
#' Remove dimensions which contain identical data for all elements in it
#' 
#' 
#' @param x MAgPIE object which should be reduced
#' @param dim_exclude Vector with names of dimensions which must not be reduced
#' @return The reduced MAgPIE object
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{add_dimension}}
#' @examples
#' 
#'  #create data with 5 identical scenarios
#'  p <- add_dimension(maxample("pop"),nm = paste0("scen",1:5))
#'  p
#'  dimReduce(p)
#'  
#'  #set years to same value
#'  p[,,] <- setYears(p[,1,], NULL)
#'  p
#'  dimReduce(p)
#'  
#'  #set regions to same value
#'  p[,,] <- setCells(p[1,,], "GLO")
#'  p
#'  dimReduce(p)
#' 
#' @export

dimReduce <- function(x, dim_exclude=NULL) {
  x <- collapseNames(x)
  x <- clean_magpie(x)
  fd <- fulldim(x)[[2]]
  for(s in setdiff(getSets(x),dim_exclude)) {
    tmp <- list()
    tmp[[s]] <- fd[[s]][1]
    x_single <- mselect(x,tmp, collapseNames = TRUE)
    if(dim(x_single)[1]==1) getCells(x_single) <- "GLO"
    if(dim(x_single)[2]==1) getYears(x_single) <- NULL
    # same information in all dimension entries?
    if(all(x - x_single  == 0, na.rm = TRUE)) {
      getMetadata(x_single) <- getMetadata(x)
      x <- x_single
    }      
  }
  return(updateMetadata(x))
}