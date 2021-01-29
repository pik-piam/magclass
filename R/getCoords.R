#' Get Coordinates
#' 
#' Extracts spatial coordinates of a MAgPIE-object
#' 
#' 
#' @aliases getCoords getCoords<- 
#' @param x MAgPIE object
#' @param xlab label of x-dimension
#' @param ylab label of y-dimension
#' @param value coordinates as two column data.frame the data should be set to 
#' (first column = x, second column = y).
#' @return coordinates of the MAgPIE-object
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{as.RasterBrick}}, \code{\link{getItems}}, \code{"\linkS4class{magpie}"}
#' @examples
#'   a <- maxample("animal")#'   
#'   getCoords(a)
#' 
#' @export
getCoords <- function(x, xlab="x", ylab="y") {
  if(!hasCoords(x,xlab,ylab)) stop("No coordinates found in object!")
  .tmp <- function(x,n) return(as.numeric(sub("p",".", getItems(x,n,full=TRUE), fixed=TRUE)))
  return(data.frame(x=.tmp(x,xlab),y=.tmp(x,ylab)))
}

#' @describeIn getCoords set coordinates
#' @export
"getCoords<-" <- function(x,xlab="x",ylab="y",value) {
  if(is.null(ncol(value)) || ncol(value)!=2) stop("Coordinates supplied in unsupported format (must be a two column data.frame)") 
  if(!is.data.frame(value)) x <- as.data.frame(value)
  if(all(c("x","y") %in% names(value))) value <- value[c("x","y")]
  if(ncells(x)!=nrow(value)) stop("Number of rows does not match number of cells of the MAgPIE object!")
  getItems(x, xlab, maindim=1) <- value[[1]]
  getItems(x,ylab, maindim=1)  <- value[[2]]
  return(x)
}
