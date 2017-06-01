#' Get a list of celluare region-belongings
#' 
#' Extracts a vector containing the region of each cell of a MAgPIE-object
#' 
#' 
#' @aliases getRegionList getRegionList<-
#' @param x MAgPIE object
#' @param value A vector with ncell elements containing the regions of each
#' cell.
#' @return A vector with ncell elements containing the region of each cell.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getRegions}},\code{\link{getYears}},
#' \code{\link{getNames}}, \code{\link{getCPR}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}}, \code{"\linkS4class{magpie}"}
#' @examples
#' 
#' # a <- read.magpie("example.mz")
#' # getRegionList(a)
#' 
#' @export
getRegionList <- function(x) {
  return(factor(sub("\\..*$","",dimnames(x)[[1]])))
}

#' @describeIn getRegionList set region names
#' @export
"getRegionList<-" <- function(x,value) {
  reg <- getRegionList(x)
  if(length(reg)!=length(value)) stop("Lengths of RegionLists do not agree!")
  tmp <- sub("^.*\\.","",dimnames(x)[[1]])
  dimnames(x)[[1]] <- paste(as.vector(value),tmp,sep=".")
  return(x)
}
