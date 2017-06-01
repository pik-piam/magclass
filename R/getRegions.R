#' Get regions
#' 
#' Extracts regions of a MAgPIE-object
#' 
#' 
#' @aliases getRegions getRegions<-
#' @param x MAgPIE object
#' @param value Vector containing the new region names of the MAgPIE objects.
#' If you also want to change the mapping of regions to cell please use
#' \code{\link{getRegionList}} instead.
#' @return Regions of the MAgPIE-object
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getYears}}, \code{\link{getNames}},
#' \code{\link{getCPR}}, \code{\link{read.magpie}}, \code{\link{write.magpie}},
#' \code{"\linkS4class{magpie}"}
#' @examples
#' 
#' # a <- read.magpie("example.mz")
#' # getRegions(a)
#' 
#' @export
getRegions <- function(x) {
  if(sum(substr(dimnames(x)[[1]],4,4)!=".")>0) { #not all regions have 3-character names (need to use slow method)
    output <- unique(as.vector(as.matrix(cbind.data.frame(strsplit(dimnames(x)[[1]],'\\.'))[1,])))
  } else {  #region names all have 3 characters -> fast method
    output <- unique(substr(dimnames(x)[[1]],1,3))
  }
  return(output)
}

#' @describeIn getRegions overwrite region names
#' @export
"getRegions<-" <- function(x,value) {
  reg <- getRegions(x)
  if(!grepl(".",reg[1],fixed=TRUE)) {
    getCells(x) <- value
    return(x)
  }
  if(length(reg)!=length(value)) stop("Number of regions must agree with current number of regions!")
  tmp <- paste("SAVEREPLACE",dimnames(x)[[1]])
  for(i in 1:nregions(x)) {
    tmp <- sub(paste("SAVEREPLACE ",reg[i],"\\.",sep=""),paste(value[i],"\\.",sep=""),tmp)
  }
  dimnames(x)[[1]] <- tmp
  return(x)
}
