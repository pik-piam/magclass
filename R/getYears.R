#' Get years
#' 
#' Extracts years of a MAgPIE-object
#' 
#' setYears is a shortcut to use a MAgPIE object with manipulated year names.
#' setYears uses the variable names "object" and "nm" in order to be consistent
#' to the already existing function setNames.
#' 
#' @aliases getYears getYears<- setYears
#' @param x,object MAgPIE object
#' @param as.integer Switch to decide, if output should be the used year-name
#' (e.g. "y1995") or the year as integer value (e.g. 1995)
#' @param value,nm Years the data should be set to. Either supplied as a vector
#' of integers or a vector of characters in the predefined year format
#' ("y0000"). If only 1 year exist you can also set the name of the year to
#' NULL.
#' @return getYears returns years of the MAgPIE-object, whereas setYears
#' returns the MAgPIE object with the manipulated years.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getRegions}}, \code{\link{getNames}},
#' \code{\link{setNames}}, \code{\link{getCPR}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}}, \code{"\linkS4class{magpie}"}
#' @examples
#' 
#'  a <- as.magpie(1)
#'  getYears(a)
#'  setYears(a,1995)
#' 
#' @export
getYears <- function(x,as.integer=FALSE) {
  if(as.integer) {
    return(as.integer(sub("^y", "", dimnames(x)[[2]])))
  } else {
    return(dimnames(x)[[2]])
  }
}

#' @describeIn getYears rename years
#' @export
"getYears<-" <- function(x,value) {
  if(!is.null(value)) if(length(value)!=nyears(x)) stop("Wrong number of years supplied!")
  if(nyears(x)==0) return(x)
  if(is.null(value) & nyears(x)!=1) stop("Setting years to NULL is not possible as the number of years is not 1!")
  if(is.null(value)) {
    tmp <- list(NULL,NULL,NULL)
    if(!is.null(dimnames(x)[[1]])) tmp[[1]] <- dimnames(x)[[1]]
    if(!is.null(dimnames(x)[[3]])) tmp[[3]] <- dimnames(x)[[3]]
    names(tmp) <- names(dimnames(x))
    dimnames(x) <- tmp
  } else {
    if(all(is.numeric(value))) value <- gsub(" ","0",format(value,width=4))
    if(all(nchar(value)==4)) value <- paste("y",value,sep="")
    if(any(nchar(value)!=5) | any(substr(value,1,1)!="y")) stop("Wrong year format. Please supply either integer values or years in the format y0000!")
    dimnames(x)[[2]] <- value
  }
  return(x)
}
