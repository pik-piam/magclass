#' Collapse dataset names
#' 
#' This function has been superseded by \code{\link{collapseDim}} which is a 
#' more generalized version of this function. Please use this one instead!
#' 
#' This function will remove names in the data dimension which are the same for
#' each element (meaning that this data dimension contains exactly one element)
#' 
#' 
#' @param x MAgPIE object
#' @param collapsedim If you want to remove the names of particular dimensions
#' provide the dimensions here. Since the function only works in the third dimension, 
#' you have to count from there on (e.g. dim = 3.2 refers to collapsedim = 2). 
#' Alternatively, you can also specify the name of the dimension. Default: NULL.
#' CAUTION with parameter collapsedim! You could also force him to remove dimnames, 
#' which are NOT the same for each element and so create duplicates in dimnames.
#' @param preservedim If you want to remove the name of particular dimensions except some, 
#' you can specify the dimension(s) to preserve here. See collapsedim for naming convention.
#' Note that preservedim will be ignored in the case, of a specified collapsedim
#' @return The provided MAgPIE object with collapsed names
#' @author Jan Philipp Dietrich, David Klein, Xiaoxi Wang
#' @seealso \code{\link{collapseDim}}, \code{\link{getItems}},
#' \code{"\linkS4class{magpie}"}
#' @examples
#' 
#'  x <- new.magpie("GLO",2000,c("bla.a","bla.b"))  
#'  print(x)
#'  # An object of class "magpie"
#'  # , , bla.a
#'  #      y2000
#'  # GLO.1    NA
#'  # , , bla.b
#'  #      y2000
#'  # GLO.1    NA
#' 
#'  print(collapseNames(x))
#'  # An object of class "magpie"
#'  # , , a
#'  #      y2000
#'  # GLO.1    NA
#'  # , , b
#'  #      y2000
#'  # GLO.1    NA
#' 
#'  print(collapseNames(x), collapseNames = 2)
#'  # An object of class "magpie"
#'  # , , bla
#'  #      y2000
#'  # GLO.1    NA
#'  # , , bla
#'  #      y2000
#'  # GLO.1    NA
#' 
#' @export collapseNames

collapseNames <- function(x, collapsedim=NULL, preservedim=NULL) {
  
  if(is.null(x)) return(NULL)
  if(is.null(getNames(x))) return(x)
  f <- fulldim(x)
  
  if(length(collapsedim)&length(preservedim)) warning("You can not preserve and collapse dims at the same time.
                                                      Preservedim argument will be ignored.")
  
  
  if (is.null(collapsedim)) {
    collapsedim <- which(f[[1]][-1:-2]==1)
    if(!is.null(preservedim)) collapsedim <- setdiff(collapsedim, preservedim)
  } else if(is.character(collapsedim)) {
    tmp <- match(collapsedim,names(f[[2]]))-2
    if(any(is.na(tmp))) {
      warning("Unknown collapsedim(s) specified. Unknown collapsedim(s) will be ignored (\"",paste(collapsedim[is.na(tmp)],collapse="\", \""),"\")!")
      tmp <- tmp[!is.na(tmp)]
    }
    collapsedim <- tmp
  }
  
  
  maxdim <- length(f[[1]])-2
  tmp <- getNames(x)
  tmp2 <- names(dimnames(x))[3]
  for(i in collapsedim) {
    searchstring <- paste("^(",paste(rep(".*\\.",i-1),collapse=""),")[^\\.]*(",paste(rep("\\..*",maxdim-i),collapse=""),")$",sep="")
    tmp <- sub(searchstring,"\\1\\2",tmp)
    tmp2 <- sub(searchstring,"\\1\\2",tmp2)
  }
  tmp <- gsub("\\.+","\\.",tmp)
  tmp <- sub("^\\.","",tmp)
  tmp <- sub("\\.$","",tmp)
  tmp2 <- gsub("\\.+","\\.",tmp2)
  tmp2 <- sub("^\\.","",tmp2)
  tmp2 <- sub("\\.$","",tmp2)
  if(length(tmp)==1) if(tmp=="") tmp <- NULL
  if(length(tmp2)==0) tmp2 <- "data"
  getNames(x) <- tmp
  names(dimnames(x))[3] <- tmp2
  x <- clean_magpie(x,what="sets")
  return(updateMetadata(x))
}
