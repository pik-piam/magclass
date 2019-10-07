#' Get Cells
#' 
#' Extracts cell names of a MAgPIE-object
#' 
#' setCells is a shortcut to use a MAgPIE object with manipulated cell names.
#' setCells uses the variable names "object" and "nm" in order to be consistent
#' to the already existing function setNames.
#' 
#' @aliases getCells getCells<- setCells
#' @param x,object MAgPIE object
#' @param value,nm cell names the data should be set to.
#' @return getCells returns cell names of the MAgPIE-object, whereas setCells
#' returns the MAgPIE object with the manipulated cell names.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getRegions}}, \code{\link{getNames}},
#' \code{\link{setNames}}, \code{\link{getCPR}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}}, \code{"\linkS4class{magpie}"}
#' @examples
#' 
#'  a <- as.magpie(1)
#'  getCells(a)
#'  setCells(a,"AFR")
#' 
#' @export
getCells <- function(x) {
  return(dimnames(x)[[1]])
}

#' @describeIn getCells set cell names
#' @export
"getCells<-" <- function(x,value) {
  if(length(value)!=ncells(x)) stop("Wrong number of cell names supplied!")
  if(ncells(x)==0) return(x)
  if(is.null(value)) stop("Setting cell names to NULL is not allowed!")
  if(length(value)==1) value <- list(value)
  dimnames(x)[[1]] <- value
  return(x)
}
