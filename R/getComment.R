#' getComment
#' 
#' Extracts the comment from a MAgPIE-object
#' 
#' 
#' @aliases getComment getComment<- setComment
#' @param x,object MAgPIE object
#' @param value,nm A vector containing the comment.
#' @return getComment returns the comment attached to a MAgPIE-object, NULL if
#' no comment is present. setComment returns the magpie object with the
#' modified comment.
#' @author Markus Bonsch
#' @seealso \code{\link{getRegions}}, \code{\link{getNames}},
#' \code{\link{getYears}}, \code{\link{getCPR}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}}, \code{"\linkS4class{magpie}"}
#' @examples
#' 
#'  a <- as.magpie(1)
#'  #returns NULL
#'  getComment(a)
#'  #set the comment
#'  getComment(a)<-c("bla","blubb")
#'  getComment(a)
#' 
#' @export
getComment <- function(x) {
  return(attr(x,"comment"))
}

#' @describeIn getComment set comment
#' @export
"getComment<-" <- function(x,value) {
  attr(x,"comment")<-value
  return(x)
}
