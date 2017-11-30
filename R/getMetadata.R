#' getMetadata
#' 
#' This function allows users to set and retrieve metadata for magclass objects 
#' 
#' Metadata is an attribute of a magclass object, and it includes the default 
#' fields of "unit", "source", "creationDate", "user", "calcHistory" and "description", 
#' all contained in a list.
#' 
#' The "source" element should include all information about the source(s)
#' where the data was originally reported. Specifically, the authors,
#' publication date, article title, journal name and volume, page numbers, DOI, ISSN
#' 
#' @aliases getMetadata getMetadata<-
#' @param x MAgPIE object
#' @param type A vector containing the Metadata field.
#' @param value An object containing the Metadata entry.
#' @return getMetadata returns the metadata attached to a MAgPIE-object, NULL if
#' no metadata attribute is present. getMetadata<- returns the magpie object with the
#' modified metadata.
#' @author Stephen Bi
#' @seealso \code{\link{getComment}}, \code{\link{getRegions}}, \code{\link{getNames}},
#' \code{\link{getYears}}, \code{\link{getCPR}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}}, \code{"\linkS4class{magpie}"}
#' @examples
#' 
#'  a <- as.magpie(1)
#'  #returns NULL
#'  getMetadata(a)
#'  #set the unit field
#'  getMetadata(a, "unit")<- "GtCO2eq"
#'  getMetadata(a)
#'  
#'  #set all Metadata fields
#'  M <- list('unit'='kg', 'source'=list('author'='John Doe', 'date'='January 1, 2017', 
#'  'title'='example', 'journal'='BigJournal, Vol. 200, pp. 100-115'), 
#'  'creationDate'=sys.Date(), 'user'='you', calcHistory'='readSource', 
#'  'description'='nonsense')
#'  getMetadata(a) <- M
#'  getMetadata(a)
#' 
#' @export

getMetadata <- function(x, type=NULL) {
  M <- attr(x, "Metadata")
  if(is.null(type)) {
    return(M)
  } else if(length(type)>1){
    return(M[type])
  } else {
    return(M[[type]])
  }
}
 
#' @describeIn getMetadata set and modify Metadata
#' @export
"getMetadata<-" <- function(x, type=NULL, value) {
  M <- attr(x, "Metadata")
  if (!is.list(M))  M <- list()
  if (!is.null(type))  M[[type]] <- value
  if (is.null(type)){
    if (!is.list(value)){
      stop("Metadata must be a list object if no type is specified")
    }else
      M <- value
  }
  attr(x, "Metadata") <- M
  return(x)
}
