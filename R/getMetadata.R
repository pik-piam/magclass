#' getMetadata (!experimental!)
#' 
#' This function is currently experimental and non-functional by default! To activate it,
#' set options(magclass_metadata=TRUE), otherwise it will not return or modify any metadata!
#' 
#' The function allows users to set and retrieve metadata for magclass objects 
#' 
#' Metadata is an attribute of a magclass object, and it includes the default 
#' fields of "unit", "source", "date", "user", "calcHistory" and "description", 
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
#'  options(magclass_metadata=TRUE)
#'  a <- as.magpie(1)
#'  #returns NULL
#'  getMetadata(a)
#'  #set the unit field
#'  getMetadata(a, "unit") <- "GtCO2eq"
#'  getMetadata(a)
#'  
#'  #set all Metadata fields
#'  M <- list('unit'='kg', 'source'=list('author'='John Doe', 'date'='January 1, 2017', 
#'  'title'='example', 'journal'='BigJournal, Vol. 200, pp. 100-115'), 
#'  'date'=sys.Date(), 'user'='you', calcHistory'='readSource', 
#'  'description'='nonsense')
#'  getMetadata(a) <- M
#'  getMetadata(a)
#'  options(magclass_metadata=FALSE)
#' @export

getMetadata <- function(x, type=NULL) {
  if(!isTRUE(getOption("magclass_metadata"))) return(NULL)
  M <- attr(x, "Metadata")
  if(is.null(M$unit)) M$unit <- '1'
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
  if(!isTRUE(getOption("magclass_metadata"))) return(x)
  M <- attr(x, "Metadata")
  if (!is.list(M))  M <- list()
  if (is.null(type)){
    if (!is.list(value) & !is.null(value))  stop("Metadata must be provided as a list if no type is specified")
    else{
      if (length(value$unit)>1){
        warning(value$unit," is an invalid argument for unit")
        value$unit <- 1
      }
      if (!is.null(value$source)){
        if (is.list(value$source)){
          for (i in 1:(length(value$source)-1)){
            if (is.list(value$source[[i]])){
              if (!is.null(value$source[[i+1]]) & !is.list(value$source[[i+1]])){
                warning("Source [",i+1,"] is not a list! Please include at least author, title, date, and journal. Also DOI, ISSN, URL, etc")
                value$source[[i+1]] <- NULL
              }
            }else if (!is.null(value$source[[i]]) & is.list(value$source[[i+1]])){
              warning("Source [",i,"] is not a list! Please include at least author, title, date, and journal. Also DOI, ISSN, URL, etc")
              value$source[[i]] <- NULL
            }
          }
        }else{
          warning("Source must be a formatted as a list! Please include at least author, title, date, and journal. Also DOI, ISSN, URL, etc")
          value$source <- NULL
        }
      }
      if (!is.null(value$user)){
        if (!is.character(value$user) & length(value$user)!=1){
          warning(value$user," is an invalid argument for user! Please use getMetadata.R or updateMetadata.R to provide a user")
          value$user <- NULL
        }
      }
      if(!is.null(value$date)){
        if(!is.character(value$date) & length(value$date)!=1){
          warning(value$date," is an invalid argument for date! Please use getMetadata.R or updateMetadata.R to provide a date")
          value$date <- NULL
        }
      }
      if(!is.null(value$description)){
        if(!is.character(value$description)){
          warning(value$description," is an invalid argument for description!")
          value$description <- NULL
        }
      }
    }
    M <- value
  }else if (type=="unit"){
    if (length(value)<=1)  M[[type]] <- value
    else  warning(value," is an invalid argument for unit!")
  }else if (type=="source"){
    if (is.null(value) || is.list(value))  M[[type]] <- value
    else  warning("Source field must be a list! Please include at least author, title, date, and journal. DOI, ISSN, URL, etc are also encouraged")
  }else if (type == "calcHistory"){
    if (is.character(value)){
      if (is.list(M$calcHistory))  M$calcHistory[[length(M$calcHistory)]] <- append(M$calcHistory[[length(M$calcHistory)]],value)
      else if (is.null(M[[type]]))  M[[type]] <- value
      else  M[[type]] <- list(M[[type]],value)
    }else if (is.null(value))  M[[type]] <- value
    else  warning(value," is an invalid argument for calcHistory! Please use getMetadata.R to provide the most recent function executed on, ",x)
  }else if (type=="date"){
    if  ((is.character(value) & length(value)==1))  M[[type]] <- value
    else  warning(value," is an invalid argument for date! Please use getMetadata.R or updateMetadata.R to provide a date for ",x)
  }else if (type=="user"){
    if ((is.character(value) & length(value)==1))  M[[type]] <- value
    else  warning(value," is an invalid argument for user! Please use getMetadata.R or updateMetadata.R to provide a user for ",x)
  }else if (type=="description"){
    if(is.null(value) || is.character(value))  M[[type]] <- value
    else  warning(value," is an invalid argument for description! Please use getMetadata.R to provide a description for ",x)
  }else  warning(type," is not a valid metadata field!")
  attr(x, "Metadata") <- M
  return(x)
}
