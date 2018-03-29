#' getMetadata (!experimental!)
#' 
#' This function is currently experimental and non-functional by default! To activate it,
#' set withMetadata(TRUE), otherwise it will not return or modify any metadata!
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
#'  withMetadata(TRUE)
#'  a <- as.magpie(1)
#'  #returns NULL
#'  getMetadata(a)
#'  #set the unit field
#'  getMetadata(a, "unit") <- "GtCO2eq"
#'  getMetadata(a)
#'  
#'  #set all Metadata fields
#'  M <- list(unit='kg', source=list(author='John Doe', date='January 1, 2017', 
#'  title='example', publication='BigJournal, Vol. 200, pp. 100-115', institution='IEA'), 
#'  date=as.character(Sys.time()), user='my name', calcHistory=list('downloadSource','readSource'), 
#'  description='nonsense data')
#'  getMetadata(a) <- M
#'  getMetadata(a)
#'  withMetadata(FALSE)
#' @export

getMetadata <- function(x, type=NULL) {
  if(!withMetadata()) return(NULL)
  if (!requireNamespace("data.tree", quietly = TRUE)) stop("The package data.tree is required for metadata handling!")
  M <- attr(x, "Metadata")
  if(!is.null(M) & is.null(M$unit)) M$unit <- '1'
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
  if(!withMetadata()) return(x)
  if (!requireNamespace("data.tree", quietly = TRUE)) stop("The package data.tree is required for metadata handling!")
  M <- attr(x, "Metadata")
  if (!is.list(M))  M <- list()
  if (is.null(type)){
    if (!is.list(value) & !is.null(value))  stop("Metadata must be provided as a list if no type is specified")
    else{
      if (!is.null(value$unit)){
        if (length(value$unit)>1){
          warning(value$unit," is an invalid argument for unit")
          M$unit <- "1"
        }else  M$unit <- value$unit
      }else  M$unit <- "1"
      if (!is.null(value$source)){
        if (!is(value$source,"bibentry")){
          if (is.list(value$source)){
            j <- 0
            k <- 0
            for (i in 1:(length(value$source))){
              if (is(value$source[[i]],"bibentry")){
                j <- j+1
                M$source[[j]] <- value$source[[i]]
              }else{
                if (k==0)  k <- i
                else  k <- append(k,i)
              }
            }
            if (k!=0)  warning("Source(s) ",toString(k)," are not bibentry objects!")
          }else{
            warning("Source must be an object of class bibentry or a list of bibentry objects!")
            M$source <- NULL
          }
        }else  M$source <- value$source
      }
      if (!is.null(value$calcHistory)){
        if (is(value$calcHistory,"Node")){
          if (is(M$calcHistory,"Node")){
            if (value$calcHistory$count==0){
              if (M$calcHistory$name=="ROOT")  M$calcHistory$name <- value$calcHistory$name
              else{
                cV <- data.tree::Clone(value$calcHistory)
                cM <- data.tree::Clone(M$calcHistory)
                cV$AddChildNode(cM)
                M$calcHistory <- cV
              }
            }else  M$calcHistory <- value$calcHistory
          }else  M$calcHistory <- value$calcHistory
        }else if (is.character(value$calcHistory) & length(value$calcHistory)==1){
          if (is(M$calcHistory,"Node")){
            if (M$calcHistory$name=="ROOT")  M$calcHistory$name <- value$calcHistory
            else{
              cV <- data.tree::Node$new(value$calcHistory)
              cM <- data.tree::Clone(M$calcHistory)
              cV$AddChildNode(cM)
              M$calcHistory <- cV
            }
          }
        }else  warning(value$calcHistory," is an invalid argument for calcHistory! The argument must be a string or a Node object.")
      }
      if (!is.null(value$user)){
        if (!is.character(value$user) & length(value$user)!=1){
          warning(value$user," is an invalid argument for user! Please use getMetadata or updateMetadata to provide a user")
          M$user <- NULL
        }else  M$user <- value$user
      }
      if(!is.null(value$date)){
        if(!is.character(value$date) & length(value$date)!=1){
          warning(value$date," is an invalid argument for date! Please use getMetadata or updateMetadata to provide a date")
          M$date <- NULL
        }else  M$date <- value$date
      }
      if(!is.null(value$description)){
        if(is.character(value$description))  M$description <- value$description
        else if (is.list(value$description))  M$description <- value$description
        else{
          warning(value$description," is an invalid argument for description!")
          M$description <- NULL
        }
      }
      if(!is.null(value$note)){
        if(is.character(value$note))  M$note <- value$note
        else if (is.list(value$note))  M$note <- value$note
        else{
          warning(value$note," is an invalid argument for note!")
          M$note <- NULL
        }
      }
    }
  }else if (type=="unit"){
    if (is.character(value) & length(value)==1)  M[[type]] <- value
    else if (is.null(value))  M$unit <- '1'
    else  warning(value," is an invalid argument for unit!")
  }else if (type=="source"){
    if (is.null(value))  M[[type]] <- value
    if (!is(value,"bibentry")){
      if (is.list(value)){
        j <- 0
        k <- 0
        for (i in 1:(length(value))){
          if (is(value[[i]],"bibentry")){
            j <- j+1
            M$source[[j]] <- value[[i]]
          }else{
            if (k==0)  k <- i
            else  k <- append(k,i)
          }
        }
        if (k!=0)  warning("Source(s) ",toString(k)," are not bibentry objects!")
      }else  warning("Source must be an object of class bibentry or a list of bibentry objects!")
    }else  M$source <- value
  }else if (type == "calcHistory"){
    if (is(value,"Node")){
      if (is(M[[type]],"Node")){
        if (data.tree::isRoot(value))  M[[type]] <- value
        else if (is.null(value$children)){
          if (M[[type]]$name=="ROOT")  M[[type]]$name <- value$name
          c <- data.tree::Clone(M[[type]])
          value$AddChildNode(c)
          M[[type]] <- value
        }
      }else  M[[type]] <- value
    }else if (is.character(value)){
      if (length(value)==1){
        if (is(M[[type]],"Node")){
          if (M[[type]]$name=="ROOT")  M[[type]]$name <- value
          else{
            c <- data.tree::Clone(M[[type]])
            M[[type]] <- data.tree::Node$new(value)
            M[[type]]$AddChildNode(c)
          }
        }else  M[[type]] <- data.tree::Node$new(value)
      }else  warning(value,"is an invalid argument for calcHistory! The argument must be a character of length 1 or a Node object.")
    }else if (is.null(value))  M[[type]] <- value
    else  warning(value," is an invalid argument for calcHistory! The argument must be a string or a Node object.")
  }else if (type=="date"){
    if  ((is.character(value) & length(value)==1) | is.null(value))  M[[type]] <- value
    else  warning(value," is an invalid argument for date! Please use getMetadata or updateMetadata to enter a date.")
  }else if (type=="user"){
    if ((is.character(value) & length(value)==1) | is.null(value))  M[[type]] <- value
    else  warning(value," is an invalid argument for user! Please use getMetadata or updateMetadata to enter a user.")
  }else if (type=="description"){
    if(is.null(value) | is.character(value))  M[[type]] <- value
    else  warning(value," is an invalid argument for description! Please use getMetadata or updateMetadata to enter a description.")
  }else if (type=="note"){
    if(is.null(value) | is.character(value))  M[[type]] <- value
    else  warning(value," is an invalid argument for note! Please use getMetadata or updateMetadata to enter a note.")
  }else  warning(type," is not a valid metadata field!")
  
  attr(x, "Metadata") <- M
  return(x)
}
