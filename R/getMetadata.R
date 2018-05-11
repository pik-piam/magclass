#' getMetadata (!experimental!)
#' 
#' This function is currently experimental and non-functional by default! To activate it,
#' set withMetadata(TRUE), otherwise it will not return or modify any metadata!
#' 
#' The function allows users to set and retrieve metadata for magclass objects 
#' 
#' Metadata is an attribute of a magclass object, and it includes the default 
#' fields of "unit", "source", "date", "user", "calcHistory", "description" 
#' and "note", all contained in a list.
#' 
#' The "source" element is stored as a Bibtex class object (or a list thereof), but the value argument 
#' here can be either a Bibtex or bibentry object (or a list of any combination). Include all relevant 
#' information regarding where the data was originally reported. Specifically, the type of publication,
#' author(s), article title, journal/publication name, volume, page numbers, URL and DOI.
#' 
#' The "calcHistory" field is stored as a Node class object. The value argument can be either a single
#' node, a character of length 1 (to be converted to a node), or a full data tree. In the first two cases, 
#' the provided value will become the root node (read as the most recent function applied to the object).
#' In the case of a full tree input, this will replace any existing calcHistory. Use updateMetadata() to 
#' merge the calcHistory of two magpie objects.
#' 
#' @aliases getMetadata getMetadata<-
#' @param x MAgPIE object
#' @param type A vector containing the Metadata field. If NULL, getMetadata() will return all 
#' non-NULL fields, and 'getMetadata<-' will update all fields specified in value.
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
#' @importFrom utils toBibtex
"getMetadata<-" <- function(x, type=NULL, value) {
  if(!withMetadata()) return(x)
  if(!is.magpie(x)){
    warning("x argument must be a magpie object!")
    return(x)
  }
  if (!requireNamespace("data.tree", quietly = TRUE)) stop("The package data.tree is required for metadata handling!")

  .setSource <- function(old,new) {
    #first remove any sources which are not of bibentry or Bibtex class
    if (is(new,"bibentry"))  new <- toBibtex(new)
    else if (is.list(new)) {
      k <- NULL
      for (i in 1:(length(new))) {
        if (is(new[[i]],"bibentry")) {
          new[[i]] <- toBibtex(new[[i]])
        }else if (!is(new[[i]],"Bibtex")) {
          k <- c(k,i)
        }
      }
      if (!is.null(k)){
        new[k] <- NULL
        warning("Source(s) ",toString(k)," are not Bibtex/bibentry objects!")
      }
    }else if (!is(new,"Bibtex")) {
      warning("Source must be an object of class Bibtex/bibentry or a list of Bibtex/bibentry objects!")
      return(old)
    }
    #merge new source(s) with existing source list (if any)
    if (is.null(old))  return(new)
    else if (is.list(old)) {
      if (is.list(new))  new <- append(old,new)
      else  new <- append(old,list(new))
    }else {
      if(is.list(new))  new <- append(list(old),new)
      else  new <- list(old,new)
    }
    #check if all sources are unique
    if (is.list(new)) {
      j <- NULL
      k <- 1
      for (i in 2:length(new)) {
        for (k in 1:(i-1)) {
          if (!is.na(new[[i]]["doi"]) & !is.na(new[[i-k]]["doi"])) {
            if (new[[i]]["doi"]==new[[i-k]]["doi"]) {
              j <- c(j,i)
            }
          }else if (!is.na(new[[i]]["url"]) & !is.na(new[[i-k]]["url"])) {
            if (new[[i]]["url"]==new[[i-k]]["url"]) {
              if (!is.na(new[[i]]["title"]) & !is.na(new[[i-k]]["title"])) {
                if (length(agrep(new[[i]]["title"],new[[i-k]]["title"],ignore.case=TRUE,max.distance=0.15))) {
                  j <- c(j,i)
                }
              }
            }
          }else if (!is.na(new[[i]]["title"]) & !is.na(new[[i-k]]["title"])) {
            if (length(agrep(new[[i]]["title"],new[[i-k]]["title"],ignore.case=TRUE,max.distance=0.15))) {
              if (!is.na(new[[i]]["author"]) & !is.na(new[[i-k]]["author"])) {
                if (length(agrep(new[[i]]["author"],new[[i-k]]["author"],ignore.case=TRUE,max.distance=0.15))) {
                  j <- c(j,i)
                }
              }else  warning(paste0("Please provide author(s) for source[[",i,"]]"))
            }
          }else  warning(paste0("Please provide a title for source[[",i,"]]"))
        }
      }
      #remove any redundant sources
      if (!is.null(j))  new[j] <- NULL
    }
    return(new)
  }
  
  .setCalcHistory <- function(old,new) {
    if (is.null(new))  return(old)
    if (is(new,"Node")){
      if (is.null(old))  return(new)
      #if new entry is a single childless node, it gets set as the root node of the existing history
      else if (data.tree::isLeaf(new) & is.null(new$children)) {
        if (is(old,"Node")){
          #root node is set to ROOT when calcHistory is merged in updateMetadata, so it shall be replaced by value
          if (old$name=="ROOT")  old$name <- new$name
          else {
            c <- data.tree::Clone(old)
            new$AddChildNode(c)
          }
        }#if new entry is a full tree, it replaces the existing history
      }
      #new entry can also be a character of length 1, converted here to a node
    }else if (is.character(new)){
      if (length(new)==1){
        if (is.null(old))  return(data.tree::Node$new(new))
        if (is(old,"Node")){
          if (old$name=="ROOT")  old$name <- new
          else{
            c <- data.tree::Clone(old)
            new <- data.tree::Node$new(new)
            new$AddChildNode(c)
          }
        }else  new <- data.tree::Node$new(new)
      }else  warning(new,"is an invalid argument for calcHistory! The argument must be a character of length 1 or a Node object.")
    }else  warning(new," is an invalid argument for calcHistory! The argument must be a string or a Node object.")
    return(new)
  }
  
  #initialize existing metadata
  M <- attr(x, "Metadata")
  if (!is.list(M))  M <- list()
  
  #handle all metadata fields if no type specified
  if (is.null(type)){
    if (!is.list(value) & !is.null(value))  stop("Metadata must be provided as a list if no type is specified")
    else{
      #unit
      if (!is.null(value$unit)){
        if (length(value$unit)>1){
          warning(value$unit," is an invalid argument for unit")
          #Default unit 1 indicates unitless or "no units specified"
          M$unit <- "1"
        }else  M$unit <- value$unit
      }else  M$unit <- "1"
      #source
      if (!is.null(value$source)){
        M$source <- .setSource(M$source,value$source)
      }
      #calcHistory
      M$calcHistory <- .setCalcHistory(M$calcHistory,value$calcHistory)
      #user
      if (!is.null(value$user)){
        if (!is.character(value$user) & length(value$user)!=1){
          warning(value$user," is an invalid argument for user! Please use getMetadata or updateMetadata to provide a user")
          M$user <- NULL
        }else  M$user <- value$user
      }
      #date
      if(!is.null(value$date)){
        if(!is.character(value$date) & length(value$date)!=1){
          warning(value$date," is an invalid argument for date! Please use getMetadata or updateMetadata to provide a date")
          M$date <- NULL
        }else  M$date <- value$date
      }
      #description
      if(!is.null(value$description)){
        if(is.character(value$description))  M$description <- value$description
        else if (is.list(value$description))  M$description <- value$description
        else{
          warning(value$description," is an invalid argument for description!")
          M$description <- NULL
        }
      }
      #note
      if(!is.null(value$note)){
        if(is.character(value$note))  M$note <- value$note
        else if (is.list(value$note))  M$note <- value$note
        else{
          warning(value$note," is an invalid argument for note!")
          M$note <- NULL
        }
      }
    }
    #if a type argument is given, only handle that particular field
  }else if (type=="unit"){
    if (is.character(value) & length(value)==1)  M[[type]] <- value
    else if (is.null(value))  M$unit <- '1'
    else  warning(value," is an invalid argument for unit!")
  }else if (type=="source"){
    if (is.null(value))  M[[type]] <- value
    else  M[[type]] <- .setSource(M$source,value)
  }else if (type == "calcHistory"){
    M[[type]] <- .setCalcHistory(M$calcHistory,value)
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
