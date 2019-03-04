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
  if (Sys.getlocale("LC_CTYPE")!="en_US.UTF-8")  tmp <- suppressWarnings(Sys.setlocale("LC_ALL","en_US.UTF-8"))
  if (!requireNamespace("data.tree", quietly = TRUE)) stop("The package data.tree is required for metadata handling!")
  units::units_options(auto_convert_names_to_symbols=FALSE, allow_mixed=FALSE, negative_power=TRUE, set_units_mode="standard")
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
#' @importFrom utils toBibtex
"getMetadata<-" <- function(x, type=NULL, value) {
  if(!withMetadata()) return(x)
  if (Sys.getlocale("LC_CTYPE")!="en_US.UTF-8")  tmp <- suppressWarnings(Sys.setlocale("LC_ALL","en_US.UTF-8"))
  if(!is.magpie(x)){
    warning("x argument must be a magpie object!")
    return(x)
  }
  if (!requireNamespace("data.tree", quietly = TRUE)) stop("The package data.tree is required for metadata handling!")
  units::units_options(auto_convert_names_to_symbols=FALSE, allow_mixed=FALSE, negative_power=TRUE, set_units_mode="standard")
  
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
          if (old$name=="ROOT") {
            old$name <- new$name
            return(old)
          }else {
            c <- data.tree::Clone(old)
            new$AddChildNode(c)
          }
        }#if new entry is a full tree, it replaces the existing history
      }
      #new entry can also be a character of length 1, converted here to a node
    }else if (is.character(new)){
      if (length(new)==1){
        if (new=="delete")  return(NULL)
        if (is.null(old))  return(data.tree::Node$new(new))
        if (is(old,"Node")){
          if (old$name=="ROOT") {
            old$name <- new
            return(old)
          }else {
            c <- data.tree::Clone(old)
            new <- data.tree::Node$new(new)
            new$AddChildNode(c)
          }
        }else  new <- data.tree::Node$new(new)
      }else  warning(new,"is an invalid argument for calcHistory! The argument must be a character of length 1 or a Node object.")
    }else  warning(new," is an invalid argument for calcHistory! The argument must be a string or a Node object.")
    return(new)
  }
  
  .setVersion <- function(pre,ver) {
    if (is.character(ver)) {
      new <- vector()
      if (length(ver)==1) {
        if (grepl(";",ver,fixed=TRUE)) {
          ver <- unlist(strsplit(ver,";",fixed=TRUE))
        }
      }
      for (i in 1:length(ver)) {
        if (grepl("[[:digit:]]",ver[i]) & grepl("[[:alpha:]]",ver[i])) {
          new[i] <- trimws(gsub("[[:alpha:]]","",ver[i]))
          names(new)[i] <- trimws(gsub("[[:digit:]]","",gsub("[[:punct:]]","",ver[i])))
        }else if(grepl("[[:alpha:]]",names(ver[i]))) {
          if (grepl("[[:digit:]]",ver[i])) {
            new[i] <- ver[i]
            names(new)[i] <- names(ver[i])
          }else  warning(ver[i]," is an invalid entry for version! Please provide the version number.")
        }else  warning(ver[i]," is an invalid entry for version! Please provide both the package name and version number.")
      }
      if (!is.null(pre)) {
        dbl <- vector()
        k <- 1
        for (i in 1:length(new)) {
          for (j in 1:length(pre)) {
            if (as.character(names(pre)[j])==as.character(names(new)[i])) {
              dbl[k] <- i
              k <- k+1
              if (as.package_version(new[i]) < as.package_version(pre[j])) {
                warning(paste("The provided version",new[i],"for the",names(new[i]),"package is behind the previously used version",paste0("(",pre[j],")")))
              }
              pre[j] <- new[i]
            }
          }
        }
        if (length(dbl>0)) {
          new <- new[-dbl]
        }
        return(c(pre,new))
      }else  return(new)
    }else {
      warning(ver,"is an invalid entry for version! Please use getMetadata or updateMetadata to enter a version number.")
      return(pre)
    }
  }
  
  conv2unit <- function(x) {
    if (is.null(x)) {
      x <- install_magpie_units("unknown")
    }else if (length(x)>1) {
      x <- install_magpie_units("unknown")
    #*****Mixed units handling in development*****  
    #  if (length(unique(x))==1) {
    #    x <- unique(x)
    #  } else {
    #    if (is(x,"mixed_units")) {
    #      return(x)
    #    }else {
    #      for (i in 1:length(x)) {
    #        x[i] <- install_magpie_units(x[i])
    #      }
    #      x <- mixed_units(1,x)
    #    }
    #  }
    }else if (!is(x,"units") & !is(x,"mixed_units")) {
      x <- install_magpie_units(x)
    }
    return(x)
  }
  
  #initialize existing metadata
  M <- attr(x, "Metadata")
  if (!is.list(M))  M <- list()
  
  #handle all metadata fields if no type specified
  if (is.null(type)){
    if (!is.list(value) & !is.null(value))  stop("Metadata must be provided as a list if no type is specified")
    else{
      #unit
      M$unit <- conv2unit(value$unit)
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
        if(is.character(value$description) | is.list(value$description)) {
          M$description <- unique(value$description)
          if (length(M$description)==1)  M$description <- unlist(M$description)
        }
        else{
          warning(value$description," is an invalid argument for description!")
          M$description <- NULL
        }
      }
      #note
      if(!is.null(value$note)) {
        if(is.character(value$note) | is.list(value$note)) {
          M$note <- unique(value$note)
          if (length(M$note)==1)  M$note <- unlist(M$note)
        }
        else{
          warning(value$note," is an invalid argument for note!")
          M$note <- NULL
        }
      }
      #version
      if (!is.null(value$version)) {
        M$version <- .setVersion(M$version,value$version)
      }else  M$version <- NULL
    }
    #if a type argument is given, only handle that particular field
  }else if (type=="unit"){
    M$unit <- conv2unit(value)
  }else if (type=="source"){
    if (is.null(value))  M[[type]] <- value
    else  M[[type]] <- .setSource(M$source,value)
  }else if (type == "calcHistory"){
    M[[type]] <- .setCalcHistory(M$calcHistory,value)
  }else if (type=="date"){
    if  ((is.character(value) & length(value)==1) | is.null(value))  M[[type]] <- value
    else  warning(value," is an invalid argument for date! Please use getMetadata or updateMetadata to enter a date.")
  }else if (type=="user"){
    if ((is.character(value) & length(value)==1) | is.null(value)) {
      if (length(unique(value))==1)  M[[type]] <- unlist(unique(value))
      else  M[[type]] <- unique(value)
    }
    else  warning(value," is an invalid argument for user! Please use getMetadata or updateMetadata to enter a user.")
  }else if (type=="description"){
    if(is.null(value) | is.character(value) | is.list(value)) {
      if (length(unique(value))==1)  M[[type]] <- unlist(unique(value))
      else  M[[type]] <- unique(value)
    }
    else  warning(value," is an invalid argument for description! Please use getMetadata or updateMetadata to enter a description.")
  }else if (type=="note"){
    if(is.null(value) | is.character(value) | is.list(value))  M[[type]] <- value
    else  warning(value," is an invalid argument for note! Please use getMetadata or updateMetadata to enter a note.")
  }else if (type=="version") {
    if (is.null(value)) {
      M$version <- NULL
    }else {
      M$version <- .setVersion(M$version,value)
    }
  }else  warning(type," is not a valid metadata field!")
  
  attr(x, "Metadata") <- M
  return(x)
}
