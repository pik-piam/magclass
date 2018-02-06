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
#' @importFrom data.tree isRoot
#' @importFrom data.tree isLeaf
#' @importFrom data.tree Clone
#' @importFrom data.tree Node
"getMetadata<-" <- function(x, type=NULL, value) {
  if(!withMetadata()) return(x)
  M <- attr(x, "Metadata")
  if (!is.list(M))  M <- list()
  if (is.null(type)){
    if (!is.list(value) & !is.null(value))  stop("Metadata must be provided as a list if no type is specified")
    else{
      if (length(value$unit)>1){
        warning(value$unit," is an invalid argument for unit")
        value$unit <- "1"
      }
      if (!is.null(value$source)){
        if (is.list(value$source)){
          for (i in 1:(length(value$source))){
            if (is.list(value$source[[i]])){
              if (is.null(value$source[[i]]$author)){
                warning("No author provided for source #",i,"!")
                value$source[[i]]$author <- "Not provided"
              }
              if (is.null(value$source[[i]]$title)){
                warning("No title provided for source #",i,"!")
                value$source[[i]]$title <- "Not provided"
              }
              if (is.null(value$source[[i]]$date)){
                warning("No date provided for source #",i,"!")
                value$source[[i]]$date <- "Not provided"
              }
              if (is.null(value$source[[i]]$publication)){
                warning("No publication provided for source #",i,"!")
                value$source[[i]]$publication <- "Not provided"
              }
              if (is.null(value$source[[i]]$institution)){
                warning("No institution provided for source #",i,"!")
                value$source[[i]]$institution <- "Not provided"
              }
            }else if (i==1){
              if (is.null(value$source$author)){
                warning("No author provided for source!")
                value$source$author <- "Not provided"
              }
              if (is.null(value$source$title)){
                warning("No title provided for source!")
                value$source$title <- "Not provided"
              }
              if (is.null(value$source$date)){
                warning("No date provided for source!")
                value$source$date <- "Not provided"
              }
              if (is.null(value$source$publication)){
                warning("No publication provided for source!")
                value$source$publication <- "Not provided"
              }
              if (is.null(value$source$institution)){
                warning("No institution provided for source!")
                value$source$institution <- "Not provided"
              }
            }else if (is.list(value$source[[i-1]])){
              warning("Source #",i," must be a formatted as a list! Please include at least author, title, date, and journal. If possible, also DOI, ISSN, URL, etc")
              value$source[[i]] <- NULL
            }
          }
        }else{
          warning("Source must be a formatted as a list! Please include at least author, title, date, and journal. If possible, also DOI, ISSN, URL, etc")
          value$source <- NULL
        }
      }
      if (!is.null(value$calcHistory)){
        if (is(value$calcHistory,"Node")){
          if (is(M$calcHistory,"Node")){
            if (isLeaf(value$calcHistory) & is.null(value$calcHistory$children)){
              c <- Clone(M$calcHistory)
              value$calcHistory$AddChildNode(c)
            }
          }
        }else if (is.character(value$calcHistory) & length(value$calcHistory)==1){
          if (is(M$calcHistory,"Node")){
            c <- Clone(M$calcHistory)
            value$calcHistory <- Node$new(value$calcHistory)
            value$calcHistory$AddChildNode(c)
          }else  value$calcHistory <- Node$new(value$calcHistory)
        }else{
          warning(value$calcHistory," is an invalid argument for calcHistory! The argument must be a Node object.")
          value$calcHistory <- NULL
        }
      }
      if (!is.null(value$user)){
        if (!is.character(value$user) & length(value$user)!=1){
          warning(value$user," is an invalid argument for user! Please use getMetadata or updateMetadata to provide a user")
          value$user <- NULL
        }
      }
      if(!is.null(value$date)){
        if(!is.character(value$date) & length(value$date)!=1){
          warning(value$date," is an invalid argument for date! Please use getMetadata or updateMetadata to provide a date")
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
    if (is.character(value) & length(value)==1)  M[[type]] <- value
    else if (is.null(value))  M$unit <- '1'
    else  warning(value," is an invalid argument for unit!")
  }else if (type=="source"){
    if (is.null(value))  M[[type]] <- value
    else if (is.list(value)){
      for (i in 1:(length(value))){
        if (is.list(value[[i]])){
          if (is.null(value[[i]]$author)){
            warning("No author provided for source #",i,"!")
            value[[i]]$author <- "Not provided"
          }
          if (is.null(value[[i]]$title)){
            warning("No title provided for source #",i,"!")
            value[[i]]$title <- "Not provided"
          }
          if (is.null(value[[i]]$date)){
            warning("No date provided for source #",i,"!")
            value[[i]]$date <- "Not provided"
          }
          if (is.null(value[[i]]$publication)){
            warning("No publication provided for source #",i,"!")
            value[[i]]$publication <- "Not provided"
          }
          if (is.null(value[[i]]$institution)){
            warning("No institution provided for source #",i,"!")
            value[[i]]$institution <- "Not provided"
          }
        }else if (i==1){
          if (is.null(value$author)){
            warning("No author provided for source!")
            value$author <- "Not provided"
          }
          if (is.null(value$title)){
            warning("No title provided for source!")
            value$title <- "Not provided"
          }else if (is.null(value$date)){
            warning("No date provided for source!")
            value$date <- "Not provided"
          }
          if (is.null(value$publication)){
            warning("No publication provided for source!")
            value$publication <- "Not provided"
          }
          if (is.null(value$institution)){
            warning("No institution provided for source!")
            value$institution <- "Not provided"
          }
        }else if (is.list(value[[i-1]])){
          warning("Source #",i," must be a formatted as a list! Please include at least author, title, date, and journal. If possible, also DOI, ISSN, URL, etc")
        }
      }
    }else{
      warning("Source must be a formatted as a list! Please include at least author, title, date, and journal. If possible, also DOI, ISSN, URL, etc")
    }
  }else if (type == "calcHistory"){
    if (is(value,"Node")){
      if (is(M[[type]],"Node")){
        if (isRoot(value))  M[[type]] <- value
        else if (is.null(value$children)){
          c <- Clone(M[[type]])
          value$AddChildNode(c)
          M[[type]] <- value
        }
      }else  M[[type]] <- value
    }else if (is.character(value)){
      if (length(value)==1){
        if (is(M[[type]],"Node")){
          c <- Clone(M[[type]])
          M[[type]] <- Node$new(value)
          M[[type]]$AddChildNode(c)
        }else  M[[type]] <- Node$new(value)
      }else  warning(value,"is an invalid argument for calcHistory! The argument must be a character of length 1 or a Node object.")
    }else if (is.null(value))  M[[type]] <- value
    else  warning(value," is an invalid argument for calcHistory! The argument must be a character of length 1 or a Node object.")
  }else if (type=="date"){
    if  ((is.character(value) & length(value)==1) | is.null(value))  M[[type]] <- value
    else  warning(value," is an invalid argument for date! Please use getMetadata or updateMetadata to provide a date.")
  }else if (type=="user"){
    if ((is.character(value) & length(value)==1) | is.null(value))  M[[type]] <- value
    else  warning(value," is an invalid argument for user! Please use getMetadata or updateMetadata to provide a user")
  }else if (type=="description"){
    if(is.null(value) | is.character(value))  M[[type]] <- value
    else  warning(value," is an invalid argument for description! Please use getMetadata or updateMetadata to provide a description")
  }else  warning(type," is not a valid metadata field!")
  attr(x, "Metadata") <- M
  return(x)
}
