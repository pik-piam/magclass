#' updateMetadata
#' 
#' This function is to be used by other functions to update metadata for magclass objects 
#' 
#' When an operation is performed on a MAgPIE dataset, updateMetadata can be used to copy 
#' Metadata entries to the new MAgPIE object or update the Metadata fields appropriately.  
#' fields of "unit", "source", "creationDate", "user" and "calcHistory", 
#' contained in a list.
#' 
#' The "source" component should include all information about the source(s)
#' where the data was originally reported. Specifically, the authors,
#' publication date, article title, journal 
#' 
#' @aliases updateMetadata
#' @param x MAgPIE object to be updated
#' @param y MAgPIE object to copy Metadata from
#' @param unit A vector indicating the units of measure of the MAgPIE data. Possible arguments are: 
#' - "keep": maintains the unit field in x
#' - "copy": copies the unit field of y to x
#' - "clear": deletes the unit field from x
#' - "merge": concatenates the unit fields of x and each element of y into a list.
#' - string or vector specifying new units for x
#' The default argument is "keep". 
#' @param source A list indicating the source(s) of the MAgPIE data. Possible arguments are "keep", "copy",
#' "clear", and "merge", or a new source can be entered here in the form of a list. "keep" by default.
#' @param calcHistory A vector indicating the functions through which x has passed. Possible arguments are
#' "keep", "copy", "clear", "merge", and "update", which adds the function presently calling updateMetadata 
#' to calcHistory and also merges if y is provided. "update" by default.
#' @param creationDate A character indicating the MAgPIE object's creation date. Possible arguments are 
#' "keep", "copy", and "update", which sets the creationDate of x to the current time. "update" by default.
#' @param user A string indicating the user who created the MAgPIE object. Possible arguments are "keep",
#' "copy", "update", which retrieves the username currently logged into the system, or a character string 
#' which specifies a new user. "update" by default.
#' @param description A character string containing a description of the dataset. Possible arguments are 
#' "keep", "copy", "clear", "merge", or a new description can be defined here by a character string. 
#' "keep" by default.
#' @param n If calcHistory is to be updated, this integer indicates how many frames ahead in the stack to 
#' find the function to append to the the object's calcHistory. n=1 by default.
#' @return updateMetadata returns the magpie object x with metadata modified as desired.
#' @author Stephen Bi
#' @seealso \code{\link{getComment}}, \code{\link{getMetadata}}, \code{\link{getNames}},
#' \code{\link{getYears}}, \code{\link{getCPR}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}}, \code{"\linkS4class{magpie}"}, \code{\link{make_unit}}
#' @examples
#' 
#' x <- as.magpie(0)
#' y <- as.magpie(1)
#' getMetadata(y, "unit") = "km"
#' x <- updateMetadata(x, y, description = "bla")
#' getMetadata(x)
#' 
#' @export
#' @importFrom units make_unit

updateMetadata <- function(x, y=NULL, unit="keep", source="keep", calcHistory="keep", user="update", creationDate="update", description="keep", n=1){
  
  if (is.list(y)){
    for (i in 1:length(y)){
      if (!is.null(getMetadata(y[[i]])))
        if (i==1)  x <- updateMetadata(x, y[[i]], unit, source, calcHistory, user, creationDate, description, n=n+i)
        else{
          if (!is.null(getMetadata(y[[i]], "unit")) && is.character(getMetadata(y[[i]], "unit"))){
            if (!is.null(getMetadata(y[[i-1]], "unit")) && is.character(getMetadata(y[[i-1]], "unit"))){
              if (getMetadata(y[[i]],"unit") == getMetadata(y[[i-1]],"unit"))  iUnit <- "keep"
            }else iUnit <- unit
          }else iUnit <- unit
          if (!is.null(getMetadata(y[[i]],"source"))) iSource <- "merge"
          else iSource <- "keep"
          if (!is.null(getMetadata(y[[i]],"description")))  iDescription <- "merge"
          else iDescription <- "keep"
          x <- updateMetadata(x, y[[i]], iUnit, iSource, calcHistory, user, creationDate, iDescription, n=n+i)
        }
    }
    return(x)
  }else if (!is.null(y) & !is.magpie(y))  warning("y argument must be a magpie object or a list of magpie objects!")
  
  Mx <- getMetadata(x)
  My <- getMetadata(y)
  if (calcHistory=="update")  fn <- as.character(sys.call(-n))
  
  if (user=="update"){
    env <- if(.Platform$OS.type == "windows") "USERNAME" else "USER"
    Mx$user <- Sys.getenv(env)
  }
  if (creationDate=="update")  Mx$creationDate <- as.character(Sys.time())
  
  if (is.null(y)){
    if (unit=="copy")  warning("Units cannot be copied without a second magpie argument provided!")
    else if (unit=="clear")  Mx$unit <- NULL
    else if (unit=="update")  warning("Update is an invalid argument for unit!")
    else if (unit=="merge")  warning("Units cannot be merged without a second magpie argument provided!")
    else if (unit!="keep") if(is.character(unit)) Mx$unit <- unit else warning("Invalid argument for unit!")
    
    if (source=="copy")  warning("Source cannot be copied without a second magpie argument provided!")
    else if (source=="merge")  warning("Sources cannot be merged without a second magpie argument provided!")
    else if (source=="update")  warning("Update is an invalid argument for source! Please specify keep, copy, merge, or clear.")
    #Should source be deletable?
    else if (source=="clear")  Mx$source <- NULL
    else if (source!="keep")  if(is.list(source)) Mx$source <- source else warning("Invalid argument for source!")
    
    if (calcHistory=="update" & n!=0){ 
      if (!is.na(fn[1]) && !is.null(fn[1]))  Mx$calcHistory <- c(Mx$calcHistory, fn[1])
    }
    else if (calcHistory=="copy")  warning("calcHistory cannot be copied without a second magpie argument provided!")
    #Should calcHistory be deletable?
    else if (calcHistory=="clear")  warning("calcHistory cannot be cleared! Please specify keep, update, merge, or copy.")
    else if (calcHistory=="merge")  warning("calcHistory cannot be merged without a second magpie argument provided!")
    else if (calcHistory!="keep")  warning("Invalid argument for calcHistory!")
    
    if (user=="copy")  warning("User cannot be copied without a second magpie argument provided!")
    else if (user=="clear")  Mx$user <- NULL
    else if (user=="merge")  warning("Merge is an invalid argument for user! Please specify keep, update, clear, or copy.")
    else if (user!="update" & user!="keep"){
      if (is.character(user) & length(user)==1)  Mx$user <- user
      else  warning("Invalid argument for user!")
    }
    
    if (creationDate=="copy")  warning("creationDate cannot be copied without a second magpie argument provided!")
    else if (creationDate=="clear")  warning("creationDate cannot be cleared! Please specify keep, copy, or update.")
    else if (creationDate=="merge")  warning("creationDate cannot be merged! Please specify keep, copy, or update.")
    else if (creationDate!="update" & creationDate!="keep")  warning("Invalid argument for creationDate!")
    
    if (description=="copy")  warning("Creation Date cannot be copied without a second magpie argument provided!")
    else if (description=="clear")  Mx$description <- NULL
    else if (description=="merge")  warning("Description cannot be merged without a second magpie argument provided!")
    else if (description=="update")  warning("Update is an invalid argument for description! Please specify keep, copy, merge, or clear.")
    else if (description!="keep") if(is.character(description)) Mx$description <- description else warning("Invalid argument for description!")
    
  }else{
    if (is(unit,"units"))  Mx$unit <- unit
    else if (unit=="copy")  if(!is.null(My$unit)) Mx$unit <- My$unit else warning("Attempting to copy NULL unit!")
    else if (unit=="clear")  Mx$unit <- NULL
    else if (unit=="merge"){
      if (!is.null(Mx$unit)){
        if (!is.list(Mx$unit))  Mx$unit <- list(Mx$unit, My$unit)
        else  Mx$unit[[length(Mx$unit)+1]] <- My$unit
      }else  Mx$unit <- My$unit
    }else if (unit=="update")  warning("Invalid argument for unit!")
    else if (unit!="keep")  if(is.character(unit))  Mx$unit <- unit else warning("Invalid argument for unit!")
    
    if (source=="copy")  if(!is.null(My$source)) Mx$source <- My$source else warning("Attempting to copy a NULL source!")
    else if (source=="merge"){ 
      if (!is.null(Mx$source)){
        if (!is.null(My$source)) Mx$source <- append(Mx$source, list(My$source))
        else warning("y has a NULL entry for source!")
      }else if (is.null(My$source))  warning("y has a NULL entry for source!")
      else Mx$source <- list(My$source)
    }else if (source=="clear")  Mx$source <- NULL
    else if (source=="update")  warning("Update is an invalid argument for source! Please specify keep, copy, merge, clear, or a new source in a list")
    else if (source!="keep")  if(is.list(source)) Mx$source <- source else warning("Invalid argument for source!")
    
    if (calcHistory=="update"){
      if (!is.null(My$calcHistory)){
          if (!is.null(Mx$calcHistory)){
            if (is.list(Mx$calcHistory))  Mx$calcHistory[[length(Mx$calcHistory)+1]] <- c(My$calcHistory, fn[1])
            else  Mx$calcHistory <- list(Mx$calcHistory, c(My$calcHistory, fn[1]))
          }else Mx$calcHistory <- c(My$calcHistory, fn[1])
      }else if (!is.null(Mx$calcHistory))  Mx$calcHistory <- c(Mx$calcHistory, fn[1])
      else  Mx$calcHistory <- fn[1]
    }else if (calcHistory=="merge"){
      if (!is.null(My$calcHistory)){
        if (!is.null(Mx$calcHistory)){
          if (is.list(Mx$calcHistory))  Mx$calcHistory[[length(Mx$calcHistory)+1]] <- My$calcHistory
          else  Mx$calcHistory <- list(Mx$calcHistory, My$calcHistory)
        }else Mx$calcHistory <- My$calcHistory
      }
    }else if (calcHistory=="copy"){
      if (!is.null(My$calcHistory)) Mx$calcHistory <- My$calcHistory else warning("Attempting to copy a NULL calcHistory!")
    }else if (calcHistory=="clear") warning("Calculation history cannot be cleared! Please specify keep, update, or copy.")
    else if (calcHistory!="keep")  warning("Invalid argument for calcHistory!")
    
    if (user=="copy"){
      if (!is.null(My$user)) Mx$user <- My$user else warning("Attempting to copy a NULL user!")
    }else if (user=="clear")  Mx$user <- NULL
    else if (user!="update" & user!="keep"){
      if (is.character(user) & length(user)==1)  Mx$user <- user
      else  warning("Invalid argument for user!")
    }
    
    if (creationDate=="copy"){
      if (!is.null(My$creationDate)) Mx$creationDate <- My$creationDate else warning("Attempting to copy a NULL creationDate!")
    }else if (creationDate=="clear")  warning("Creation date cannot be cleared, only updated or copied!")
    else if (creationDate!="update" & creationDate!="keep")  warning("Invalid argument for creationDate")

    if (description=="copy")  if(!is.null(My$description)) Mx$description <- My$description else warning("Attempting to copy a NULL description!")
    else if (description=="clear")  Mx$description <- NULL
    else if (description=="update")  warning("Update is an invalid argument for description! Please specify keep, copy, merge, or clear.")
    else if (description=="merge"){
      if (!is.null(Mx$description)){
        if (!is.list(Mx$description))  Mx$description <- list(Mx$description, My$description)
        else  Mx$description[[length(Mx$description)+1]] <- My$description
      }else  Mx$description <- My$description
    }else if (description!="keep") if(is.character(description)||is.list(description)) Mx$description <- description else warning("Invalid argument for description!")
  }
  getMetadata(x) <- Mx
  return(x)
}