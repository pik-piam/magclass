#' updateMetadata (!experimental!)
#' 
#' This function is currently experimental and non-functional by default! To activate it,
#' set options(magclass_metadata=TRUE), otherwise it will not return or modify any metadata!
#' 
#' This function is to be used by other functions to update metadata for magclass objects 
#' 
#' When an operation is performed on a MAgPIE dataset, updateMetadata can be used to copy 
#' Metadata entries to the new MAgPIE object or update the Metadata fields appropriately.  
#' fields of "unit", "source", "date", "user" and "calcHistory", 
#' contained in a list.
#' 
#' The "source" component should include all information about the source(s)
#' where the data was originally reported. Specifically, the authors,
#' publication date, article title, journal 
#' 
#' @aliases updateMetadata
#' @param x MAgPIE object to be updated
#' @param y MAgPIE object to copy Metadata from (optional)
#' @param unit An object of type units indicating the units of measure of the MAgPIE data. 
#' Possible arguments are: 
#' - "keep": maintains the unit field in x
#' - "copy": copies the unit field of y to x
#' - "clear": deletes the unit field from x
#' - string or vector specifying new units for x
#' The default argument is "keep". 
#' @param source A list indicating the source(s) of the MAgPIE data. Possible arguments are "keep", "copy",
#' "clear", or a new source can be entered here in the form of a list. "keep" by default.
#' @param calcHistory A vector indicating the functions through which x has passed. Possible arguments are
#' "keep", "copy", "clear", and "update", which adds the function presently calling updateMetadata (or a 
#' function further upstream if specified by n) to calcHistory and also merges if y is provided. "keep" by default.
#' @param date A character indicating the MAgPIE object's last modified date. Possible arguments are 
#' "keep", "copy", and "update", which sets the date of x to the current time. "update" by default.
#' @param user A string indicating the user who last modified the MAgPIE object. Possible arguments are "keep",
#' "copy", "update", which retrieves the username currently logged into the system, or a character string 
#' which specifies a new user. "update" by default.
#' @param description A character string containing a description of the dataset. Possible arguments are 
#' "keep", "copy", "clear", or a new description can be defined here by a character string. "keep" by default.
#' @param n If calcHistory is to be updated, this integer indicates how many frames ahead in the stack to 
#' find the function to append to the the object's calcHistory. n=1 by default.
#' @return updateMetadata returns the magpie object x with metadata modified as desired.
#' @author Stephen Bi
#' @seealso \code{\link{getComment}}, \code{\link{getMetadata}}, \code{\link{getNames}},
#' \code{\link{getYears}}, \code{\link{getCPR}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}}, \code{"\linkS4class{magpie}"}
#' @export
#' 
updateMetadata <- function(x, y=NULL, unit="keep", source="keep", calcHistory="keep", user="update", date="update", description="keep", n=1){
  if(!isTRUE(getOption("magclass_metadata"))) return(x)
  if (is.list(y)){
    for (i in 1:length(y)){
      if (is.magpie(y[[i]]))  x <- updateMetadata(x, y[[i]], unit, source, calcHistory, user, date, description, n=n+i)
      else  stop("All list components of y must be magpie objects!")
    }
    return(x)
  }else if (!is.null(y) & !is.magpie(y))  warning("y argument must be a magpie object or a list of magpie objects!")
  
  Mx <- getMetadata(x)
  My <- getMetadata(y)

  if (unit=="copy"){
    if (!is.null(y))  Mx$unit <- My$unit
    else  warning("Units cannot be copied without a second magpie argument provided!")
  }else if (unit=="clear")  Mx$unit <- NULL
  else if (unit=="update")  warning("Update is an invalid argument for unit! Valid arguments include: copy, clear, keep, or a desired unit character")
  else if (unit!="keep")  Mx$unit <- unit
  
  if (source=="copy"){
    if (!is.null(y)){
      if (is.list(My$source)){
        if (is.list(Mx$source)){
          if (is.list(Mx$source[[2]]))  Mx$source <- append(Mx$source, list(My$source))
          else  Mx$source <- list(Mx$source, My$source)
        }else  Mx$source <- My$source
      }
    }else  warning("Source cannot be copied without a second magpie argument provided!")
  }else if (source=="update")  warning("Update is an invalid argument for source! Please specify keep, copy, or clear.")
  else if (source=="clear")  Mx$source <- NULL
  else if (source!="keep")  Mx$source <- source
  
  if (calcHistory=="update"){
    fn <- as.character(sys.call(-n))
    if (!is.na(fn[1]) & !is.null(fn[1])){
      if (is.null(y)){
        if (!is.null(Mx$calcHistory) & !is.list(Mx$calcHistory))  Mx$calcHistory <- list(Mx$calcHistory, fn[1])
        else if (is.list(Mx$calcHistory))  Mx$calcHistory <- append(Mx$calcHistory, fn[1])
        else  Mx$calcHistory <- fn[1]
      }else if (is.null(getMetadata(x))){
        if (!is.null(My$calcHistory))  Mx$calcHistory <- list(My$calcHistory, fn[1])
        else  Mx$calcHistory <- fn[1]
      }else if (is.list(Mx$calcHistory))  Mx$calcHistory[[length(Mx$calcHistory)+1]] <- c(My$calcHistory, fn[1])
      else  Mx$calcHistory <- list(Mx$calcHistory, c(My$calcHistory, fn[1]))
    }else  warning("n argument is out of range! calcHistory has not been updated!")
  }else if (calcHistory=="copy"){
    if (!is.null(y)){
      if (is.null(getMetadata(x)))  Mx$calcHistory <- My$calcHistory
      else if (is.list(Mx$calcHistory))  Mx$calcHistory[[length(Mx$calcHistory)+1]] <- My$calcHistory
      else  Mx$calcHistory <- list(Mx$calcHistory, My$calcHistory)
    }else  warning("calcHistory cannot be copied without a second magpie argument provided!")
  }else if (calcHistory=="clear")  warning("calcHistory cannot be cleared! Please specify keep, update, or copy.")
  else if (calcHistory!="keep")  warning("Invalid argument ",calcHistory," for calcHistory!")
  
  if (user=="update"){
    env <- if(.Platform$OS.type == "windows") "USERNAME" else "USER"
    Mx$user <- Sys.getenv(env)
  }else if (user=="copy"){
    if (!is.null(y)){
      if (!is.null(My$user))  Mx$user <- My$user
      else  warning("Attempting to copy a NULL user!")
    }else  warning("User cannot be copied without a second magpie argument provided!")
  }else if (user=="clear")  Mx$user <- NULL
  else if (user!="keep"){
    if (is.character(user) & length(user)==1)  Mx$user <- user
    else  warning("Invalid argument ",user," for user!")
  }
  
  if (date=="update")  Mx$date <- as.character(Sys.time())
  else if (date=="copy"){
    if (!is.null(y)){
      if (!is.null(My$date))  Mx$date <- My$date
      else  warning("Attempting to copy a NULL date!")
    }else  warning("date cannot be copied without a second magpie argument provided!")
  }else if (date=="clear")  warning("date cannot be cleared! Please specify keep, copy, or update.")
  else if (date!="keep")  warning("Invalid argument ", date," for date!")
    
  if (description=="copy"){
    if (!is.null(y))  Mx$description <- My$description
    else  warning("Description cannot be copied without a second magpie argument provided!")
  }else if (description=="clear")  Mx$description <- NULL
  else if (description=="update")  warning("Update is an invalid argument for description! Please specify keep, copy, merge, or clear.")
  else if (description!="keep"){
    if (is.character(description))  Mx$description <- description
    else  warning("Invalid argument ",description," for description!")
  }
  getMetadata(x) <- Mx
  return(x)
}