#' updateMetadata (!experimental!)
#' 
#' This function is currently experimental and non-functional by default! To activate it,
#' set withMetadata(TRUE), otherwise it will not return or modify any metadata!
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
#' - "update": if units of x do not match units of y, sets units to "mixed". Else, copies units of y to x.
#' - string or vector specifying new units for x
#' The default argument is "keep". 
#' @param source A list indicating the source(s) of the MAgPIE data. Possible arguments are "keep", "copy",
#' "clear", or a new source can be entered here in the form of a list. "keep" by default.
#' @param calcHistory A tree-like object of class Node indicating the functions through which x has passed. 
#' Possible arguments are "keep", "copy", "clear", and "update", which adds the function presently calling 
#' updateMetadata (or a function further upstream if specified by n) to calcHistory and also merges if y is 
#' provided. A node object can also be provided which will overwrite any existing value. "keep" by default.
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
#' @importFrom data.tree Node
#' @importFrom data.tree Clone
#' 
updateMetadata <- function(x, y=NULL, unit="keep", source="keep", calcHistory="keep", user="update", date="update", description="keep", n=1){
  if(!withMetadata()) return(x)
  
  Mx <- getMetadata(x)
  #Recursive function to merge metadata from a list of magpie objects.
  if (is.list(y)){
    for (i in 1:length(y)){
      if (is.magpie(y[[i]])){
        #Special calcHistory handling for merging a list of magpie objects.
        if (calcHistory=="update"){
          if (i==1){
            f <- as.character(sys.call(-n))[1]
            if (f=="/"|f=="*"|f=="+"|f=="-"|f=="^"|f=="%%"|f=="%/%")  f <- paste0("Ops(",f,")")
            else if (f=="mcalc")  f <- paste0(f,"(",as.character(sys.call(-n))[3],")")
            fn <- Node$new(f)
            if (is(Mx$calcHistory,"Node")){
              cHx <- Clone(Mx$calcHistory)
              cHx$name <- paste(i,cHx$name)
              if (is(getMetadata(y[[i]],"calcHistory"),"Node")){
                cHy <- Clone(getMetadata(y[[i]],"calcHistory"))
                cHy$name <- paste(i+1,cHy$name)
                fn$AddChildNode(cHx)$AddSiblingNode(cHy)
                j <- i+2
              }else  fn$AddChildNode(cHx)
            }else if (is(getMetadata(y[[i]],"calcHistory"),"Node")){
              cHy <- Clone(getMetadata(y[[i]],"calcHistory"))
              cHy$name <- paste(i,cHy$name)
              fn$AddChildNode(cHy)
              j <- i+1
            }else{
              fn$AddChild(i)
              j <- i+1
            }
            Mx$calcHistory <- fn
          }else{
            if (is(Mx$calcHistory,"Node")){
              cHx <- Clone(Mx$calcHistory)
              if (is(getMetadata(y[[i]],"calcHistory"),"Node")){
                cHy <- Clone(getMetadata(y[[i]],"calcHistory"))
                cHy$name <- paste(j,cHy$name)
                cHx$AddChildNode(cHy)
              }else  cHx$AddChild(j)
              Mx$calcHistory <- cHx
              j <- j+1
            }
          }
          x <- updateMetadata(x, y[[i]], unit, source, Mx$calcHistory, user, date, description, n=n+1)
        }else  x <- updateMetadata(x, y[[i]], unit, source, calcHistory, user, date, description, n=n+1)
      }else  stop("All list components of y must be magpie objects!")
    }
    return(x)
  }else if (!is.null(y) & !is.magpie(y))  warning("y argument must be a magpie object or a list of magpie objects!")
  
  My <- getMetadata(y)
  
  if (unit=="copy"){
    if (!is.null(y))  Mx$unit <- My$unit
    else  warning("Units cannot be copied without a second magpie argument provided!")
  }else if (unit=="clear")  Mx$unit <- NULL
  else if (unit=="update"){
    if (!is.null(getMetadata(x,"date"))){
      if (is.null(Mx$unit))  Mx$unit <- "1"
      if (is.null(My$unit))  My$unit <- "1"
      if (Mx$unit!=My$unit)  Mx$unit <- "mixed"
    }else  Mx$unit <- My$unit
  }else if (unit!="keep")  Mx$unit <- unit
  
  if (source[[1]]=="copy"){
    if (!is.null(y)){
      if (is.list(My$source)){
        if (is.list(Mx$source)){
          if (is.list(Mx$source[[2]]))  Mx$source <- append(Mx$source, list(My$source))
          else  Mx$source <- list(Mx$source, My$source)
        }else  Mx$source <- My$source
      }
    }else  warning("Source cannot be copied without a second magpie argument provided!")
  }else if (source[[1]]=="update")  warning("Update is an invalid argument for source! Please specify keep, copy, or clear.")
  else if (source[[1]]=="clear")  Mx$source <- NULL
  else if (source[[1]]!="keep")  Mx$source <- source
  
  if (is(calcHistory,"Node"))  Mx$calcHistory <- calcHistory
  else if (calcHistory=="update"){
    if (!is.na(as.character(sys.call(-n))[1]) & !is.null(sys.call(-n))){
      f <- as.character(sys.call(-n))[1]
      if (f=="/"|f=="*"|f=="+"|f=="-"|f=="^"|f=="%%"|f=="%/%")  f <- paste0("Ops(",f,")")
      else if (f=="mcalc")  f <- paste0(f,"(",as.character(sys.call(-n))[3],")")
      fn <- Node$new(f)
      if (is(Mx$calcHistory,"Node")){
        cHx <- Clone(Mx$calcHistory)
        fn$AddChildNode(cHx)
      }
    }else  warning("n argument is out of range! calcHistory has not been updated!")
    if (!is.null(y)){
      if (is(My$calcHistory,"Node")){
        cHy <- Clone(My$calcHistory)
        fn$AddChildNode(cHy)
      }
    }
    Mx$calcHistory <- fn
  }else if (calcHistory=="copy"){
    if (!is.null(y)){
      if (is(Mx$calcHistory,"Node"))  warning("Cannot overwrite calcHistory! Use update argument to merge.")
      else if (!is(My$calcHistory, "Node"))  warning("Attempting to copy a calcHistory which is not a Node object!")
      else  Mx$calcHistory <- My$calcHistory
    }else  warning("calcHistory cannot be copied without a second magpie argument provided!")
  }else if (calcHistory=="clear")  warning("calcHistory cannot be cleared! Please specify keep, update, or copy.")
  else if (calcHistory!="keep"){
    if (is.character(calcHistory) & length(calcHistory)==1)  Mx$calcHistory <- calcHistory
    else  warning("Invalid argument ",calcHistory," for calcHistory!")
  }
  
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