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
#' The default argument is "keep" if no y argument is provided, or "update" if y is provided. 
#' @param source A list indicating the source(s) of the MAgPIE data. Possible arguments are "keep", "copy",
#' "clear", or a new source can be entered here in the form of a list. By default, "keep" if no y argument,
#' or "copy" if y is provided.
#' @param calcHistory A tree-like object of class Node indicating the functions through which x has passed. 
#' Possible arguments are "keep", "copy", "clear", and "update", which adds the function presently calling 
#' updateMetadata (or a function further upstream if specified by n) to calcHistory and also merges if y is 
#' provided. A node object can also be provided which will overwrite any existing value. By default, "keep" 
#' if no y argument, or "update" if y is provided.
#' @param date A character indicating the MAgPIE object's last modified date. Possible arguments are 
#' "keep", "copy", and "update", which sets the date of x to the current time. "update" by default.
#' @param user A string indicating the user who last modified the MAgPIE object. Possible arguments are "keep",
#' "copy", "update", which retrieves the username currently logged into the system, or a character string 
#' which specifies a new user. "update" by default.
#' @param description A character string containing a description of the dataset. Possible arguments are 
#' "keep", "copy", "clear", or a new description can be defined here by a character string. By default, "keep" 
#' if no y argument, or "copy" if y is provided.
#' @param n If calcHistory is to be updated, this integer indicates how many frames ahead in the stack to 
#' find the function to append to the the object's calcHistory. n=1 by default.
#' @return updateMetadata returns the magpie object x with metadata modified as desired.
#' @author Stephen Bi
#' @seealso \code{\link{getComment}}, \code{\link{getMetadata}}, \code{\link{getNames}},
#' \code{\link{getYears}}, \code{\link{getCPR}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}}, \code{"\linkS4class{magpie}"}
#' @export
#' @importFrom methods getPackageName
#' 
updateMetadata <- function(x, y=NULL, unit=ifelse(is.null(y),"keep","update"), source=ifelse(is.null(y),"keep","copy"), 
                           calcHistory=ifelse(is.null(y),"keep","update"), user="update", date="update", description=ifelse(is.null(y),"keep","copy"), n=1){

  if(!withMetadata()) return(x)
  if (!requireNamespace("data.tree", quietly = TRUE)) stop("The package data.tree is required for metadata handling!")
  if (!isTRUE(getOption("reducedHistory")) & calcHistory=="merge")  calcHistory <- "update"
  
  #Function nodeClone clones a node object and, if necessary, prepares it for merging and attaches it to the new root
  nodeClone <- function(x,fn=NULL){
    if (is.null(x))  return(fn)
    if(isTRUE(getOption("CloneMetadata"))){
      xc <- data.tree::Clone(x)
      if (!is.null(fn) & is(fn,"Node"))  fn$AddChildNode(xc)
      else  return(xc)
    }else{
      if (!is.null(fn) & is(fn,"Node"))  fn$AddChildNode(x)
      else  return(x)
    }
  }
  #Function newCall creates the appropriate call to be displayed for the new root
  newCall <- function(n,convert=TRUE){
    if (!is.na(as.character(sys.call(-n))[1]) & !is.null(sys.call(-n))){
      f <- as.character(sys.call(-n))[1]
      #if (f=="/"|f=="*"|f=="+"|f=="-"|f=="^"|f=="%%"|f=="%/%")  f <- paste0("Ops(",f,")")
      if (f=="mcalc")  f <- paste0(f,"(",as.character(sys.call(-n))[3],")")
      if (getPackageName(sys.frame(-n))=="madrat" | getPackageName(sys.frame(-n))=="moinput"){
        f <- deparse(sys.call(-n),width.cutoff = 500)
        if (grepl("subtype,",f,fixed=TRUE))  f <- gsub("subtype,",paste0(eval.parent(expression(subtype),n),","),f,fixed=TRUE)
        else if (grepl("type,",f,fixed=TRUE))  f <- gsub("type,",paste0(eval.parent(expression(type),n),","),f,fixed=TRUE)
        if (grepl("subtype)",f,fixed=TRUE))  f <- gsub("subtype)",paste0(eval.parent(expression(subtype),n),")"),f,fixed=TRUE)
        else if (grepl("type)",f,fixed=TRUE))  f <- gsub("type)",paste0(eval.parent(expression(type),n),")"),f,fixed=TRUE)
      }
      if (grepl(":::",f[1],fixed=TRUE))  f <- unlist(strsplit(f,":::",fixed=TRUE))[2]
      if (convert==TRUE)  return(data.tree::Node$new(f))
      else  return(f)
    }else  stop("n argument is out of range! calcHistory cannot be updated!")
  }
  
  #Function buildTree constructs the calcHistory data tree 
  buildTree <- function(x,y=NULL,n,cH){
    if (!is.null(y)){
      if (cH=="update")  rt <- newCall(n)
      else if (cH=="merge"){
        if (is(x,"Node")){
          if (x$name=="ROOT")  rt <- nodeClone(x)
          else  rt <- data.tree::Node$new("ROOT")
        }else  rt <- data.tree::Node$new("ROOT")
      }
      if (!is.list(y)){
        mY <- getMetadata(y,"calcHistory")
        if (is(x,"Node")){
          if (is(mY,"Node")){
            if (x$name!="ROOT")  nodeClone(x,rt)
            if (mY$name!="ROOT")  nodeClone(mY,rt)
          }else  return(nodeClone(x))
        }else if (is(mY,"Node"))  return(nodeClone(mY))
      }else{
        mY <- list()
        j <- 0
        for (i in 1:length(y)){
          if (is(getMetadata(y[[i]],"calcHistory"),"Node")){
            j <- j+1
            mY[[j]] <- getMetadata(y[[i]],"calcHistory")
          }
        }
        if (j==0){
          if (is(x,"Node"))  return(nodeClone(x))
          else  return(NULL)
        }else if (j==1){
          if (is(x,"Node")){
            if (x$name!="ROOT")  nodeClone(x,rt)
            else  nodeClone(mY[[1]],rt)
          }else  return(mY[[1]])
        }else{
          if (is(x,"Node")){
            if (x$name!="ROOT")  nodeClone(x,rt)
          }
          for (ii in 1:length(mY)){
            if (mY[[ii]]$name!="ROOT")  nodeClone(mY[[ii]],rt)
          }
          if (rt$count==0 & rt$name=="ROOT")  return(NULL)
          else if (rt$count==1 & rt$name=="ROOT")  return(rt$children[[1]])
        }
      }
    }else{
      if (is(x,"Node")){
        if (x$name=="ROOT"){
          x$name <- newCall(n,convert=FALSE)
          return(nodeClone(x))
        }else{
          rt <- newCall(n)
          nodeClone(x,rt)
        }
      }else  return(newCall(n))
    }
    return(rt)
  }
  Mx <- getMetadata(x)
  #Recursive function to merge metadata from a list of magpie objects.
  if (is.list(y)){
    My <- list()
    for (i in 1:length(y)){
      if (is.magpie(y[[i]])){
        My[[i]] <- getMetadata(y[[i]])
      }else  stop("All list components of y must be magpie objects!")
    }
  }else if (!is.null(y) & !is.magpie(y)){
    y <- NULL
    warning("y argument must be a magpie object or a list of magpie objects!")
  }else  My <- getMetadata(y)
  
  if (is.null(unit))  unit <- "keep"
  if (unit=="copy"){
    if (!is.null(y))  Mx$unit <- My$unit
    else  warning("Units cannot be copied without a second magpie argument provided!")
  }else if (unit=="clear")  Mx$unit <- NULL
  else if (unit=="update"){
    if (!is.null(getMetadata(x,"unit"))){
      if (is.null(Mx$unit))  Mx$unit <- "1"
      if (is.null(My$unit))  My$unit <- "1"
      if (Mx$unit!=My$unit)  Mx$unit <- "mixed"
    }else  Mx$unit <- My$unit
  }else if (unit!="keep")  Mx$unit <- unit
  
  if (is.null(source))  source <- "keep"
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
  
  if (is(calcHistory,"Node") | is.null(calcHistory)){
    getMetadata(x,"calcHistory") <- NULL
    Mx$calcHistory <- calcHistory
  }else if (calcHistory=="update"){
    Mx$calcHistory <- buildTree(Mx$calcHistory,y,n+2,calcHistory)
    getMetadata(x,"calcHistory") <- NULL
  }else if (calcHistory=="merge"){
    if (!is.null(y)){
      Mx$calcHistory <- buildTree(Mx$calcHistory,y,n+2,calcHistory)
    }else  warning(paste(calcHistory,"is an invalid argument for calcHistory! A magpie or list of magpies must be provided for y"))
  }else if (calcHistory=="copy"){
    if (!is.null(y)){
      if (!is.null(My$calcHistory) & (!is(My$calcHistory, "Node")))  warning("Attempting to copy a calcHistory which is not a Node object!")
      else if (is(My$calcHistory,"Node"))  Mx$calcHistory <- data.tree::Clone(My$calcHistory)
      else  Mx$calcHistory <- My$calcHistory
    }else  warning("calcHistory cannot be copied without a second magpie argument provided!")
  }else if (calcHistory=="clear")  warning("calcHistory cannot be cleared! Please specify keep, update, or copy.")
  else if (calcHistory=="keep")  Mx$calcHistory <- NULL
  else if (is.character(calcHistory) & length(calcHistory)==1)  Mx$calcHistory <- calcHistory
  else  warning("Invalid argument ",calcHistory," for calcHistory!")
  
  if (is.null(user))  user <- "keep"
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
  
  if (is.null(date))  date <- "update"
  if (date=="update")  Mx$date <- as.character(Sys.time())
  else if (date=="copy"){
    if (!is.null(y)){
      if (!is.null(My$date))  Mx$date <- My$date
      else  warning("Attempting to copy a NULL date!")
    }else  warning("date cannot be copied without a second magpie argument provided!")
  }else if (date=="clear")  warning("date cannot be cleared! Please specify keep, copy, or update.")
  else if (date!="keep")  warning("Invalid argument ", date," for date!")
    
  if (is.null(description))  Mx$description <- "keep"
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