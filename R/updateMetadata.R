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
#' @param source An object of class Bibtex (or a list of Bibtex objects) indicating the source(s) of the 
#' input data in BibTeX style. Possible arguments are "keep", "clear", "copy" (which overwrites the source(s) 
#' of x with the source(s) of y), "merge" (which combines the sources of x and y in a list), or a new source 
#' can be entered here as a Bibtex object. By default, "keep" if no y argument, or "merge" if y is provided.
#' @param calcHistory A tree-like object of class Node indicating the functions through which x has passed. 
#' Possible arguments are "keep", "copy", "clear", "merge" (which combines the history trees of 2 or more 
#' objects), and "update" (which adds the function presently calling updateMetadata (or a function further 
#' upstream if specified by n) to calcHistory and also merges if y is provided). A node object can also be 
#' provided which will overwrite any existing value. Finally, if a character of length one is provided, the
#' behavior will be like "update" using the string as the new root node. By default, "keep" if no y argument,
#' or "merge" if y is provided.
#' @param date A character indicating the MAgPIE object's last modified date. Possible arguments are 
#' "keep", "copy", and "update" (which sets the date of x to the current time). "update" by default.
#' @param user A string indicating the user who last modified the MAgPIE object. Possible arguments are "keep",
#' "copy", "update" (which retrieves the username currently logged into the system), or a character string 
#' which specifies a new user. "update" by default.
#' @param description A string or list of strings containing a description of the dataset. Possible arguments are 
#' "keep", "copy", "merge", "clear", or a new description can be defined here by a character string. By default, 
#' "keep" if no y argument, or "copy" if y is provided.
#' @param note A string or list of strings for attaching notes (e.g. instructions, warnings, etc.) to the data.
#' Possible arguments are "keep", "copy", "merge", clear", or a new note can be entered here as a character string. 
#' By default, "keep" if no y argument, or "copy" if y is provided.
#' @param version A named vector containing the name(s) and version number(s) of the software used. Possible 
#' arguments are "keep" (default), "copy", "merge", "clear", or a character vector (package names and numbers 
#' can be provided as a named vector, in concatenated strings with a space separating name & number, or in a 
#' single string with a ';' separating each package).
#' @param n If calcHistory is to be updated, this integer indicates how many frames ahead in the stack to 
#' find the function to append to the the object's calcHistory. n=1 by default.
#' @param cH_priority Integer to set the significance of the function call with respect to calcHistory tracking 
#' (lower = more significant). To be compared against the "calcHistory_verbosity" global option (user can set this 
#' via withMetadata).
#' @return updateMetadata returns the magpie object x with metadata modified as desired.
#' @author Stephen Bi
#' @seealso \code{\link{getComment}}, \code{\link{getMetadata}}, \code{\link{getNames}},
#' \code{\link{getYears}}, \code{\link{getCPR}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}}, \code{"\linkS4class{magpie}"}
#' @export
#' @importFrom methods getPackageName
#' 
updateMetadata <- function(x, y=NULL, unit=ifelse(is.null(y),"keep","update"), source=ifelse(is.null(y),"keep","merge"), 
                           calcHistory=ifelse(is.null(y),"keep","update"), user="update", date="update", description=ifelse(is.null(y),"keep","merge"), 
                           note=ifelse(is.null(y),"keep","merge"), version=ifelse(is.null(y),"keep","merge"), n=1, cH_priority=2) {

  if(!withMetadata()) return(x)
  if (!requireNamespace("data.tree", quietly = TRUE)) stop("The package data.tree is required for metadata handling!")
  units::units_options(auto_convert_names_to_symbols=FALSE, allow_mixed=FALSE, negative_power=TRUE, set_units_mode="standard")
  if (is.null(cH_priority))  cH_priority <- 2
  if (cH_priority > getOption("calcHistory_verbosity")) {
    if(is.character(calcHistory) && calcHistory=="update")  calcHistory <- "merge"
  }

  #Function nodeClone clones a node object and, if necessary, prepares it for merging and attaches it to the new root
  nodeClone <- function(x,fn=NULL){
    if (is.null(x))  return(fn)
    xc <- data.tree::Clone(x)
    if (!is.null(fn)) {
      if (!is(fn,"Node")) {
        fn <- data.tree::Node$new(fn)
      }
      if (xc$name=="ROOT"){
        for (i in 1:xc$count){
          fn$AddChildNode(xc$children[[i]])
        }
      }else  fn$AddChildNode(xc)
    }else  return(xc)
    return(fn)
  }
  #Function newCall creates the appropriate function call to be displayed for the new root
  newCall <- function(n,convert=TRUE) {
    if (!is.na(as.character(sys.call(-n))[1]) & !is.null(sys.call(-n))) {
      fname <- as.character(sys.call(-n))[1]
      if (fname %in% c("/","*","+","-","^","%%","%/%")) {
        if (convert==TRUE)  return(data.tree::Node$new(trimws(deparse(sys.call(-n),width.cutoff=500))))
        else  return(trimws(deparse(sys.call(-n),width.cutoff=500)))
      }else if (length(sys.calls())>(n+1) && as.character(sys.call(-n-1))[1]==fname)  return(NULL)
      else if (fname=="mcalc") {
        if (convert==TRUE)  return(data.tree::Node$new(paste0(fname,"(",as.character(sys.call(-n))[3],")")))
        else  return(paste0(fname,"(",as.character(sys.call(-n))[3],")"))
      }
      
      func <- trimws(deparse(sys.call(-n),width.cutoff = 500))
      if (length(func)>1)  func <- func[1]
      if (grepl(":::",func,fixed=TRUE))  func <- unlist(strsplit(func,":::",fixed=TRUE))[2]
      if (grepl("::",func,fixed=TRUE))  func <- unlist(strsplit(func,"::",fixed=TRUE))[2]
      tmp <- unlist(strsplit(func,"(",fixed=TRUE))
      if (length(tmp[-1])>1) {
        tmp[2] <- paste(tmp[-1],collapse="(")
      }
      tmp <- gsub(".{1}$","",tmp[2])
      arg <- trimws(unlist(strsplit(tmp,",",fixed=TRUE)))
      fchanged <- NULL
      for(i in 1:length(arg)) {
        fchanged[i] <- FALSE
        if (grepl("[",arg[i],fixed=TRUE)) {
          k <- i
          while (!grepl("]",arg[k],fixed=TRUE)) {
            arg[i] <- paste0(arg[i],",",arg[k])
            arg <- arg[-k]
            if (k>=length(arg))  break
            else  k <- k+1
          }
        }
        if (grepl("(",arg[i],fixed=TRUE)) {
          j <- i
          while (!grepl(")",arg[j],fixed=TRUE)) {
            if (grepl("(",arg[j],fixed=TRUE)) {
              if (j>i | length(regmatches(arg[j],gregexpr("(",arg[j],fixed=TRUE)))>1) {
                while (!grepl(")",arg[j],fixed=TRUE)) {
                  arg[i] <- paste0(arg[i],",",arg[j])
                  arg <- arg[-j]
                  if (j>=length(arg))  break
                  else  j <- j+1
                }
              }
            }
            if (j>=length(arg))  break
            else  j <- j+1
            arg[i] <- paste0(arg[i],",",arg[j])
            arg <- arg[-j]
          }
        }
        if(grepl("=",arg[i],fixed=TRUE)) {
          tmp <- trimws(unlist(strsplit(arg[i],"=",fixed=TRUE)))
          if (length(tmp)>2) {
            #INSPECT THIS PART!!!!
            #if (grepl("(",substr(arg[i],1,1),fixed=TRUE)) {
            #  if (grepl(")",substr(arg[i],nchar(arg[i]),nchar(arg[i])),fixed=TRUE)) {
            #    tmp <- gsub("(","",gsub(")","",tmp,fixed=TRUE),fixed=TRUE)
            #    if (grepl(",",tmp,fixed=TRUE)) {
            #      tmp <- tmp[which(grepl("=",trimws(unlist(strsplit(tmp,",",fixed=TRUE))),fixed=TRUE))]
            #    }
            #  }
            #}else 
            if (grepl("(",arg[i],fixed=TRUE)) {
              tmp[2] <- paste(tmp[-1],collapse=", ")
              #tmp[2] <- newCall(n+1,convert=FALSE)
            }
          }
        }else {
          tmp <- c(NA,arg[i])
        }
        pretmp <- NULL
        if (grepl("(",tmp[2],fixed=TRUE) & !grepl("c(",substr(tmp[2],1,2),fixed=TRUE) & !grepl("list(",tmp[2],fixed=TRUE)) {
          pretmp <- try(eval.parent(parse(text=tmp[2]),n=n+1),silent=TRUE)
          if (is(pretmp,"try-error"))  pretmp <- NULL
        }else if(!grepl("\u0022",tmp[2]) & grepl("[[:alpha:]]",tmp[2])) {
          if (!any(tmp[2]==c("T","F","TRUE","FALSE","NULL")) || grepl("[[:punct:]]",tmp[2])) {
            pretmp <- get0(tmp[2],envir=parent.frame(n+1))
          }
        }
        if (!is.null(pretmp) && !(class(pretmp) %in% c("matrix","magpie","array","data.frame"))) {
          if (length(pretmp)>1)  tmp[2] <- paste0("c(",paste(pretmp,collapse=", "),")")
          else if (is.null(pretmp))  tmp[2] <- "NULL"
          else  tmp[2] <- pretmp
          fchanged[i] <- TRUE
        }
        if (is.null(tmp[2]))  tmp[2] <- "NULL"
        else if (is.na(tmp[2]))  tmp[2] <- "NA"
        else if (is.nan(tmp[2]))  tmp[2] <- "NaN"
        if (isTRUE(fchanged[i])) {
          if ((!is.na(tmp[1]) & grepl("\u0022",tmp[2])) || is.numeric(tmp[2]) || !grepl("[[:alpha:]]",tmp[2]) || tmp[2] %in% c("NULL","NA","NaN","FALSE","TRUE","T","F")) {
            arg[i] <- paste0(tmp[1],"=",tmp[2])
          }else if (!is.na(tmp[1])) {
            arg[i] <- paste0(tmp[1],"=\"",tmp[2],"\"")
          }else {
            arg[i] <- tmp[2]
          }
        }
      }
      if(any(fchanged==TRUE))  func <- paste0(fname,"(",paste(arg,collapse=", "),")")
      
      if (convert==TRUE)  return(data.tree::Node$new(func))
      else  return(func)
    }else  stop("n argument is out of range! calcHistory cannot be updated!")
  }
  
  #Function buildTree constructs the calcHistory data tree 
  buildTree <- function(x,y=NULL,n,cH){
    if (length(sys.calls()) < n)  cH <- "merge"
    if (cH=="update"){
      rootNode <- newCall(n)
      if (is.null(rootNode)) {
        cH <- "merge"
      }else if (is.null(y)) {
        if (is(x,"Node")){
          if (x$name=="ROOT"){
            x$name <- newCall(n,convert=FALSE)
            return(nodeClone(x))
          }else  return(nodeClone(x,rootNode))
        }else  return(rootNode)
      }
    }
    if (cH=="merge"){
      rootNode <- data.tree::Node$new("ROOT")
      if (is.null(y)){
        if (is(x,"Node"))  return(nodeClone(x))
        else  return(NULL)
      }
    }
    if (is.list(y)){
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
        else if (is.null(rootNode) || rootNode$name=="ROOT")  return(NULL)
      }else if (j==1){
        if (is(x,"Node")){
          nodeClone(x,rootNode)
          nodeClone(mY[[1]],rootNode)
        }else  return(mY[[1]])
      }else{
        nodeClone(x,rootNode)
        for (ii in 1:j)  nodeClone(mY[[ii]],rootNode)
      }
    }else if (is.magpie(y)){
      mY <- getMetadata(y,"calcHistory")
      if (is(x,"Node")){
        if (is(mY,"Node")) {
          nodeClone(x,rootNode)
          nodeClone(mY,rootNode)
        }else  return(nodeClone(x,rootNode))
      }else if (is(mY,"Node"))  return(nodeClone(mY,rootNode))
      else if (is.null(rootNode) || rootNode$name=="ROOT")  return(NULL)
    }
    return(rootNode)
  }
  
  #Function for merging metadata from 2 or more objects
  mergeFields <- function(x,y) {
    if (!is.null(x)) {
      if (!is.null(y)) {
        if(!is.list(x)) {
          if (is.list(y))  x <- append(list(x),y)
          else  x <- list(x,y)
        }else {
          if (is.list(y))  x <- append(x,y)
          else  x <- append(x,list(y))
        }
      }
    }else if (!is.null(y))  return(y)
    return(x)
  }
  
  #Run updateMetadata recursively for each magpie object in y
  if (is.list(y)) {
    if (length(y) > 1) {
      for (i in 1:(length(y)-1)) {
        if (is.magpie(y[[i]])) {
          x <- updateMetadata(x, y[[i]], unit, source, calcHistory="keep", user, date, description, note, version, n=n+1, cH_priority)
        }
      }
    }
    My <- getMetadata(y[[length(y)]])
  }else if (is.magpie(y)) {
    My <- getMetadata(y)
  }else if (!is.null(y)){
    y <- NULL
    warning("y argument must be a magpie object or a list of magpie objects!")
  }
  Mx <- getMetadata(x)
  #Change default arguments in case either x or y is NULL
  if (!is.null(y)) {
    if (is.null(Mx)) {
      if (is.character(unit))  unit <- "copy"
      if (is.character(source))  source <- "copy"
      if (is.character(calcHistory))  if (calcHistory=="merge")  calcHistory <- "copy"
      if (description=="merge")  description <- "copy"
      if (version=="merge")  version <- "copy"
    }else if (is.null(My)) {
      if (is.character(unit))  unit <- "keep"
      if (is.character(source))  source <- "keep"
      if (is.character(calcHistory))  if (calcHistory=="merge")  calcHistory <- "keep"
      if (description=="merge")  description <- "keep"
      if (version=="merge")  version <- "keep"
    }
  }
  
  if (is.null(unit))  unit <- "keep"
  if (is(unit,"units")) { #| is(unit,"mixed_units")
    Mx$unit <- unit
  }else if (unit=="copy") {
    if (!is.null(y))  Mx$unit <- My$unit
    else  warning("Units cannot be copied without a second magpie argument provided!")
  }else if (unit=="clear") {
    Mx$unit <- NULL
  }else if (unit=="update") {
    if (is.null(Mx$unit)) {
      Mx$unit <- My$unit
    }else if (!is.null(My$unit)) {
      Mx$unit <- list(Mx$unit,My$unit)
    }
  }else if (unit!="keep")  Mx$unit <- unit
  
  if (is.null(source))  source <- "keep"
  if (is.list(source) | is(source,"Bibtex") | is(source,"bibentry")) {
    Mx$source <- source
  }else if (source=="merge"){
    if (!is.null(y)){
      Mx$source <- mergeFields(Mx$source,My$source)
    }else  warning("Source cannot be merged without a second magpie argument provided!")
  }else if (source=="copy"){
    if (!is.null(y)){
      if (!is.null(My$source)){
        getMetadata(x,"source") <- NULL
        Mx$source <- My$source
      }
    }else  warning("Source cannot be copied without a second magpie argument provided!")
  }else if (source=="update") {
    warning("Update is an invalid argument for source! Please specify keep, copy, or clear.")
  }else if (source=="clear") {
    Mx$source <- NULL
  }else if (source!="keep")  Mx$source <- source
  
  if (is.null(calcHistory) || is(calcHistory,"Node")) {
    getMetadata(x,"calcHistory") <- "delete"
    Mx$calcHistory <- calcHistory
  }else if (calcHistory=="update" | calcHistory=="merge") {
    Mx$calcHistory <- buildTree(Mx$calcHistory,y,n+2,calcHistory)
    getMetadata(x,"calcHistory") <- "delete"
  }else if (calcHistory=="copy") {
    if (!is.null(y)){
      if (!is.null(My$calcHistory) && (!is(My$calcHistory, "Node")))  warning("Attempting to copy a calcHistory which is not a Node object!")
      else if (is(My$calcHistory,"Node")) {
        Mx$calcHistory <- data.tree::Clone(My$calcHistory)
        getMetadata(x,"calcHistory") <- "delete"
      }else {
        getMetadata(x,"calcHistory") <- "delete"
        Mx$calcHistory <- My$calcHistory
      }
    }else  warning("calcHistory cannot be copied without a second magpie argument provided!")
  }else if (calcHistory=="clear") {
    warning("calcHistory cannot be cleared! Please specify keep, update, or copy.")
  }else if (calcHistory=="keep") {
    Mx$calcHistory <- NULL
  }else if (is.character(calcHistory) & length(calcHistory)==1) {
    if (is.null(y) || is.null(My$calcHistory)) {
      Mx$calcHistory <- calcHistory
    }else if (is.null(Mx$calcHistory)) {
      Mx$calcHistory <- nodeClone(My$calcHistory,calcHistory)
    }else {
      Mx$calcHistory <- nodeClone(Mx$calcHistory,calcHistory)
      Mx$calcHistory <- nodeClone(My$calcHistory,Mx$calcHistory)
    }
  }else  warning("Invalid argument ",calcHistory," for calcHistory!")
  
  if (is.null(user))  user <- "keep"
  if (user=="update"){
    env <- if(.Platform$OS.type == "windows") "USERNAME" else "USER"
    Mx$user <- Sys.getenv(env)
  }else if (user=="copy"){
    if (!is.null(y)){
      if (!is.null(My$user))  Mx$user <- My$user
      else  warning("Attempting to copy a NULL user!")
    }else  warning("User cannot be copied without a second magpie argument provided!")
  }else if (user=="clear") {
    Mx$user <- NULL
  }else if (user!="keep"){
    if (is.character(user) & length(user)==1)  Mx$user <- user
    else  warning("Invalid argument ",user," for user!")
  }
  
  if (is.null(date))  date <- "update"
  if (date=="update") {
    Mx$date <- as.character(Sys.time())
  }else if (date=="copy"){
    if (!is.null(y)){
      if (!is.null(My$date))  Mx$date <- My$date
      else  warning("Attempting to copy a NULL date!")
    }else  warning("date cannot be copied without a second magpie argument provided!")
  }else if (date=="clear") {
    warning("date cannot be cleared! Please specify keep, copy, or update.")
  }else if (date!="keep")  warning("Invalid argument ", date," for date!")
    
  if (is.null(description))  Mx$description <- "keep"
  if (length(description)>1){
    if (is.character(description) | is.list(description))  Mx$description <- description
    else  warning("Invalid argument for description!")
  }else if (description=="copy"){
    if (!is.null(y))  Mx$description <- My$description
    else  warning("Description cannot be copied without a second magpie argument provided!")
  }else if (description=="clear") {
    Mx$description <- NULL
  }else if (description=="update") {
    warning("Update is an invalid argument for description! Please specify keep, copy, merge, or clear.")
  }else if (description=="merge"){
    if (!is.null(y))  Mx$description <- mergeFields(Mx$description,My$description)
    else  warning("description cannot be merged without a second magpie argument provided!")
  }else if (description!="keep"){
    if (is.character(description))  Mx$description <- description
    else  warning("Invalid argument ",description," for description!")
  }
  if (is.null(note))  note <- "keep"
  if (length(note)>1){
    if (is.character(note) | is.list(note))  Mx$note <- note
    else  warning("Invalid argument for note!")
  }else if (note=="copy"){
    if (!is.null(y))  Mx$note <- My$note
    else  warning("note cannot be copied without a second magpie argument provided!")
  }else if (note=="clear") {
    Mx$note <- NULL
  }else if (note=="merge"){
    if (!is.null(y))  Mx$note <- mergeFields(Mx$note,My$note)
    else  warning("note cannot be merged without a second magpie argument provided!")
  }else if (note=="update") {
    warning("Update is an invalid argument for note! Please specify keep, copy, merge, or clear.")
  }else if (note!="keep"){
    if (is.character(note))  Mx$note <- note
    else  warning("Invalid argument ",note," for note!")
  }
  if (is.null(version))  version <- "clear"
  if (length(version)>1) {
    Mx$version <- version
  }else if (version=="copy") {
    if (!is.null(y)) {
      getMetadata(x,"version") <- NULL
      Mx$version <- My$version
    }else  warning("version cannot be copied without a second magpie argument provided!")
  }else if (version=="clear") {
    Mx$version <- NULL
  }else if (version=="merge") {
    Mx$version <- My$version
  }else if (version=="update") {
    warning("update is an invalid argument for version!")
  }else if (version!="keep") {
    Mx$version <- version
  }
  
  getMetadata(x) <- Mx
  return(x)
}