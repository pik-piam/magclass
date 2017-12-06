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
#' - string or vector specifying new units for x
#' The default argument is "keep". 
#' @param source A list indicating the source(s) of the MAgPIE data. Possible arguments are "keep", "copy",
#' "clear", and "update", which merges the source(s) of y with the source(s) of x. "keep" by default.
#' @param calcHistory A vector indicating the functions through which x has passed. Possible arguments are
#' "keep", "copy", "clear", and "update", which adds the function presently calling updateMetadata to 
#' calcHistory and, if y is provided, also merges the calcHistories of x and y. "update" by default.
#' @param creationDate A character indicating the MAgPIE object's creation date. Possible arguments are 
#' "keep", "copy", and "update", which sets the creationDate of x to the current time. "update" by default.
#' @param user A string indicating the user who created the MAgPIE object. Possible arguments are "keep",
#' "copy", "update", which retrieves the username currently logged into the system, or a character string 
#' which specifies a new user. "update" by default.
#' @param description A character string containing a description of the dataset. Possible arguments are 
#' "keep", "copy", "clear", or a character string which sets a new description. "keep" by default.
#' @return updateMetadata returns the magpie object x with metadata modified as desired.
#' @author Stephen Bi
#' @seealso \code{\link{getComment}}, \code{\link{getMetadata}}, \code{\link{getNames}},
#' \code{\link{getYears}}, \code{\link{getCPR}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}}, \code{"\linkS4class{magpie}"}
#' @examples
#' 
#' x <- as.magpie(0)
#' y <- as.magpie(1)
#' getMetadata(y, "unit") = "km"
#' x <- updateMetadata(x, y, description = "bla")
#' getMetadata(x)
#' 
#' @export

updateMetadata <- function(x, y=NULL, unit="keep", source="keep", calcHistory="update", user="update", creationDate="update", description="keep"){
  Mx <- getMetadata(x)
  My <- getMetadata(y)
  
  if (user=="update"){
    env <- if(.Platform$OS.type == "windows") "USERNAME" else "USER"
    Mx$user <- Sys.getenv(env)
  }
  if (creationDate=="update")  Mx$creationDate <- as.character(Sys.time())
  
  if (is.null(y)){
    if (unit=="copy")  stop("Units cannot be copied without a second magpie argument provided!")
    else if (unit=="clear" || (unit=="keep" & is.null(Mx$unit)))  Mx$unit <- "no units specified!"
    else if (unit=="update")  stop("Invalid argument for unit!")
    else if (unit!="keep")  Mx$unit <- if(is.character(unit)) unit else stop("Invalid argument for unit!")
    
    if (source=="copy")  stop("Source cannot be copied without a second magpie argument provided!")
    else if (source=="update")  stop("Sources cannot be updated (merged) without a second magpie argument provided!")
    #Should source be deletable?
    else if (source=="clear" || (source=="keep" & is.null(Mx$source))) Mx$source <- "no source specified!"
    else if (source!="keep")  stop("Invalid argument for source!")
    
    if (calcHistory=="update")  Mx$calcHistory <- c(Mx$calcHistory, sys.call(-1))
    else if (calcHistory=="copy")  stop("calcHistory cannot be copied without a second magpie argument provided!")
    #Should calcHistory be deletable?
    else if (calcHistory=="clear") stop("Calculation history cannot be cleared! Please specify keep, update, or copy.")
    else if (calcHistory!="keep")  stop("Invalid argument for calcHistory!")
    
    if (user=="copy")  stop("User cannot be copied without a second magpie argument provided!")
    else if (user=="clear")  stop("User cannot be cleared! Please specify either a user or keep, update or copy.")
    else if (user=="keep" & is.null(Mx$user))  Mx$user <- "no user specified!"
    else if (user!="update" & user!="keep"){
      if (is.character(user) & length(user)==1)  Mx$user <- user
      else  stop("Invalid argument for user!")
    }
    
    if (creationDate=="copy")  stop("Creation Date cannot be copied without a second magpie argument provided!")
    else if (creationDate=="clear")  stop("Creation date cannot be cleared, only updated or copied!")
    else if (creationDate!="update" & creationDate!="keep")  stop("Invalid argument for creationDate!")
    
    if (description=="copy")  stop("Creation Date cannot be copied without a second magpie argument provided!")
    else if (description=="clear" || (description=="keep" & is.null(Mx$description)))  Mx$description <- "no description given!"
    else if (description=="update")  stop("Invalid argument for description!")
    else if (description!="keep") if(is.character(description)) Mx$description <- description else stop("Invalid argument for description!")
    
  }else if (!is.magpie(y))  stop("y argument must be a magpie object!")
    
  else if (length(y)==1){
    if (unit=="copy")  Mx$unit <- if(!is.null(My$unit)) My$unit else "no units specified!"
    else if (unit=="clear" || (unit=="keep" & is.null(Mx$unit)))  Mx$unit <- "no units specified!"
    else if (unit=="update")  stop("Invalid argument for unit!")
    else if (unit!="keep")  Mx$unit <- if(is.character(unit)) unit else stop("Invalid argument for unit!")
    
    if (source=="copy")  Mx$source <- if(!is.null(My$source)) My$source else "no source specified!"
    else if (source=="update" & !is.null(My$source)) Mx$source <- list(Mx$source, My$source)
    else if (source=="clear" || (source=="keep" & is.null(Mx$source)))  Mx$source <- "no source specified!"
    else if (source!="keep")  stop("Invalid argument for source!")
    
    if (calcHistory=="update"){
      if (!is.null(My$calcHistory)){
        if (!is.null(Mx$calcHistory))  Mx$calcHistory <- list(c(Mx$calcHistory, sys.call(-1)), c(My$calcHistory, sys.call(-1)))
        else Mx$calcHistory <- list(c(My$calcHistory, sys.call(-1)))
      }else Mx$calcHistory <- c(Mx$calcHistory, sys.call(-1))
    }else if (calcHistory=="copy"){
      if (!is.null(My$calcHistory)) Mx$calcHistory <- My$calcHistory else stop("Cannot copy calcHistory: no calcHistory found in y!")
    }else if (calcHistory=="clear") stop("Calculation history cannot be cleared! Please specify keep, update, or copy.")
    else if (calcHistory!="keep")  stop("Invalid argument for calcHistory!")
    
    if (user=="copy"){
      if (!is.null(My$user)) Mx$user <- My$user else stop("Cannot copy user: no user found in y!")
    }else if (user=="clear")  stop("User cannot be cleared! Please specify keep, update, copy, or a new user")
    else if (user=="keep" & is.null(Mx$user))  Mx$user <- "no user specified!"
    else if (user!="update" & user!="keep"){
      if (is.character(user) & length(user)==1)  Mx$user <- user
      else  stop("Invalid argument for user!")
    }
    
    if (creationDate=="copy"){
      if (!is.null(My$creationDate)) Mx$creationDate <- My$creationDate else stop("Cannot copy creationDate: no creationDate found in y!")
    }else if (creationDate=="clear")  stop("Creation date cannot be cleared, only updated or copied!")
    else if (creationDate!="update" & creationDate!="keep")  stop("Invalid argument for creationDate")

    if (description=="copy")  Mx$description <- if(!is.null(My$description)) My$description else "no description given!"
    else if (description=="clear" || (description=="keep" & is.null(Mx$description)))  Mx$description <- "no description given!"
    else if (description=="update")  stop("Invalid argument for description!")
    else if (description!="keep") if(is.character(description)) Mx$description <- description else stop("Invalid argument for description!")
  }
  getMetadata(x) <- Mx
  return(x)
}