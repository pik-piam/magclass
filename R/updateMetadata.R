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
#' @param unit A vector indicating the units of measure of the MAgPIE data. TRUE argument copies the entry 
#' from x, FALSE deletes the field, or new units can be set with a vector. FALSE by default.
#' @param source A list indicating the source(s) of the MAgPIE data. TRUE = copy from x. TRUE by default.
#' @param creationDate A character indicating the MAgPIE object's creation date. TRUE = update to 
#' current date, FALSE = copy from x. FALSE by default.
#' @param user A string indicating the user who created the MAgPIE object. TRUE = set to the current 
#' user, FALSE = copy from x, or a desired user can be set with a character. FALSE by default.
#' @param calcHistory A vector indicating the functions through which x has passed. If '...' is not
#' empty, it becomes a list of vectors for each of the datasets. TRUE by default.
#' @param description A character string containing a description of the dataset. TRUE = copy from x,
#' FALSE = delete, or a new description can be set with a character argument. FALSE by default.
#' @return getMetadata returns the metadata attached to a MAgPIE-object, NULL if
#' no metadata attribute is present. getMetadata<- returns the magpie object with the
#' modified metadata.
#' @author Stephen Bi
#' @seealso \code{\link{getComment}}, \code{\link{getRegions}}, \code{\link{getNames}},
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

updateMetadata <- function(x, y=NULL, unit="copy", source="copy", calcHistory="update", user="update", creationDate="update", description="copy"){
  Mx <- getMetadata(x)
  My <- getMetadata(y)
  
  if (unit=="copy" & !is.null(Mx$unit))  getMetadata(x, "unit") <- My$unit
  else if (unit=="delete" || is.null(Mx$unit))  getMetadata(x, "unit") <- "no units specified"
  else if (is.character(unit))  getMetadata(x, "unit") <- unit
  
  if (user=="update"){
    env <- if(.Platform$OS.type == "windows") "USERNAME" else "USER"
    getMetadata(x,"user") <- Sys.getenv(env)
  }else if (user=="copy")  getMetadata(x, "user") <- My$user
  else if (is.character(user))  getMetadata(x, "user") <- user
  
  if (creationDate=="update")  getMetadata(x, "creationDate") <- as.character(Sys.Date())
  else  getMetadata(x, "creationDate") <- Mx$creationDate
  
  if (description=="copy")  getMetadata(x, "description") <- Mx$description
  else if (is.character(description))  getMetadata(x, "description") <- description
  
  if (is.null(y)){
    if (source=="copy" & !is.null(Mx$source))  getMetadata(x, "source") <- Mx$source
    
    if (calcHistory=="update")  getMetadata(x, "calcHistory") <- c(Mx[["calcHistory"]], sys.call(-1))
    
  }else {
    if (calcHistory=="update"){
      getMetadata(x, "calcHistory") <- list(c(Mx$calcHistory, sys.call(-1)), c(My$calcHistory, sys.call(-1)))
    }
    if (source=="copy"){
      if(!is.null(Mx$source)) getMetadata(x, "source") <- if(is.null(My$source)) Mx$source else list(Mx$source, My$source)
      }else if (!is.null(My$source)) getMetadata(x, "source") <- My$source
  }
  return(x)
}