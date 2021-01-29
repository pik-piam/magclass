#' sizeCheck
#' 
#' Calculates expected magclass object length and checks that it stays below
#' the limit defined with magclass_sizeLimit. This is useful to prevent
#' out of memory errors in case of unwanted object expansions
#' Ignored if \code{getOption("magclass_sizeLimit")} is negative.
#' 
#' 
#' @param dim dimensions of the current object as returned by function \code{dim}
#' @param newnames a list of new dimensions to be added to the object
#' @author Jan Philipp Dietrich
#' @examples
#' 
#' pop <- maxample("pop")
#' magclass:::sizeCheck(dim(pop),dimnames(pop))
#' 
sizeCheck <- function(dim, newnames=NULL) {
  if(is.null(getOption("magclass_sizeLimit"))) options(magclass_sizeLimit=10^8)
  # estimate new object size and check against size limit
  if(!is.null(getOption("magclass_sizeLimit")) && getOption("magclass_sizeLimit")>0) {
    if(is.null(newnames)) {
      add <- 1
    } else {
      add <- prod(sapply(newnames,length))
    }
    size <- add*prod(dim)
    if(size > getOption("magclass_sizeLimit")) {
      head2 <- function(x,length=3) {
        if(length(x)<=length) return(x)
        return(c(head(x,3),"..."))
      }
      if(is.null(newnames)) {
        stop("magclass object size limit reached! getOption(\"magclass_sizeLimit\")=",getOption("magclass_sizeLimit"))
      } else {
        head_newnames <- lapply(newnames,head2)
        stop("magclass object size limit reached. Elements to add: ", paste(names(head_newnames),head_newnames, sep=" = ", collapse=", "))
      }
    }
  }
}


