#' sizeCheck
#' 
#' Calculates expected magclass object length and checks that it stays below
#' the limit defined with magclass_sizeLimit. This is useful to prevent
#' out of memory errors in case of unwanted object expansions
#' Ignored if \code{getOption("magclass_sizeLimit")} is \code{NULL}.
#' 
#' 
#' @param dim dimensions of the current object as returned by function \code{dim}
#' @param newnames a list of new dimensions to be added to the object
#' @author Jan Philipp Dietrich
#' @examples
#' 
#' magclass:::sizeCheck(dim(population_magpie),dimnames(population_magpie))
#' 
sizeCheck <- function(dim, newnames) {
  # estimate new object size and check against size limit
  if(!is.null(getOption("magclass_sizeLimit"))) {
    size <- prod(sapply(newnames,length))*prod(dim)
    if(size > getOption("magclass_sizeLimit")) {
      head2 <- function(x,length=3) {
        if(length(x)<=length) return(x)
        return(c(head(x,3),"..."))
      }
      head_newnames <- lapply(newnames,head2)
      stop("magclass object size limit reached. Elements to add: ", paste(names(head_newnames),head_newnames, sep=" = ", collapse=", "))
    }
  }
}

options(magclass_sizeLimit=10^8)
