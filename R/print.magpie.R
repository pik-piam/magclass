#' print
#' 
#' print method for MAgPIE objects for conventient display of magpie data.
#' 
#' 
#' @param x MAgPIE object
#' @param drop argument which controls whether empty dimensions should be
#' skipped or not.
#' @param ... arguments to be passed to or from other methods.
#' @return print displays the given MAgPIE object on screen.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link[base]{print}}
#' @examples
#' 
#'   data(population_magpie)
#'   print(population_magpie)
#'   print(population_magpie[,1,], drop=FALSE)
#'   print(population_magpie[,1,])
#' 
#' @export
print.magpie <- function(x, drop=TRUE, ...) {
    p <- print(as.array(x)[,,,drop=drop], ...)
    
    unit <- getMetadata(x,"unit")
    if(!is.null(unit)) {
      factor <- ifelse(as.numeric(unit)!=1,paste0(as.character(unit),"*"),"")
      cat("Unit: ",factor,as.character(attr(unit, "units")),"\n", sep="")
    }
}
