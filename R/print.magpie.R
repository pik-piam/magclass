#' print
#' 
#' print method for MAgPIE objects for conventient display of magpie data.
#' 
#' 
#' @param x MAgPIE object
#' @param drop argument which controls whether empty dimensions should be
#' skipped or not.
#' @param reshape agrument that controls tabular representation of 3rd data dimension cross tables
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
print.magpie <- function(x, drop=TRUE, reshape=FALSE, ...) {
  
  if(reshape){
    if(drop){
      if(all(fulldim(x)[[1]][1:2]==1) & length(fulldim(x)[[1]])==4){
        
        names1 <- getNames(x, dim=1)
        names2 <- getNames(x, dim=2)
        datadim  <- names(fulldim(x)[[2]])[3:4]
        
        p <- print(reshape(subset(as.data.frame(x, rev=2), select=c(datadim,".value")), 
                           timevar=datadim[1], idvar=datadim[2],
                           direction="wide",
                           new.row.names = names2,
                           varying = list(names1))[2:3])
        
      } else {
        warning("reshape option can just be used with 'fulldim(x)[[1]] == 1 1 n m'. Execute with reshape=FALSE:")
        print.magpie(x=x, drop=drop, reshape=FALSE, ...)
      }
      
    } else {
      warning("reshape option can just be used with drop=TRUE. Execute with reshape=FALSE:")
      print.magpie(x=x, drop=drop, reshape=FALSE, ...)
    }
    
  } else {
    
    p <- print(as.array(x)[,,,drop=drop], ...)
    
  }
  
  unit <- getMetadata(x,"unit")
  if(!is.null(unit)) {
    factor <- ifelse(as.numeric(unit)!=1,paste0(as.character(unit),"*"),"")
    cat("Unit: ",factor,as.character(attr(unit, "units")),"\n", sep="")
  }
}
