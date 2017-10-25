#' @title dimOrder
#' @description Changes the order of the 3rd dimension in a magpie object similar to unwrapping and applying the aperm command, but more efficient.
#' 
#' @param x magpie object
#' @param perm vector with the new order of the 3rd dimension
#'
#' @return magpie object
#' @author Benjamin Leon Bodirsky

#' @examples
#' 
#' \dontrun{ 
#' data("population_magpie")
#' x<-setNames(population_magpie,c("kj","kej"))*population_magpie
#' dimOrder(x=x,perm=c(2,1))
#' }
#' @export



dimOrder <- function(x,perm){
  dimensions=length(fulldim(x)[[1]])-2
  if (dimensions!=length(perm)) {stop("perm has to have the length of the 3rd dimensions")}
  if (!identical(levels(as.factor(1:dimensions)),levels(as.factor(perm)))) {stop("perm has to include all 3rd dimensions")}
  if (dimensions>1){
    pattern=paste0("^([^\\.]*)\\.",paste0(rep("([^\\.]*)\\.",dimensions-2),collapse = ""),"([^\\.]*)$")
    order=paste0(paste0("\\",perm),collapse=".")
    getNames(x) <- gsub(pattern = pattern,replacement=order,getNames(x))  
    getSets(x)[3:length(getSets(x))]<-getSets(x)[3:length(getSets(x))][perm]
  }
  return(x)
}
