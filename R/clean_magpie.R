#' MAgPIE-Clean
#' 
#' Function cleans MAgPIE objects so that they follow some extended magpie
#' object rules (currently it makes sure that the dimnames have names and
#' removes cell numbers if it is purely regional data)
#' 
#' 
#' @param x MAgPIE object which should be cleaned.
#' @param what term defining what type of cleaning should be performed. Current
#' modes are "cells" (removes cell numbers if the data seems to be regional -
#' this should be used carefully as it might remove cell numbers in some cases
#' in which they should not be removed), "sets" (making sure that all
#' dimensions have names) and "all" (performing all available cleaning methods)
#' @return The eventually corrected MAgPIE object
#' @author Jan Philipp Dietrich
#' @seealso \code{"\linkS4class{magpie}"}
#' @examples
#' 
#'  data(population_magpie)
#'  a <- clean_magpie(population_magpie)
#' 
#' @export clean_magpie
clean_magpie <- function(x,what="all") {
  if(!(what %in% c("all","cells","sets"))) stop('Unknown setting for argument what ("',what,'")!')
  #remove cell numbers if data is actually regional
  if(what=="all" | what =="cells") {
    if(ncells(x)==nregions(x)) {
      getCells(x) <- getRegions(x)
      if(!is.null(names(dimnames(x))[[1]])) {
        if(!is.na(names(dimnames(x))[[1]])) {
          names(dimnames(x))[[1]] <- sub("\\..*$","",names(dimnames(x))[[1]])
        }
      }
    }
  }
  #make sure that all dimensions have names
  if(what=="all" | what =="sets") {
    
    if(is.null(names(dimnames(x)))) names(dimnames(x)) <- rep(NA,3)
    
    .count_subdim <- function(x,sep="\\.") {
      o <- nchar(gsub(paste0("[^",sep,"]*"),"",x))+1
      if(length(o)==0) o <- 0
      return(o)
    }
    
    .fix_names <- function(names,ndim,key="data") {
      if(is.na(names) || names=="" || names=="NA") {
        tmp <- rep(key,max(1,ndim))
        names <- paste(make.unique(tmp, sep = ""),collapse =".")
      } else {
        cdim <- .count_subdim(names)
        if(ndim!=cdim) {
          if(ndim>cdim) {
            names <- paste(c(names,rep(key,ndim-cdim)),collapse=".")
          } else {
            search <- paste0(c(rep("\\.[^\\.]*",cdim-ndim),"$"),collapse="")
            names <- sub(search,"",names)
          }
          names <- paste0(make.unique(strsplit(names,"\\.")[[1]],sep = ""),collapse=".")
        } 
      }
      return(names)
    }
    
    names <- names(dimnames(x))
    keys <- c("region","year","data")
    for(i in 1:3) {
      names[i] <- .fix_names(names[i],ndim=.count_subdim(dimnames(x)[[i]][1]),key=keys[i])
    }
    names(dimnames(x)) <- names
  }  
  
  return(updateMetadata(x))
}
