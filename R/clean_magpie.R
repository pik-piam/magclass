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
    
    names <- names(dimnames(x))
    if(!is.na(names[1]) & (names[1]!="") & (names[1]!="NA")) {
      c1 <- .count_subdim(dimnames(x)[[1]][1])
      c2 <- .count_subdim(names[1])
      if(c1!=c2) {
        if(c1>2) stop("More than 2 spatial subdimensions not yet implemented")
        names[1] <- paste(names[1],"cell",sep=".")
      }
    } else {
      names[1] <- ifelse(all(grepl("\\.",dimnames(x)[[1]])),"region.cell","region") 
    }
    if(is.na(names[2]) | names[2]=="NA" | names[2]=="") {
      names[2] <- "year"
    }
    if(is.na(names[3]) | names[3]=="" | names[3]=="NA") {
      ndim <- nchar(gsub("[^\\.]","",getNames(x)[1])) +1
      names[3] <- ifelse(length(ndim)>0,paste0("data",1:ndim,collapse="."),"data1")
    } else {
      c1 <- .count_subdim(dimnames(x)[[3]][1])
      c2 <- .count_subdim(names[3])
      if(c1!=c2) {
        if(c1>c2) {
          names[3] <- paste(c(names[3],rep("data",c1-c2)),collapse=".")
        } else {
          search <- paste0(c(rep("\\.[^\\.]*",c2-c1),"$"),collapse="")
          names[3] <- sub(search,"",names[3])
        }
        names[3] <- paste0(make.unique(strsplit(names[3],"\\.")[[1]],sep = ""),collapse=".")

      } 
    }
    
    names(dimnames(x)) <- names
  }  
  return(x)
}
