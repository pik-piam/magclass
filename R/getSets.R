#' Get sets
#' 
#' Extracts sets of a MAgPIE-object if available
#' 
#' 
#' @aliases getSets getSets<-
#' @param x MAgPIE object
#' @param sep A character separating joined dimension names
#' @param fulldim bool: Consider dimension 3 as a possible aggregate of more
#' dimensions (TRUE) or stick to it as one dimension (FALSE)
#' @param value A vector with set names you want to replace the current set
#' names of the object with.
#' @return Sets of the MAgPIE-object. If no information about contained sets is
#' available NULL
#' @author Markus Bonsch
#' @seealso \code{\link{getRegions}},
#' \code{\link{getNames}},\code{\link{getYears}}, \code{\link{getCPR}},
#' \code{\link{read.magpie}}, \code{\link{write.magpie}},
#' \code{"\linkS4class{magpie}"}
#' @examples
#' 
#'  a <- new.magpie("GLO.1",2000,c("a.o1","b.o1","a.o2"))
#'  getSets(a) <- c("reg","cell","t","bla","blub")
#'  getSets(a)
#'  
#'  getSets(a)[4] <- "BLA"
#'  getSets(a,fulldim=FALSE)
#'  getSets(a)
#' 
#' @export
getSets <- function(x,fulldim=TRUE,sep=".") {
  out <- names(dimnames(x))[drop=FALSE]
  if(is.null(out)) return(NULL)
  
  if(fulldim==TRUE){
    tmp<- strsplit(out,split=sep,fixed=TRUE)
    tmp<- lapply(tmp,FUN=function(x){
                            if(length(x)==0) x<-NA
                            return(x)
                          }
    )
    out <- as.vector(unlist(tmp))
  }
  return(out)
}



#' @describeIn getSets replace set names
#' @export
"getSets<-" <- function(x,fulldim=TRUE,sep=".",value) {
   x <- clean_magpie(x,what="sets")
   if(is.null(value)) return(x)
   if(length(names(dimnames(x)))==0) fulldim <- FALSE  
   if(length(value)==3) fulldim <- FALSE
   if(length(value)==0) fulldim <- FALSE
   if(!fulldim) {
     names(dimnames(x)) <- value
     return(x)
   } else {
     s1 <- getSets(x,fulldim=FALSE)
     s2 <- getSets(x,fulldim=TRUE)
     search_s2 <- paste0("(^|\\.)",s2,"(\\.|$)")
     where <- sapply(search_s2,grep,s1)
     if(is.list(where)) {
       #duplicates found
       j <- 1
       for(i in 1:length(where)) {
         if(length(where[[i]])>1) {
           where[[i]] <- where[[i]][j]
           j <- j+1
         }
       }
       where <- unlist(where)
     }
     names(where) <- s2
     
     if(length(value)!=length(s2)) stop("Input length does not agree with the number of sets in x!")
     
     for(i in 1:3) {
      s1[i] <- paste(value[where==i],collapse=sep)
     }
     getSets(x,fulldim=FALSE,sep=sep) <- s1
     return(x)
   }  
}
