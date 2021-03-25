#' @title magpply
#' @description apply command for magpieobjects. Very efficient for replacing loops.
#'
#' @param X magpie object
#' @param FUN function that shall be applied X
#' @param MARGIN dimension over which FUN shall be applied (like a loop over that dimension). This dimension will be preserved in the output object
#' @param ... further parameters passed on to FUN
#' @param integrate if TRUE, the output will be filled into an magpie object of the same dimensionality as X
#'
#' @return magpie object
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' pop <- maxample("pop")
#' magpply(pop,FUN=sum,MARGIN=2)
#' fourdim <- pop * setNames(pop, c("jkk","lk"))
#' magpply(fourdim, FUN=sum, MARGIN=c(1,3.1))
#' 
#' @export magpply

magpply<-function(X,FUN,MARGIN,...,integrate=FALSE){
  if(any(MARGIN>3)){
    for(counter in which(MARGIN>3)){
      MARGIN[counter]=old_dim_convention(MARGIN[counter])
    }
    X<-unwrap(X)
  }
  
  out<-apply(X = X,FUN = FUN,MARGIN=MARGIN)
  if(integrate==TRUE){
    X[,,]<-out
    out<-X
  } else {
    out<-as.magpie(out)
  }
  out <- updateMetadata(out,X,unit="copy",source="copy",calcHistory="copy",description="copy")
  return(out)
}