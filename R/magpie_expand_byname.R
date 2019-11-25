#' magpie_expand_byname
#' 
#' Expands a MAgPIE object based on a reference
#' 
#' Expansion means here that the dimensions of x are expanded acordingly to
#' ref. Please note that this is really only about expansion. In the case that
#' one dimension of ref is smaller than of x nothing happens with this
#' dimension. At the moment magpie_expand is only internally available in the
#' magclass library
#' 
#' In contrast to \code{\link{magpie_expand}} this function is expanding purely
#' on the given set names of the object and only in a single dimension. It is meant
#' as a support function for \code{\link{magpie_expand}} itself.
#' 
#' @param x MAgPIE object that should be expanded
#' @param ref MAgPIE object that serves as a reference
#' @param dim dimension that should be expanded
#' @return An expanded version of x.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{as.magpie}}, \code{\link[base]{options}}
#' @examples
#'  d <- new.magpie(c("AFR.BLUB.1","AFR.BLUB.2","EUR.BLUB.1",
#'                     "AFR.BLA.1","AFR.BLA.2","EUR.BLA.1"),fill = 1)
#'  getSets(d)[1:3] <- c("reg","b","i")                  
#'  e <- new.magpie(c("BLA.AFR.A","BLA.EUR.A","BLUB.AFR.A","BLUB.EUR.A",
#'                     "BLA.AFR.B","BLA.EUR.B","BLUB.AFR.B","BLUB.EUR.B"),fill = 2)
#'  getSets(e)[1:3] <- c("b","reg","a")
#'  magpie_expand_byname(d,e,dim=1)

magpie_expand_byname <- function(x,ref,dim=1) {
  dimnames2df <- function(x,dim=1) {
    xd <- as.data.frame(t(as.data.frame(strsplit(dimnames(x)[[dim]],".",fixed=TRUE))))
    rownames(xd) <- NULL
    tmp <- strsplit(names(dimnames(x))[dim],".",fixed=TRUE)[[1]]
    if(length(tmp)==ncol(xd)) names(xd) <- tmp
    xd$".line" <- 1:nrow(xd)
    return(xd)
  }
  
  df2dimnames <- function(x) {
    cols <- grep("^\\.",names(x),invert=TRUE,value=TRUE)
    tmp <- lapply(x[cols],as.character)
    out <- do.call("paste", c(tmp, sep = "."))
    attr(out,"name") <- paste(sub("\\.","",cols),collapse=".")
    return(out)
  }
  
  dx <- dimnames2df(x,dim=dim)
  dref <- dimnames2df(ref,dim=dim)
  m <- merge(dx,dref,sort=FALSE, suffixes=c("_x","_ref"), by=setdiff(intersect(names(dx), names(dref)),".line"))
  if(dim==1) {
    out <- x[m$".line_x",,]
  } else if(dim==2) {
    out <- x[,m$".line_x",]
  } else if(dim==3) {
    out <- x[,,m$".line_x"]
  } else {
    stop("Unsupported dim setting (dim = ",dim,")")
  }
  tmp <- df2dimnames(m)
  dimnames(out)[[dim]] <- tmp
  names(dimnames(out))[dim] <- attr(tmp,"name")
  return(out)
}



