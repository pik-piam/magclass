#' magpie_expand_dim
#' 
#' Expands a single MAgPIE object dimension
#' 
#' Expansion means here that the dimensions of x are expanded acordingly to
#' ref. Please note that this is really only about expansion. In the case that
#' one dimension of ref is smaller than of x nothing happens with this
#' dimension. At the moment magpie_expand is only internally available in the
#' magclass library
#' 
#' In contrast to \code{\link{magpie_expand}} this function is expanding only a single
#' dimension. It is meant as a support function for \code{\link{magpie_expand}} itself.
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
#'  magclass:::magpie_expand_dim(d,e,dim=1)

magpie_expand_dim <- function(x,ref,dim=1) {
  dimnames2df <- function(x,dim=1) {
    xd <- as.data.frame(t(as.data.frame(strsplit(dimnames(x)[[dim]],".",fixed=TRUE))))
    rownames(xd) <- NULL
    if(!is.null(names(dimnames(x)))) {
      tmp <- strsplit(names(dimnames(x))[dim],".",fixed=TRUE)[[1]]
      if(length(tmp)==ncol(xd)) names(xd) <- tmp
    }
    xd$".line" <- 1:nrow(xd)
    return(xd)
  }
  
  df2dimnames <- function(x) {
    cols <- grep("^\\.",names(x),invert=TRUE,value=TRUE)
    tmp <- lapply(x[cols],as.character)
    out <- do.call("paste", c(tmp, sep = "."))
    return(list(dimnames=out,name=paste(sub("\\.","",cols),collapse=".")))
  }
  
  dx <- dimnames2df(x,dim=dim)
  dref <- dimnames2df(ref,dim=dim)
  
  #detect matching columns
  if(!isTRUE(getOption("magclass_setMatching"))) {
    names(dref)[is.na(names(dref))] <- "NA"
    names(dx)[is.na(names(dx))] <- "NA"
    lx <- lapply(dx[names(dx)!=".line"],levels)
    lref <- lapply(dref[names(dref)!=".line"],levels)
    if(anyDuplicated(lx)==0 && anyDuplicated(lref)==0) {
      # matching will be based on dimension content rather
      # than set names (in case of duplicated columns,
      # set matching will be used instead)
      
      #temporarily split .line col from rest
      dref.line <- dref[".line"]
      dx.line   <- dx[".line"]
      dref[".line"] <- NULL
      dx[".line"] <- NULL
      #ensure unique set names
      tmp <- make.unique(c(names(dref),names(dx)),sep="")
      names(dref) <- tmp[1:ncol(dref)]
      names(dx) <- tmp[(ncol(dref)+1):length(tmp)]
      m <- match(lx,lref)
      tmp <- names(dref)[m]
      tmp[is.na(tmp)] <- names(dx)[is.na(tmp)]
      names(dx) <- tmp
      dref[".line"] <- dref.line
      dx[".line"] <- dx.line
    }
  }
  
  m <- merge(dref,dx,sort=FALSE, suffixes=c("_ref","_x"), by=setdiff(intersect(names(dref), names(dx)),".line"))
  
  # reorder m so that dref columns appear first
  m <- m[union(names(dref)[1:(ncol(dref)-1)],names(m))]
  m <- m[order(m$.line_ref),]
  
  tmpdim <- dim(x)
  tmpdim[dim] <- nrow(m)
  sizeCheck(tmpdim)
  
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
  dimnames(out)[[dim]] <- tmp$dimnames
  names(dimnames(out))[dim] <- tmp$name
  return(out)
}



