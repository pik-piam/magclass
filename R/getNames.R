.dim_fulldim <- function(x,dim) {
  tmp <- fulldim(x)[[2]]
  tmp[[2]] <- NULL #remove temporal dimension
  tmp[[1]] <- NULL #remove spatial dimension
  
  if(!is.null(dim)) {
    #check whether dim contains only 1 element
    if(length(dim)>1) stop("Only a single dimension can be chosen with argument \"dim\"!")
    #check whether chosen dimension exists
    if(is.numeric(dim)) {
      if(dim < 1 | dim > length(tmp)) stop("Chosen data dimension does not exist (dim = ",dim,")")
      which_dim <- dim
    } else {
      if(sum(names(tmp) %in% dim)==0) stop("Chosen data dimension does not exist (dim = ",dim,")")
      which_dim <- which(names(tmp) %in% dim)
    }
    maxdim <- length(tmp)
    tmp <- tmp[[dim]]
    if(!is.null(tmp)) {
      attr(tmp,"which_dim") <- which_dim
      attr(tmp,"maxdim") <- maxdim
    }
  }  
  return(tmp)
}







#' Get dataset names
#' 
#' Extracts dataset names of a MAgPIE-object
#' 
#' setNames is a shortcut to use a MAgPIE object with manipulated data names.
#' The setNames method uses the variable names "object" and "nm" in order to be
#' consistent to the already existing function setNames.
#' 
#' @aliases getNames getNames<- 
#' @param x MAgPIE object
#' @param fulldim specifies, how the object is treated. In case of FALSE, it is
#' assumed that x is 3 dimensional and dimnames(x)[[3]] is returned. In case of
#' TRUE, the dimnames of the real third dimension namesare returned
#' @param dim Argument to choose a specific data dimension either by name of
#' the dimension or by number of the data dimension.
#' @param value a vector of names current names should be replaced with. If
#' only one data element exists you can also set the name to NULL.
#' @return getNames returns data names of the MAgPIE-object, whereas setNames
#' returns the MAgPIE object with the manipulated data names.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{setNames-methods}}, \code{\link{getRegions}}, \code{\link{getYears}},
#' \code{\link{getCPR}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}},\code{\link{ndata}},
#' \code{"\linkS4class{magpie}"}
#' @examples
#' 
#'  a <- as.magpie(1)
#'  getNames(a)
#'  setNames(a,"bla")
#'  
#'  x <- new.magpie("GLO",2000,c("a.o1","b.o1","a.o2"))
#'  getNames(x,dim=2)
#'  
#'  getSets(x,fulldim=FALSE)[3] <- "bla.blub"
#'  getNames(x,dim="bla")
#'  
#'  getSets(x)[4] <- "ble"
#'  getNames(x,dim="ble") <- c("Hi","Bye")
#'  x
#' 
#' @export
getNames <- function(x,fulldim=FALSE,dim=NULL) {
  if(!is.null(dim)) fulldim <- TRUE
  if(fulldim==FALSE){
    return(dimnames(x)[[3]])
  } else {
    tmp <- .dim_fulldim(x,dim)
    if(!is.null(tmp)) {
      attr(tmp,"which_dim") <- NULL
      attr(tmp,"maxdim") <- NULL
    }
    return(tmp)
  }
}

#' @describeIn getNames set names
#' @export
"getNames<-" <- function(x,dim=NULL,value) {
  if(is.null(dim)) {
    if(is.null(value)) {
      if(ndata(x)>1) stop("Setting data names to NULL is not possible as ndata is bigger than 1!")
      s <- getSets(x,fulldim=FALSE)[3]
      dimnames(x)[[3]] <- value
      getSets(x,fulldim=FALSE)[3] <- s
    } else {
      if(ndata(x)!=length(value)) stop("Wrong number of data names supplied!")
      if(ndata(x)==0) return(x)
      tmp <- nchar(gsub("[^\\.]*","",value))
      if(any(tmp!=tmp[1])) stop("Inconsistent names! Number of dots per name has always to be the same as it is separating different data dimensions")
      dimnames(x)[[3]] <- value
    }
    return(x)
  } else {
   old_names <- .dim_fulldim(x,dim) 
   if(is.null(old_names)) {
     getNames(x,dim=NULL) <- value
     return(x)
   }
   which_dim <- attr(old_names,"which_dim")
   maxdim <- attr(old_names,"maxdim")
   if(is.null(which_dim)) which_dim <- 1
   if(is.null(maxdim)) maxdim <- 1
   
   if(is.null(value)) {
     if(length(old_names) > 1) stop("Setting data names to NULL is not possible as data dimension has more than 1 element!") 
     return(collapseNames(x,collapsedim=which_dim))
   } else {
     if(length(old_names)!=length(value)) stop("Wrong number of data names supplied!")
     d <- dimnames(x)[[3]]
     searchstring <- 
     start_pattern <- paste0("^(",paste(rep("[^\\.]*\\.",which_dim-1),collapse=""),")")
     end_pattern <- paste0("(",paste(rep("\\.[^\\.]*",maxdim-which_dim),collapse=""),")$")
     for(i in 1:length(value)) {
       d <- sub(paste0(start_pattern,escapeRegex(old_names[i]),end_pattern),paste0("\\1",value[i],"\\2"),d)
     }
     dimnames(x)[[3]] <- d
     return(x)
   }
  }
}
