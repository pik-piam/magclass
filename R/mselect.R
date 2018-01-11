.mselect_support <- function(search,where,ndim,names) {
  search <- escapeRegex(search)
  search <- paste0("(",paste(search,collapse="|"),")")
  search <- paste0("^",paste(rep("[^\\.]*\\.",where-1),collapse=""),search,paste(rep("\\.[^\\.]*",ndim-where),collapse=""),"$")
  return(names[grep(search,names)])
}

.mselect_coords <- function(x,...) {
  if(is.null(names(dimnames(x)))) stop("Dimnames must have names in order to use mselect!")
  args <- list(...)
  if(length(args)==1) if(is.list(args[[1]])) args <- args[[1]]
  sep="\\."
  sets <- strsplit(names(dimnames(x)),sep)
  
  i <- getCells(x)
  j <- getYears(x)
  k <- getNames(x)
  
  if(is.null(j)) j <- 1
  if(is.null(k)) k <- 1
  
  for(n in names(args)) {
    where <-grep(paste0("^",n,"$"),unlist(sets))
    if(length(where)>1) stop(paste0("set name \"",n,"\" found more than once!"))
    if(length(where)==0) stop(paste0("set name \"",n,"\" not found!"))
    
    if(where<=length(sets[[1]])) {
      # spatial
      ndim <- nchar(gsub("[^\\.]","",getCells(x)[1])) + 1
      i <- .mselect_support(args[[n]],where,ndim,i)
    } else if(where<=length(unlist(sets[1:2]))){
      #temporal
      j <- .mselect_support(args[[n]],where-length(sets[[1]]),length(sets[[2]]),j)   
    } else {
      #data
      k <- .mselect_support(args[[n]],where-length(unlist(sets[1:2])),length(sets[[3]]),k)          
    }    
  }
  m <- list(i=i,j=j,k=k)
  m[lapply(m,length)==0] <- NULL
  return(m)
}





#' MSelect
#' 
#' Select values from a MAgPIE-object
#' 
#' This functions only work for MAgPIE objects with named dimensions as the
#' dimension name (set_name) has to be used to indicate in which dimension the
#' entries should be searched for!
#' 
#' @aliases mselect mselect<-
#' @param x MAgPIE object
#' @param ... entry selections of the form
#' \code{set_name=c(set_elem1,set_elem2)}. Alternatively a single list element
#' containing these selections can be provided.
#' @param collapseNames Boolean which decides whether names should be collapsed
#' or not.
#' @param value values on which the selected magpie entries should be set.
#' @return The reduced MAgPIE object containing only the selected entries or
#' the full MAgPIE object in which a selection of entries was manipulated.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{collapseNames}}, \code{"\linkS4class{magpie}"}
#' @examples
#' 
#'  data(population_magpie)
#'  population_magpie
#'  mselect(population_magpie,i=c("AFR","EUR"),scenario="A2",t="y2035")
#' 
#' @export
mselect <- function(x,...,collapseNames=FALSE) {
  m <- .mselect_coords(x,...)
  if(collapseNames) return(collapseNames(x[m$i,m$j,m$k]))
  return(x[m$i,m$j,m$k])
}

#' @describeIn mselect replace values in magpie object
#' @export
"mselect<-" <- function(x,...,value) {
  m <- .mselect_coords(x,...)
  x[m$i,m$j,m$k] <- value
  x <- updateMetadata(x)
  return(x)
}

