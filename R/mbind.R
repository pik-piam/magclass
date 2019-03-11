#' mbind
#' 
#' Merges MAgPIE-objects with identical structure in two dimensions. If data
#' differs in the temporal or spatial dimension each year or region/cell must
#' appear only once!
#' 
#' mbind2 is a reimplementation from mbind which had the aim to increase its
#' overall memory efficiency. However, it is not clear which function is better
#' and there are also some changes in behaviour of both functions. Therefore,
#' the new version was just added as mbind2 instead of using it as a full
#' replacement for mbind.
#' 
#' @aliases mbind mbind2
#' @param ... MAgPIE objects or a list of MAgPIE objects that should be merged.
#' @return The merged MAgPIE object
#' @author Jan Philipp Dietrich, Misko Stevanovic
#' @seealso \code{"\linkS4class{magpie}"}
#' @examples
#' 
#' m <- new.magpie(c("AFR","CPA","EUR"), c(1995,2005),"Data1",fill=c(1,2,3,4,5,6))
#' ms <- dimSums(m, dims=1)
#' mbind(m, ms)
#' my <- new.magpie(getRegions(m), 2010, getNames(m), fill=c(6,6,4))
#' mbind(m, my)
#' md <- new.magpie(getRegions(m), getYears(m), "Data2", fill=c(7,6,5,7,8,9))
#' mbind(m, md)
#' 
#' data(population_magpie)
#' a <- mbind(population_magpie,population_magpie)
#' dim(population_magpie)
#' dim(a)
#' 
#' 
#' @export mbind
#' @importFrom methods new
mbind <- function(...) {  
  inputs <- list(...)
  if(length(inputs)==1 & is.list(inputs[[1]])) inputs <- inputs[[1]]
  #Remove NULL elements from list
  for(i in length(inputs):1) {
    if(is.null(inputs[[i]])) {
      inputs[[i]] <- NULL
    } else if(prod(dim(inputs[[i]]))==0) {
      inputs[[i]] <- NULL
      warning("You are trying to mbind an empty magclass object. Is that really intended?")
    }
  }
  
  # if all inputs are NULL, return NULL
  if (0 == length(inputs))
    return(NULL)

  regio <- NULL
  cells <- NULL
  elems <- NULL
  years <- NULL
  diffspat <- FALSE
  difftemp <- FALSE
  diffdata <- FALSE
  for(i in 1:length(inputs)) {
    if(!is.magpie(inputs[[i]])) stop("Inputs must all be MAgPIE-objects")
    if(is.null(dimnames(inputs[[i]])[[3]])) dimnames(inputs[[i]])[[3]] <- paste("dummydimname",1:ndata(inputs[[i]]),sep="")
    #Check which dimensions differ
    if(suppressWarnings(any(sort(dimnames(inputs[[1]])[[1]])!=sort(dimnames(inputs[[i]])[[1]])))) diffspat <- TRUE
    if(suppressWarnings(any(sort(dimnames(inputs[[1]])[[2]])!=sort(dimnames(inputs[[i]])[[2]])))) difftemp <- TRUE
    if(suppressWarnings(any(sort(dimnames(inputs[[1]])[[3]])!=sort(dimnames(inputs[[i]])[[3]])))) diffdata <- TRUE
    years <- c(years, getYears(inputs[[i]]))
    elems <- c(elems, getNames(inputs[[i]]))
    cells <- c(cells, getCells(inputs[[i]]))   
    if(!diffspat & ncells(inputs[[1]])>1) inputs[[i]] <- inputs[[i]][getCells(inputs[[1]]),,]
    if(!difftemp & nyears(inputs[[1]])>1) inputs[[i]] <- inputs[[i]][,getYears(inputs[[1]]),]
    if(!diffdata &  ndata(inputs[[1]])>1) inputs[[i]] <- inputs[[i]][,,getNames(inputs[[1]])]
  }
  
  if(!(length(grep(".",cells,fixed=TRUE)) %in% c(0,length(cells)))) stop("Mixture of regional (no cell numbers) and cellular (with cell numbers) data objects! Cannot handle this case!")
  
  if(diffspat & difftemp) stop("Cannot handle objects! Spatial as well as temporal dimensions differ!")      
  if(difftemp & diffdata) stop("Cannot handle objects! Data as well as temporal dimensions differ!")      
  if(diffdata & diffspat) stop("Cannot handle objects! Data as well as spatial dimensions differ!") 
  if(!diffspat) {
    
  }
  if(difftemp) {
    if(length(years)!=length(unique(years))) stop("Some years occur more than once! Cannot handle this case!")
    output <- new("magpie",abind::abind(inputs,along=2))
  } else if(diffspat){
    if(length(cells) != length(unique(cells))) stop("Some regions/cells occur more than once! Cannot handle this case!")
    output <- new("magpie",abind::abind(inputs,along=1))
  } else {
    tmp <- function(x) return(length(getNames(x,fulldim = TRUE)))
    tmp <- sapply(inputs,tmp)
    if(length(unique(tmp))>1) warning("mbind most likely returned an erronous magpie object due to different numbers of data subdimensions in inputs!")
    output <- new("magpie",abind::abind(inputs,along=3))
  }
  if(length(grep("dummydimname",getNames(output),fixed=TRUE))==ndata(output)) dimnames(output)[[3]] <- NULL 
  names(dimnames(output)) <- names(dimnames(inputs[[1]]))
  
  return(updateMetadata(output, inputs, calcHistory="update",cH_priority=3))
}