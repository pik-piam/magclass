#' magpie_expand
#' 
#' Expands a MAgPIE object based on a reference
#' 
#' Expansion means here that the dimensions of x are expanded acordingly to
#' ref. Please note that this is really only about expansion. In the case that
#' one dimension of ref is smaller than of x nothing happens with this
#' dimension. At the moment magpie_expand is only internally available in the
#' magclass library
#' 
#' You can influence the verbosity of this function by setting the option
#' "magclass.verbosity". By default verbosity is set to 2 which means that
#' warnings as well as notes are returned. Setting verbosity to 1 means that
#' only warnings are returned but no notes. This is done by
#' options(verbosity.level=1)
#' 
#' Currently magpie_expand is prepared to be updated to a newer version. By default the old setup is
#' currently active. To switch to the new setup you have to set \code{options(magpie_version=2)}.
#' 
#' @param x MAgPIE object that should be expanded
#' @param ref MAgPIE object that serves as a reference
#' @return An expanded version of x.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{as.magpie}}, \code{\link[base]{options}}
#' @examples
#' 
#'  a <- new.magpie(c("AFR","CPA"),"y1995",c("m","n"))
#'  b <- new.magpie("GLO","y1995",c("bla","blub"))
#'  magpie_expand(b,a)
#'  options(magclass.verbosity=1)
#'  magpie_expand(b,a)
#' 
#' @export magpie_expand
magpie_expand <- function(x,ref) {
  
  version <- getOption("magclass_version")
  if(!is.null(version) && version>1) return(magpie_expand2(x,ref))
                                            
  #x: MAgPIE object which should be expanded
  #ref: Reference object defining the structure to which x should be expanded
  #1.spatial dimension
  if(ncells(ref)>ncells(x)) {
    #regional or global data?
    if(nregions(x)==ncells(x)) {
      if(nregions(x)==1 & getRegions(x)[1]=="GLO") {
        #global data
        x <- x[rep(1,ncells(ref)),,]
        getCells(x) <- getCells(ref) 
      } else {
        #regional data
        if(nregions(x)!=nregions(ref)) stop("Cannot expand MAgPIE object! Incompatible cells (different number of regions).")
        if(any(sort(getRegions(x))!=sort(getRegions(ref)))) stop("Cannot expand MAgPIE object! Incompatible cells (different regions).")
        #If this place is reached it means that x is regional data with the same regions as ref
        cells<-gsub("\\.[0-9]+$","",getCells(ref))
        x<-x[cells,,]
        getCells(x)<-getCells(ref)
      } 
    } else {
      stop("Cannot expand MAgPIE object! Incompatible cells (different length and neither purely regional or global data).") 
    }
  } else if(ncells(ref)==ncells(x)) {
    if(any(getCells(ref)!=getCells(x))) {
      if(ncells(x)!=1 & any(sort(getCells(ref))!=sort(getCells(x)))){
        if(all(sort(getRegions(ref))==sort(getRegions(x))) & ncells(x)==nregions(x)) {
          #same regions, but different pseudo cells
          x <- x[getRegions(ref),,]
          getCells(x) <- getCells(ref)
        } else {
          stop("Cannot expand MAgPIE object! Incompatible cells (same length, different cell names).")  
        }
      } else if(ncells(x)==1){
        if(getRegions(x)=="GLO") {
          getCells(x) <- getCells(ref)
        } else {
          if(getRegions(ref)!="GLO" & getRegions(ref)!=getRegions(x)) stop("Region names do not agree! x:",getRegions(x)," ref:",getRegions(ref))
        }
      } else {
        #Different order...reorder cells
        x <- x[getCells(ref),,]
      }
    }
  }
  
  #2.temporal dimension
  if(nyears(ref)>nyears(x)) {
    if(nyears(x)==1) {
      if(!is.null(getYears(x))) stop("Years do not agree! x:",getYears(x)," ref:",getYears(ref))
      x <- x[,rep(1,nyears(ref)),]
      getYears(x) <- getYears(ref)
      getSets(x,fulldim=FALSE)[2] <- ifelse(is.null(getSets(ref,fulldim=FALSE)[2]),"year",getSets(ref,fulldim=FALSE)[2])
    } else {
      stop("Cannot expand MAgPIE object! No clear mapping in temporal dimension")
    }
  } else if(nyears(ref)==nyears(x)) {
    if(is.null(getYears(x))) {
      getYears(x) <- getYears(ref)
    } else if(any(getYears(ref)!=getYears(x))) {
      if(any(sort(getYears(ref))!=sort(getYears(x)))){
        if(nyears(x)==1) {
          stop("Years do not agree! x:",paste(getYears(x),collapse=" ")," ref:",paste(getYears(ref),collapse=" ")) 
        } else {
          stop("Years do not agree! No mapping possible! x:",paste(getYears(x),collapse=" ")," ref:",paste(getYears(ref),collapse=" "))
        }
      } else {
        #Different order...reorder years
        x <- x[,getYears(ref),]
      }
    }
  }
  
  #3.data dimension
  if(length(getNames(x))==0) {
    if(ndata(x)>1) stop("Inconsistent MAgPIE file: more than 1 element in data dimension but no names given!")
    x <- x[,,rep(1,ndata(ref))] 
    getNames(x) <- getNames(ref)
  } else if(length(getNames(ref))==0) {
    if(ndata(ref)>1) stop("Inconsistent MAgPIE reference file: more than 1 element in data dimension but no names given!")
  } else if(any(suppressWarnings(getNames(x)!=getNames(ref)))){
    if(all(suppressWarnings(sort(getNames(x))==sort(getNames(ref))))) {
      x <- x[,,getNames(ref)]
    } else {
      #both, x and ref, have data names
      #data names do not agree
      .fulldatadim <- function(x,sort=TRUE) {
        x <- fulldim(x)[[2]]
        x[[2]] <- NULL #remove temporal dim
        x[[1]] <- NULL #remove spatial dim
        if(sort) x <- lapply(x,sort)
        return(x)
      }
      rfdim <- .fulldatadim(ref,TRUE)
      xfdim <- .fulldatadim(x,TRUE)
      
      toadd <- which(!(rfdim %in% xfdim)) #which dimensions have to be added?
      if(length(toadd)>0) {
        tmp <- NULL
        for(i in toadd) { 
          tmp  <- paste(rep(tmp,each=length(rfdim[[i]])),rfdim[[i]],sep=".")
        }
        newnames <- paste(rep(getNames(x),each=length(tmp)),tmp,sep="")
        x <- x[,,rep(1:ndata(x),each=length(tmp))]
        getNames(x) <- newnames
        getSets(x,fulldim=FALSE)[3]  <- paste(getSets(x,fulldim=FALSE)[3],paste(names(rfdim)[toadd],collapse="."),sep=".")
        if(getOption("magclass.verbosity")>1) cat("NOTE (magpie_expand): data dimensionality of MAgPIE object expanded (added dimensions:",paste(rfdim[toadd]),")\n")
      }
      
      #is the order of the dimensions in the data dimension identical? reorder if necessary (only performed if the number of dimensions is the same)
      xfdim <- .fulldatadim(x,TRUE)
      .tmp <- function(rfdim,xfdim) return(which(xfdim%in% list(rfdim)))
      order <- lapply(rfdim,.tmp,xfdim)
      lengths_order <- sapply(order,length)
      if(any(lengths_order==0)) stop("Some ref dimensions cannot be found in x after expansion. magpie_expand-function seems to be bugged!")
      if(any(lengths_order>1)) {
        warning("Some ref dimensions are found more than once in x after expansion. Mapping might go wrong!")
        probs <- which(lengths_order>1)
        taken <- NULL
        for(i in probs) {
          order[[i]] <- setdiff(order[[i]],taken)
          taken <- c(taken,order[[i]][-1])
          order[[i]] <- order[[i]][1]
          if(length(order[[i]])==0) stop("Something went wrong in mapping correction in magpie_expand. Function seems to be bugged!")
        }
      }
      order <- unlist(order)
      #add missing dimensions at the end
      order <- c(order,setdiff(1:length(xfdim),order))
      
      if(any(order!=1:length(order))) {
        #different order
        search <- paste("^",paste(rep("([^\\.]*)",length(order)),collapse="\\."),"$",sep="")
        replace <- paste(paste("\\",order,sep=""),collapse="\\.")
        getNames(x) <- sub(search,replace,getNames(x))
        getSets(x,fulldim=FALSE)[3] <- sub(search,replace,getSets(x,fulldim=FALSE)[3])
      }

      
      #try to order x based on ref (only possible if objects have the same size)
      if(ndata(x)==ndata(ref)) {
        if(length(xfdim)==length(rfdim)) {
          #simple case: same number of dimensions
          #in the case that now all data names of x and ref agree reorder the data best on order of ref
          if(all(sort(getNames(x))==sort(getNames(ref)))) {
            x@.Data <- x@.Data[,,getNames(ref),drop=FALSE]
          } else {
            stop("Data names do not agree between ref and expanded x, magpie_expand seems to be bugged! (same #dimensions)")
          }
        } else {
          #more complicated case in which x has more dimensions than ref
          search <- paste0(paste0(rep("\\.[^\\.]*",length(xfdim)-length(rfdim)),collapse=""),"$")
          reduced_xnames <- sub(search,"",getNames(x))
          #in the case that now all data names of x and ref agree reorder the data best on order of ref
          if(all(sort(reduced_xnames)==sort(getNames(ref)))) {
            x <- x[,,getNames(ref)]
          } else {
            stop("Data names do not agree between ref and expanded x, magpie_expand seems to be bugged! (different #dimensions)")
          }        
        }
      }
    }
  }
  return(x)
}

magpie_expand2 <- function(x,ref) {

  #x: MAgPIE object which should be expanded
  #ref: Reference object defining the structure to which x should be expanded

  #1.spatial dimension
  #2.temporal dimension
  #3.data dimension

  for(i in 1:3) {
    if(i==1) {
      if(dim(ref)[1]==1 && !is.null(rownames(ref)) && rownames(ref)=="GLO") rownames(ref) <- NULL
      else if(dim(x)[1]==1 && !is.null(rownames(x)) && rownames(x)=="GLO") rownames(x) <- NULL
    } 
    
    if(is.null(dimnames(ref)[[i]]) && dim(ref)[i] > 1) stop("Inconsistent MAgPIE reference file: more than 1 element in dimension ",i," but no names given!")
    if(is.null(dimnames(x)[[i]])) {
      if(dim(x)[i] > 1) stop("Inconsistent MAgPIE file: more than 1 element in dimension ",i," but no names given!")
      if(dim(ref)[i] > 1) {
        if(i==1) {
          x <- x[rep(1,dim(ref)[i]),,]
        } else if(i==2) {
          x <- x[,rep(1,dim(ref)[i]),]
        } else {
          x <- x[,,rep(1,dim(ref)[i])]
        }
        dimnames(x)[[i]] <- dimnames(ref)[[i]]
      }
    } else if(!is.null(dimnames(ref)[[i]]) && any(suppressWarnings(dimnames(x)[[i]]!=dimnames(ref)[[i]]))) {
      if(dim(x)[i]==dim(ref)[i] && all(suppressWarnings(sort(dimnames(x)[[i]])==sort(dimnames(ref)[[i]])))) {
        if(i==1) {
          x <- x[dimnames(ref)[[i]],,]
        } else if(i==2) {
          x <- x[,dimnames(ref)[[i]],]
        } else {
          x <- x[,,dimnames(ref)[[i]]]
        }
      } else {
        #both, x and ref, have dimnames
        #dimnames do not agree
        .alldims <- function(x, dim, sort=TRUE) {
          x <- getItems(x,dim=dim,split=TRUE)
          if(sort) x <- lapply(x,sort)
          return(x)
        }
        rfdim <- .alldims(ref, dim=i, sort=TRUE)
        xfdim <- .alldims(x,   dim=i, sort=TRUE)

        toadd <- which(!(rfdim %in% xfdim)) #which dimensions have to be added?
        if(length(toadd)>0) {
          tmp <- NULL
          for(a in toadd) {
            tmp  <- paste(rep(tmp,each=length(rfdim[[a]])),rfdim[[a]],sep=".")
          }
          newnames <- paste(rep(dimnames(x)[[i]],each=length(tmp)),tmp,sep="")
          if(i==1) {
            x <- x[rep(1:dim(x)[i],each=length(tmp)),,]
          } else if(i==2) {
            x <- x[,rep(1:dim(x)[i],each=length(tmp)),]
          } else {
            x <- x[,,rep(1:dim(x)[i],each=length(tmp))]
          }
          dimnames(x)[[i]] <- newnames
          getSets(x,fulldim=FALSE)[i]  <- paste(getSets(x,fulldim=FALSE)[i],paste(names(rfdim)[toadd],collapse="."),sep=".")
          if(getOption("magclass.verbosity")>1) message("NOTE (magpie_expand): dimensionality of MAgPIE object expanded (added dimensions:",paste(rfdim[toadd]),")")
        }

        #is the order of the dimensions in the data dimension identical? reorder if necessary (only performed if the number of dimensions is the same)
        xfdim <- .alldims(x, dim=i, sort=TRUE)
        .tmp <- function(rfdim,xfdim) return(which(xfdim %in% list(rfdim)))
        order <- lapply(rfdim,.tmp,xfdim)
        lengths_order <- sapply(order,length)
        if(any(lengths_order==0)) stop("Some ref dimensions cannot be found in x after expansion. magpie_expand-function seems to be bugged!")
        if(any(lengths_order>1)) {
          warning("Some ref dimensions are found more than once in x after expansion. Mapping might go wrong!")
          probs <- which(lengths_order>1)
          taken <- NULL
          for(i in probs) {
            order[[i]] <- setdiff(order[[i]],taken)
            taken <- c(taken,order[[i]][-1])
            order[[i]] <- order[[i]][1]
            if(length(order[[i]])==0) stop("Something went wrong in mapping correction in magpie_expand. Function seems to be bugged!")
          }
        }
        order <- unlist(order)
        #add missing dimensions at the end
        order <- c(order,setdiff(1:length(xfdim),order))

        if(any(order!=1:length(order))) {
          #different order
          search <- paste("^",paste(rep("([^\\.]*)",length(order)),collapse="\\."),"$",sep="")
          replace <- paste(paste("\\",order,sep=""),collapse="\\.")
          dimnames(x)[[i]] <- sub(search,replace,dimnames(x)[[i]])
          getSets(x,fulldim=FALSE)[i] <- sub(search,replace,getSets(x,fulldim=FALSE)[i])
        }

        #try to order x based on ref (only possible if objects have the same size)
        if(dim(x)[i]==dim(ref)[i]) {
          if(length(xfdim)==length(rfdim)) {
            #simple case: same number of dimensions
            #in the case that now all data names of x and ref agree reorder the data best on order of ref
            if(all(sort(dimnames(x)[[i]])==sort(dimnames(ref)[[i]]))) {
              if(i==1) {
                x@.Data <- x@.Data[dimnames(ref)[[i]],,,drop=FALSE]
              } else if(i==2) {
                x@.Data <- x@.Data[,dimnames(ref)[[i]],,drop=FALSE]
              } else {
                x@.Data <- x@.Data[,,dimnames(ref)[[i]],drop=FALSE]
              }
            } else {
              stop("names do not agree between ref and expanded x, magpie_expand seems to be bugged! (same #dimensions)")
            }
          } else {
            #more complicated case in which x has more dimensions than ref
            search <- paste0(paste0(rep("\\.[^\\.]*",length(xfdim)-length(rfdim)),collapse=""),"$")
            reduced_xnames <- sub(search,"",dimnames(x)[[i]])
            #in the case that now all data names of x and ref agree reorder the data best on order of ref
            if(all(sort(reduced_xnames)==sort(dimnames(ref)[[i]]))) {
              if(i==1) {
                x <- x[dimnames(ref)[[i]],,]
              } else if(i==2) {
                x <- x[,dimnames(ref)[[i]],]
              } else {
                x <- x[,,dimnames(ref)[[i]]] 
              }
            } else {
              stop("Names do not agree between ref and expanded x, magpie_expand seems to be bugged! (different #dimensions)")
            }
          }
        }
      }
    }
  }
  getSets(x) <- make.unique(getSets(x),sep="")
  return(x)
}

