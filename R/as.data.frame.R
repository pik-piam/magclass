#' ~~ Methods for Function as.data.frame ~~
#'
#' ~~ Methods for function \code{as.data.frame} ~~
#'
#'
#' @name as.data.frame-methods
#' @aliases as.data.frame as.data.frame-methods as.data.frame,ANY-method
#' as.data.frame,magpie-method
#' @docType methods
#' @param x A MAgPIE-object
#' @param rev The revision of the algorithm that should be used for conversion.
#' rev=1 creates columns with the predefined names Cell, Region, Year, Data1,
#' Data2,... and Value, rev=2 uses the set names of the MAgPIE object for
#' naming and adds an attribute "dimtype" to the data.frame which contains
#' information about the types of the different columns (spatial, temporal,
#' data or value), rev=3 orders and renames columns according to IAMC template conventions:
#' "model","scenario","region","variable","unit","period","value".
#' In addition, xy coordinates are added if the data is at grid cell level.
#' @section Methods: \describe{
#'
#' \item{list("signature(x = \"magpie\")")}{ Conversion creates columns for
#' Cell, Region, Year, Data1, Data2,... and Value } }
#' @keywords methods
#' @examples
#'
#' pop <- maxample("pop")
#' head(as.data.frame(pop))
#' head(as.data.frame(pop,rev=2))
#'
#' @importFrom utils type.convert
#' @exportMethod as.data.frame

setMethod("as.data.frame",
  signature(x="magpie"),
  function(x,rev=1)
  {
    if(rev==1) {
      yearsAsIntegers <- suppressWarnings(getYears(x,as.integer=TRUE))
      if(any(is.na(getYears(x)) != is.na(yearsAsIntegers))) {
        dimnames(x)[[2]] <- getYears(x)
      } else {
        dimnames(x)[[2]] <- yearsAsIntegers
      }
      if(is.null(dimnames(x)[[2]])) dimnames(x)[[2]] <- 0
      if(is.null(dimnames(x)[[3]])) dimnames(x)[[3]] <- "NA"
      if(any(dim(x)==0)) {
        return(data.frame())
      } else {
        x <- as.data.frame(as.table(x))
      }
      if(all(grepl(".",x[[3]],fixed=TRUE))) {
        levels(x[[3]]) <- gsub("\\.$","\\.STRINGTORESETAG",levels(x[[3]]))
        tmp <- data.frame(t(matrix(unlist(strsplit(as.character(x[[3]]),split="\\.")),ncol=nrow(x))),stringsAsFactors=FALSE)
        for(i in 1:ncol(tmp)) {
          tmp[[i]] <- factor(tmp[[i]],unique(tmp[[i]]))
          levels(tmp[[i]]) <- gsub("STRINGTORESETAG","",levels(tmp[[i]]))
        }
        x <- cbind(x[,1:2],tmp,x[4])
      }
      colnames(x) <- c("Region","Year",paste("Data",1:(dim(x)[2]-3),sep=""),"Value")
      x <- cbind(Cell=suppressWarnings(as.integer(gsub("^[^\\.]*\\.","",x$Region))),x)
      x$Region <- gsub("\\..*$","",x$Region)
      return(x)
    } else if(rev==2) {
      x <- clean_magpie(x,what="sets")
      dimnames(x)[[2]] <- getYears(x,as.integer=TRUE)
      if(any(dim(x)==0)) {
        return(data.frame())
      } else {
        x <- as.data.frame(as.table(x), stringsAsFactors=FALSE)
      }
      names(x)[4] <- ".value"
      what <- ".value"
      types <- c(".spat",".temp",".data")
      for(i in 3:1) {
        if(grepl(".",names(x)[i],fixed=TRUE)) {
          tmp <- data.frame(t(matrix(unlist(strsplit(as.character(x[[i]]),split="\\.")),ncol=nrow(x))),stringsAsFactors=FALSE)
          names(tmp) <-  strsplit(names(x)[i],split="\\.")[[1]]
          if(i==1) {
            x <- cbind(tmp,x[2:length(x)])
          } else {
            x <- cbind(x[1:(i-1)],tmp,x[(i+1):length(x)])
          }
          what <- c(paste0(types[i],1:dim(tmp)[2]),what)
        } else {
          what <- c(paste0(types[i],1),what)
        }
      }
      #use other types than character if possible
      for(i in 1:ncol(x)) {
        if(is.character(x[[i]])) {
          x[[i]] <- type.convert(x[[i]],as.is=TRUE)
          if(is.character(x[[i]])) x[[i]] <- factor(x[[i]],unique(x[[i]]))
        }
      }
      attr(x,"dimtype") <- what
      return(x)
    } else if(rev==3) {
      x <- clean_magpie(x,what="sets")
      if (!hasCoords(x) && dimExists(1.2, x)) {
        items <- getItems(x, dim = 1.2)
        if (length(items) == 59199 & all(items == seq_len(59199))) {
          # special treatment for data with 59199 cells as this
          # is the originally used 0.5deg resolution in magclass
          # before it had been generalized
          #getCoords(x) <- magclassdata$half_deg[c("lon", "lat")]
          addCoords <- TRUE
        }
      } else addCoords <- FALSE
      dimnames(x)[[2]] <- getYears(x,as.integer=TRUE)
      if(any(dim(x)==0)) {
        return(data.frame())
      } else {
        x <- as.data.frame(as.table(x), stringsAsFactors=FALSE)
      }
      names(x)[4] <- ".value"
      what <- ".value"
      types <- c(".spat",".temp",".data")
      for(i in 3:1) {
        if(grepl(".",names(x)[i],fixed=TRUE)) {
          tmp <- data.frame(t(matrix(unlist(strsplit(as.character(x[[i]]),split="\\.")),ncol=nrow(x))),stringsAsFactors=FALSE)
          names(tmp) <-  strsplit(names(x)[i],split="\\.")[[1]]
          if(i==1) {
            x <- cbind(tmp,x[2:length(x)])
          } else {
            x <- cbind(x[1:(i-1)],tmp,x[(i+1):length(x)])
          }
          what <- c(paste0(types[i],1:dim(tmp)[2]),what)
        } else {
          what <- c(paste0(types[i],1),what)
        }
      }
      #use other types than character if possible
      for(i in 1:ncol(x)) {
        if(is.character(x[[i]])) {
          x[[i]] <- type.convert(x[[i]],as.is=TRUE)
          if(is.character(x[[i]])) x[[i]] <- factor(x[[i]],unique(x[[i]]))
        }
      }
      attr(x,"dimtype") <- what
      unitsplit <- function(x, col) {
        w <- grepl("\\(.*\\)", x[[col]])
        #    x[[col]][!w] <- paste0(x[[col]][!w], " (N/A)")
        key <- "(.*?) \\((([^\\|]*))\\)($|\\.)"
        tmp <- data.frame(sub(key, "\\1", x[[col]]),
                          sub(key, "\\2", x[[col]]))
        names(tmp) <- c(names(x)[col], "unit")
        x <- cbind(tmp, x[setdiff(seq_len(ncol(x)), col)])
        return(x)
      }
      i <- which(names(x)=="variable")
      if(length(i)==1) {
        if((any(grepl(" \\(.*\\)$", x[,i])))) {
          x <- unitsplit(x,i)
          x$variable <- factor(x$variable)
          x$unit <- factor(x$unit)
        }
      }
      names(x)[names(x)=="year"] <- "period"
      names(x)[names(x)==".value"] <- "value"
      if(all(c("model","scenario","variable","unit") %in% names(x))) {
        x <- x[,c("model","scenario","region","variable","unit","period","value")]
      } else if (all(c("model","scenario","variable") %in% names(x))) {
        x <- x[,c("model","scenario","region","variable","period","value")]
      }
      if(addCoords) {
        coords <- magclassdata$half_deg[c("lon", "lat")]
        names(coords) <- c("x","y")
        x <- cbind(x,coords)
        x[["region1"]] <- NULL
      }
      x$region <- factor(x$region,sort(levels(x$region)))
      return(x)
    } else {
      stop('Unknown revision "',rev,'"!')
    }
  }
)
