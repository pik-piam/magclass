#' Write file in report format
#' 
#' This function writes the content of a MAgPIE object into a file or returns
#' it directly using the reporting format as it is used for many model
#' intercomparisons.
#' 
#' 
#' @param x MAgPIE object or a list of lists with MAgPIE objects as created by
#' read.report. In the latter case settings for model and scenario are
#' overwritten by the information given in the list.
#' @param file file name the object should be written to. If NULL the formatted
#' content is returned
#' @param model Name of the model which calculated the results
#' @param scenario The scenario which was used to get that results.
#' @param unit Unit of the data. Only relevant if unit is not already supplied
#' in Dimnames (format "name (unit)"). Can be either a single string or a
#' vector of strings with a length equal to the number of different data
#' elements in the MAgPIE object
#' @param ndigit Number of digits the output should have
#' @param append Logical which decides whether data should be added to an
#' existing file or an existing file should be overwritten
#' @param skipempty Determines whether empty entries (all data NA) should be
#' written to file or not.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{read.report}}
#' @examples
#' 
#' \dontrun{
#' data(population_magpie)
#' write.report(population_magpie)
#' }
#' 
#' @export write.report
#' @importFrom utils write.table
#' 
write.report <- function(x,file=NULL,model="MAgPIE",scenario="default",unit=NA,ndigit=4,append=FALSE,skipempty=TRUE) {
  if(is.list(x)) {
    if(is.list(x[[1]])) {
      for(scenario in names(x)){
        for(model in names(x[[scenario]])) {
          write.report(x[[scenario]][[model]],file=file,model=model,scenario=scenario,unit=unit,ndigit=ndigit,append=append)  
          append <- TRUE
        }
      }  
    } else {
      stop("Wrong format. x must be either a list of lists or a MAgPIE object! Only single list found!")
    }
  } else {

    if(!is.magpie(x)) stop("Input is not a MAgPIE object!")
    dimnames(x)[[1]] <- sub("^GLO(\\.{0,1}[0-9]*)$","World\\1",dimnames(x)[[1]])
    
    # If data was read in by read.report there is an attribute $dimnames$scenario.model.variable.
    # This will be used below to structure the output of write.report exactly like the input.
    # That means: write.report automatically recognizes models and scenarios and 
    # does not put it into the data-dimension
    if (scenario[1] == "default" & length(scenario) == 1 & length(names(attr(x,"dimnames"))) >2) {
      if (names(attr(x,"dimnames"))[[3]] == "scenario.model.variable") {
        scenario <- fulldim(x)[[2]]$scenario
        model <- fulldim(x)[[2]]$model
      }
    }

    if (is.na(unit) & withMetadata()) {
      unit <- units(x)
    }
    unitdef<-unit
    if (is(unit,"units")) {
      if (as.numeric(unit)!=1) {
        unit <- paste(as.character(unit),as.character(units(unit)))
      }else {
        unit <- as.character(units(unit))
      }
    }
    ii<-1
    for (mod in model) {
      for (scen in scenario) {
        if (length(fulldim(x)[[2]]) == 5 ) {
          if (length(strsplit(getNames(x)[1],split="\\.")[[1]]) > 1 
              & scen %in% unlist(lapply(strsplit(getNames(x),split="\\."),'[[',1))
              & mod %in% unlist(lapply(strsplit(getNames(x),split="\\."),'[[',2))) {
            scenmod <- paste(scen,mod,sep=".")
            if(!any(grepl(scenmod,getNames(x)))) {
              next()
            }
            xtemp<-x[,,scenmod] 
          } else {
            xtemp <- x
          }
        } else {
          xtemp<-x           
        }
        ndata <- ndata(xtemp)
        nregions <- nregions(xtemp)
        nyears <- nyears(xtemp)
        regions <- getRegions(xtemp)
        years <- gsub(" ",0,prettyNum(getYears(xtemp,as.integer=TRUE),width=4))
        if(length(unit)==1) {
          nelem_with_brackets <- length(grep("\\(*\\)$",getNames(xtemp)))
          if(nelem_with_brackets==dim(xtemp)[3]) {
            tmp <- getNames(xtemp)
            dimnames(xtemp)[[3]] <- sub(" \\(([^\\(]*)\\)($|\\.)","",tmp)
            unit <- sub("\\)$","",sub(".* \\(","",tmp))
          } else {
            if(nelem_with_brackets > 0) warning("Some but not all variable entries provide information in brackets which might be a unit information. To have it detected as unit all entries must provide this information!")
            unit <- rep(unit,ndata)
          }
        }
        
        output <- matrix(NA,nregions*ndata,5+nyears)
        colnames(output) <- c("Model","Scenario","Region","Variable","Unit",years)
        output[,"Model"] <- mod
        output[,"Scenario"] <- scen
        output[,"Region"] <- rep(regions,ndata)
        
        for(i in 1:ndata){
          if (length(fulldim(x)[[2]]) == 5){
            if (length(strsplit(getNames(xtemp)[i],split="\\.")[[1]]) > 1 
                & strsplit(getNames(xtemp)[i],split="\\.")[[1]][[1]]==scen 
                & strsplit(getNames(xtemp)[i],split="\\.")[[1]][[2]]==mod) {
              output[(i-1)*nregions + 1:nregions,"Variable"] <- strsplit(getNames(xtemp)[i],split="\\.")[[1]][[3]]
            } else {
              output[(i-1)*nregions + 1:nregions,"Variable"] <- gsub(".","|",getNames(xtemp)[i],fixed=TRUE)
            }
          } else {
            output[(i-1)*nregions + 1:nregions,"Variable"] <- gsub(".","|",getNames(xtemp)[i],fixed=TRUE)          
          }
          output[(i-1)*nregions + 1:nregions,"Unit"] <- unit[i]
          output[(i-1)*nregions + 1:nregions,5+1:nyears] <- round(xtemp[,,i],ndigit)
        }
        
        if(skipempty) {
          toskip <- which(rowSums(!is.na(output[,5+(1:nyears),drop=FALSE]))==0)
          if(length(toskip)>0) output <- output[-toskip,,drop=FALSE]
        }
        output[is.na(output)] <- "N/A"
        output[which(output=="NaN")]<-"N/A"
        if(is.null(file)) {
          print(output)
        } else {
          if(!file.exists(file)) append <- FALSE
          if(ii > 1) append <-TRUE
          if(append) {
            #check header for consistency
            header <- read.table(file, nrows = 1, sep = ";", stringsAsFactors = FALSE) 
            years1 <- as.numeric(header[sapply(header,is.numeric)])
            years2 <- as.numeric(colnames(output)[!is.na(suppressWarnings(as.numeric(colnames(output))))])
            union <- sort(union(years1,years2))
            addycols <- function(data,years) {
              ycols <- !is.na(suppressWarnings(as.numeric(colnames(data))))
              tmp <- data[ycols]
              data <- data[!ycols]
              data[as.character(sort(years))] <- "N/A"
              data[names(tmp)] <- tmp 
              return(data)
            }
            if(length(union)>length(years1)) {
              data <- read.table(file, sep = ";", stringsAsFactors = FALSE, header = TRUE, check.names=FALSE)   
              data <- data[-length(data)]
              write.table(addycols(data,union),file,quote=FALSE,sep=";",row.names=FALSE,col.names=TRUE,append=FALSE,eol=";\n")
            } 
            if(length(union)>length(years2)) {
              output <- addycols(as.data.frame(output),union)
            }
          }
          write.table(output,file,quote=FALSE,sep=";",row.names=FALSE,col.names=!append,append=append,eol=";\n")
          ii<-ii+1
        }
        unit<-unitdef
      }
    }

    

  }
}
