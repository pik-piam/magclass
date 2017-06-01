#' Write file in report format
#' 
#' This function writes the content of a MAgPIE object into a file or returns
#' it directly using the reporting format as it is used for many model
#' intercomparisons. It is a rewritten version of write.report and will
#' probably replace write.report somewhen in the future
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
#' data(population_magpie)
#' write.report2(population_magpie)
#' 
#' @importFrom utils write.table
#' @importFrom reshape2 dcast melt
#' @export
write.report2 <- function(x,file=NULL,model=NULL,scenario=NULL,unit=NULL,ndigit=4,append=FALSE,skipempty=TRUE) {
  if(is.list(x)) {
    if(is.list(x[[1]])) {
      for(scenario in names(x)){
        for(model in names(x[[scenario]])) {
          write.report2(x[[scenario]][[model]],file=file,model=model,scenario=scenario,unit=unit,ndigit=ndigit,append=append)  
          append <- TRUE
        }
      }  
    } else {
      stop("Wrong format. x must be either a list of lists or a MAgPIE object! Only single list found!")
    }
  } else {
    if(!is.magpie(x)) stop("Input is not a MAgPIE object!")
    prepare_data <- function(x, model=NULL, scenario=NULL, unit=NULL, skipempty=FALSE, ndigit=4) {
      sep <- "."
      # clean data
      x <- round(clean_magpie(x,what="sets"), digits = ndigit)
      names(dimnames(x))[1] <- "Region"
      dimnames(x)[[1]] <- sub("^GLO(\\.{0,1}[0-9]*)$","World\\1",dimnames(x)[[1]])
      dimnames(x)[[2]] <- substring(dimnames(x)[[2]],2)
      
      # convert to data frame
      x <- dcast(melt(x,as.is=TRUE,na.rm = skipempty),eval(parse(text=paste0("...~",names(dimnames(x))[2]))))
      
      # split data and dimension information
      data <- x[3:length(x)]
      x <- x[1:2]
      
      # split subdimensions
      colsplit <- function(x,col,sep=".") {
        if(all(grepl(sep,x[[col]],fixed=TRUE))) {
          tmp <- data.frame(t(matrix(unlist(strsplit(as.character(x[[col]]),split=sep,fixed=TRUE)),ncol=length(x[[col]]))),stringsAsFactors=FALSE)
          names(tmp) <- strsplit(names(x)[col],split=sep,fixed=TRUE)[[1]]
          x <- cbind(tmp,x[setdiff(1:ncol(x),col)])
        }  
        return(x)
      }
      for(i in grep(sep,names(x),fixed=TRUE)) x <- colsplit(x,i,sep=sep)
      
      unitsplit <- function(x,col) {
        w <- grepl("\\(.*\\)",x[[col]])
        x[[col]][!w] <- paste0(x[[col]][!w]," (N/A)")
        tmp <- data.frame(sub("^([^\\(]*) \\((.*)\\)$","\\1",x[[col]]),
                          sub("^([^\\(]*) \\((.*)\\)$","\\2",x[[col]]))
        names(tmp) <- c(names(x)[col],"unit")
        x <- cbind(tmp,x[setdiff(1:ncol(x),col)])
        return(x) 
      }
      for(i in 1:length(x)) {
        if(!(tolower(names(x)[i]) %in% c("scenario","model","region"))) {
          if(any(grepl(" \\(.*\\)$",x[i]))) x <- unitsplit(x,i)
        }
      }
      
      correct_names <- function(x,name="Scenario", replacement=NULL) {
        if(is.null(replacement)) replacement <- "N/A"
        w <- which(tolower(names(x))==tolower(name))
        if(length(w)==0) {
          x <- cbind(replacement,x)
        } else if(length(w)==1) {
          x <- cbind(x[w],x[-w])
        } else {
          warning("Found ",name," more than once! First occurrence will be used")
          w <- w[1]
          x <- cbind(x[w],x[-w])
        }
        names(x)[1] <- name
        return(x)
      }
      
      x <- correct_names(x,name="Unit", replacement=unit)
      x <- correct_names(x,name="Region", replacement=NULL)
      x <- correct_names(x,name="Scenario", replacement=scenario)
      x <- correct_names(x,name="Model", replacement=model)
      
      if(length(x)==4) {
        tmp <- "N/A"
      } else {
        tmp <- eval(parse(text=paste0("paste(",paste0("x[[",5:length(x),"]]",collapse=", "),", sep=sep)")))
      }
      x <- cbind(x[1:3],Variable=tmp,x[4])
      
      data[is.na(data)] <- "N/A"
      
      return(cbind(x,data))
    }
    x <- prepare_data(x, model=model, scenario=scenario, unit=unit, skipempty = skipempty, ndigit=ndigit)
    if(is.null(file)) {
      print(x)
    } else {
      if(!file.exists(file)) append <- FALSE
      if(append) {
        #check header for consistency
        header <- read.table(file, nrows = 1, sep = ";", stringsAsFactors = FALSE) 
        years1 <- as.numeric(header[sapply(header,is.numeric)])
        years2 <- as.numeric(colnames(x)[!is.na(suppressWarnings(as.numeric(colnames(x))))])
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
          x <- addycols(as.data.frame(x),union)
        }
      }
      write.table(x,file,quote=FALSE,sep=";",row.names=FALSE,col.names=!append,append=append,eol=";\n")
    }
  }
}

