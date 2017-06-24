#' Write file in specific project format
#' 
#' Reads in a reporting.mif or uses a magpie object based on a read in
#' reporting.mif, substitutes names of variables according to the mappping,
#' mutliplies by an optional factor in the 3rd column of the mapping, and saves
#' the output in a new *.mif
#' 
#' 
#' @param mif Lists with magpie-objects or a magpie-object as created by read.report or a path to
#' a report.mif
#' @param mapping mapping of the varialbe names of the read in mif. the header
#' is used for naming.
#' @param file name of the project specipic report, default=NULL means that the names of the header of the reporting is used
#' @param max_file_size maximum file size in MB; if size of file exceeds max_file_size reporting is split into multiple files
#' @param ... arguments passed to write.report and write.report2
#' @author Christoph Bertram, Lavinia Baumstark, Anastasis Giannousakis, Florian Humpenoeder
#' @seealso \code{\link{write.report}}
#' @examples
#' 
#' \dontrun{
#' write.reportProject("REMIND_generic_test.mif","Mapping_generic_ADVANCE.csv")
#' }
#' 
#' @export write.reportProject
#' @importFrom utils read.csv2
#' 
write.reportProject <- function(mif,mapping,file=NULL,max_file_size=NULL,...){
  if(is.character(mif)){
    data <- read.report(mif,as.list=TRUE)
  } else if (is.list(mif)){
    data <- mif
  } else if (is.magpie(mif)){
    scenario <- getNames(mif,dim=1)
    model    <- getNames(mif,dim=2)
    data <- list()
    for (s in scenario){
      for (m in model) {
        data[[s]][[m]] <- collapseNames(mif[,,s][,,m])
      }
    }
   } else {
    stop("please provide either a path to a mif-file, a read in mif-file (in list-structure or as a magpie object)")
  }
  # read in mapping of the names of variables for the project, handle NAs
  map <- read.csv2(mapping,colClasses="character")
  map <- sapply(X = map,FUN = function(x) gsub("N/A","NA",x,fixed = T))
  map <- as.data.frame(map,stringsAsFactors = FALSE)
  
  
  missingc <- c()
  # select variables and change names of reported variables
  new_data <- list()
  for (n in names(data)){
    for (m in names(data[[n]])){
      ind <- which(map[,names(map)[1]]  %in% intersect(map[,names(map)[1]],getNames(data[[n]][[m]])))
      if ("factor" %in% names(map)) {
        if (unique(map$factor)=="") map$factor <- 1
        new_data[[n]][[m]] <- setNames(mbind(lapply(map[ind,names(map)[1]],function(x) as.numeric(map[which(map[,names(map)[1]]==x),"factor"])*(data[[n]][[m]][,,x]))),map[ind,names(map)[2]])
      } else {
        new_data[[n]][[m]] <- setNames(mbind(lapply(map[ind,names(map)[1]],function(x) (data[[n]][[m]][,,x]))),map[ind,names(map)[2]])
      }
      
        if (length(setdiff(map[,names(map)[1]],getNames(data[[n]][[m]]))) !=0) {
          missingc <- c(missingc,setdiff(map[,names(map)[1]],getNames(data[[n]][[m]])))
        }
      }
  }
  if (length(missingc) !=0) warning(paste0("Following variables were not found in the generic data and were excluded: \"",paste(unique(missingc),collapse = "\", \""),"\""))
  if(!is.null(file)){
    file <- file
  } else {  
    # calculate name of new reporting
    file <- gsub(names(map)[1],names(map)[2],mif)
  }  
  # save project reporting
  write.report2(new_data,file=file,...)
  
  if (!is.null(max_file_size)) {
    file_size <- file.size(file)/10^6 #file size in MB
    if(file_size > max_file_size) {
      x <- read.report(file,as.list=FALSE)
      scen <- getNames(x,dim=1)
      n_scen <- length(scen)
      n_files <- ceiling(file_size / max_file_size)
      if (n_files > n_scen) {
        n_files <- n_scen
        warning("Minimum is one scenario per file!")
        }
      scen_per_file <- floor(length(scen)/n_files)
      first_scen <- 1
      for (f in 1:n_files) {
        print(paste0("File ",f))
        #prepare scenario subset
        last_scen <- (first_scen+scen_per_file-1)
        if (last_scen > n_scen) last_scen <- n_scen
        scen_subset <- scen[first_scen:last_scen]
        print(scen_subset)
        #subset data
        tmp <- x[,,scen_subset]
        #prepare file name
        file_name <- unlist(strsplit(file,"\\."))
        last <- length(file_name)
        #write report
        write.report2(tmp,file=paste0(file_name[1:last-1],"_part",f,".",file_name[last]),...)
        #set counter for next loop
        first_scen <- first_scen+scen_per_file
      }
    }
  }
}


