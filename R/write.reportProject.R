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
#' @author Christoph Bertram, Lavinia Baumstark, Anastasis Giannousakis
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
write.reportProject <- function(mif,mapping,file=NULL){
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
  write.report(new_data,file=file)
}


