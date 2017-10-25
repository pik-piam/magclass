#' Write file in specific project format
#' 
#' Reads in a reporting.mif or uses a magpie object based on a read in
#' reporting.mif, substitutes names of variables according to the mappping,
#' mutliplies by an optional factor in a column named "factor" of the mapping, and saves
#' the output in a new *.mif
#' 
#' 
#' @param mif Lists with magpie-objects or a magpie-object as created by read.report or a path to
#' a report.mif
#' @param mapping mapping of the variable names of the read in mif. the header
#' is used for naming. The format of the mapping should be: 1st column the standard naming in PIK mif format. X further columns that contain the indicator names in the reporting format. Can also contain several indicator columns (e.g Variable and Item).
#' Optional columns with reserved names are unit, weight, and factor.
#' Factor is a number that the results will be multplied with (e.g. to transform CO2 into C)
#' Weight is needed if several mif indicators shall be aggregated to one reporting indicator. You always need a weight column if you have multiple mif to one reporting mappings. If you have a weight column, you have to have values in it for all indicators. If NULL, the results are added up; if you provide an indicator name (of a mif indicator), this indicator will be used for the weighting of a weighted mean.
#' Unit is a name of the unit without ()
#'   Example:
#' "mif";"agmip";"Item";"unit";"weight";"factor"
#' "Nutrition|+|Calorie Supply (kcal/capita/day)";"CALO";"AGR";"kcal/capita/day";"NULL";1
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
  
  # if non existent, add factor column
  if(!"factor" %in% names(map)) {
    map$factor<-1
  }
  # set missing values in factor column to 1
  map$factor[which(map$factor=="")]<-1
  

  
  # reorder mapping
  indicatorcols=which(!names(map) %in% c("factor","weight","unit"))
  if(!1 %in% indicatorcols) {stop("first column has to be reporting mif output indicator name")}
  # merge multiple indicator columns
  map2 <- data.frame(map[,1],apply(map[,setdiff(indicatorcols,1),drop=FALSE],MARGIN=1,paste,collapse="."),map[,-indicatorcols],stringsAsFactors = FALSE)
  map2<-map2[apply(map[,setdiff(indicatorcols,1),drop=FALSE],MARGIN=1,paste,collapse="")!="",]
  names(map2)<-c(names(map)[1],paste(names(map[,setdiff(indicatorcols,1),drop=FALSE]),collapse="."),names(map)[-indicatorcols])
  
  map<-map2
  remove(map2)

  
  missingc <- c()
  # select variables and change names of reported variables
  new_data <- list()
  
  # without aggregation
  if(!"weight"%in%names(map)) {
    if(length(unique(map[,2]))>length(map[,2])){stop("There exist multiple mif entries for each indicator, but no weight for aggregation has been provided")}
  
    for (n in names(data)){   # n: scenarios
      for (m in names(data[[n]])){  # m: models
        ind <- which(map[,names(map)[1]]  %in% intersect(map[,names(map)[1]],getNames(data[[n]][[m]])))
        
        tmp<- setNames(mbind(
          lapply(
            map[ind,names(map)[1]],
            function(x) {
              as.numeric(map[which(map[,names(map)[1],drop=FALSE]==x),"factor"])*(data[[n]][[m]][,,x])
            }
          )), map[ind,indicatorcols[-1]])
      
        # correct sets for multicolumn objects
        getSets(tmp)<-c(getSets(tmp)[1:3],strsplit(names(map)[2],"[.]")[[1]][-1])
        new_data[[n]][[m]] <- tmp
        
        tmp<-setdiff(map[,names(map)[1],drop=FALSE],getNames(data[[n]][[m]]))
        if (length(tmp) !=0) {
          missingc <- c(missingc,tmp)
        }
      }
      
    }
  
  } else {
  # with aggregation
  
    for (n in names(data)){   # n: scenarios
      for (m in names(data[[n]])){  # m: models
        if(length(fulldim(data[[n]][[m]])[[1]])>3){stop("data has more than 3 dimensions")}
        tmp<-new.magpie(cells_and_regions = getCells(data[[n]][[m]]), years = getYears(data[[n]][[m]]), names = unique(map[,2]))
        
        for (ind_x in unique(map[,2])){
          mapindex=which(map[,2]==ind_x)
          weight_x=map$weight[mapindex]
          factor_x=setNames(as.magpie(as.numeric(map$factor[mapindex])),map[mapindex,1])
          original_x=map[mapindex,1]
          
          if(!all(original_x %in% getNames(data[[n]][[m]]))) {stop(paste0("Indicator ", original_x[which(!original_x%in%getNames(data[[n]][[m]]))]," missing in data but exists in mapping"))}
            
          if (any(weight_x=="")){
            #wenn Gewicht "" dann error
            stop(paste0("empty weight for indicator "),ind_x)
          } else if (all(weight_x != "NULL")){ 
            #wenn Gewicht vorhanden dann average
            # average: by(data = b,INDICES = b[,2],FUN = function(x){sum(x$breaks*x$test)/sum(x$test)})
            tmp[,,ind_x]<-  dimSums(data[[n]][[m]][,,original_x]*factor_x*setNames(data[[n]][[m]][,,weight_x],original_x),dim=3.1)/dimSums(setNames(data[[n]][[m]][,,weight_x],original_x),dim=3.1)
          } else if (all(weight_x=="NULL")){
            #wenn gewicht NULL dann summation
            tmp[,,ind_x] <- dimSums(data[[n]][[m]][,,original_x]*factor_x,dim=3.1)
          } else {
            stop(paste0("mixture of weights between NULL and parameters for indicator "),ind_x)
            #wenn Gewicht mischung aus NULL und "" dann error
          }
        }
        
        getSets(tmp)<-c(getSets(tmp)[1:3],strsplit(names(map)[2],"[.]")[[1]][-1])
        if("unit"%in%names(map)) {
          getNames(tmp)<-paste0(getNames(tmp)," (",map$unit[match(getNames(tmp),map[,2])],")")
        }
        new_data[[n]][[m]] <- tmp

        tmp<-setdiff(map[,names(map)[1]],getNames(data[[n]][[m]]))
        if (length(tmp) !=0) {
          missingc <- c(missingc,tmp)
        }
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


