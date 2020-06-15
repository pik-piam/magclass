#' print
#' 
#' print method for MAgPIE objects for conventient display of magpie data.
#' 
#' 
#' @param x MAgPIE object
#' @param drop argument which controls whether empty dimensions should be
#' skipped or not.
#' @param reshape agrument that controls tabular representation of nested data dimension cross tables,
#'                FALSE will reproduce standard print behavior
#'                any pair of two dimension numbers will create a table for these two dims, and loop over the other dimensions
#'                  
#' @param ... arguments to be passed to or from other methods.
#' @return print displays the given MAgPIE object on screen.
#' @author Jan Philipp Dietrich, Kristine Karstens, Felicitas Beier
#' @seealso \code{\link[base]{print}}
#' @examples
#' 
#'   data(population_magpie)
#'   print(population_magpie)
#'   print(population_magpie[,1,], drop=FALSE)
#'   print(population_magpie[,1,])
#' 
#' @export

print.magpie <- function(x, drop=TRUE, reshape=FALSE, ...) {
  
  if(length(reshape)!=2 | !is.vector(reshape, mode="numeric")) reshape <- FALSE
  if(any(!reshape)){  
    
    p <- print(as.array(x)[,,,drop=drop], ...)
    
  } else {
    
    if(drop){
      
      dims <- as.numeric(substring(names(getSets(x)),2))
      names(dims) <- getSets(x)
      reshape[reshape%%1==0] <- reshape[reshape%%1==0] +0.1
      
      #check if specified subdims exists in x
      if(!all(reshape %in% dims)){
        warning("Non-existing subdims specified in 'reshape'. Call print again with 'reshape=FALSE'.")
        print.magpie(x, reshape=FALSE, drop=drop)
      }
      
      names(reshape) <- c(names(dims[dims==reshape[1]]),names(dims[dims==reshape[2]]))
      loop           <- setdiff(dims, reshape)
      names(loop)    <- setdiff(names(dims), names(reshape))
      
      header         <- array(dim=3)
      names(header)  <- sapply(c(1:3), function(dim, i) paste(names(dim[round(dim)==i]),collapse="."), dim=loop)
      
      
      nested_loop    <- function(toPrint, missing, header){
        
        i                      <- missing[1]
        missing                <- tail(missing, length(missing)-1)
        
        for(j in getItems(toPrint, dim=i)){
          
          if(round(i)==1){        toPrintReduced <- toPrint[j,,] 
          } else if(round(i)==2){ toPrintReduced <- toPrint[,j,]
          } else if(round(i)==3){ toPrintReduced <- toPrint[,,j]
          } else stop("Something is wrong.")
          
          if(is.na(header[trunc(i)])){
            header[trunc(i)] <- j
          } else {
            header[trunc(i)] <- paste(c(header[trunc(i)],j), collapse=".")
          }
          
          if(length(missing)!=0){ 
            
            nested_loop(toPrintReduced, missing, header)
            
          } else {
            
            header[is.na(header)] <- " "
            writeLines(paste(paste(names(header), collapse = ", "), " = ",
                             paste(header,        collapse = ", ")), sep="\n\n")
            writeLines(paste("\t", names(reshape[1])))
            p   <- print(reshape(subset(as.data.frame(toPrintReduced, rev=2), select=c(names(reshape),".value")), 
                                 timevar=names(reshape[1]), idvar=names(reshape[2]),
                                 direction="wide",
                                 new.row.names = getItems(toPrintReduced, dim=reshape[2]),
                                 varying = list(getItems(toPrintReduced, dim=reshape[1]))), row.names=FALSE)
            writeLines("")
          }
          
          if(grepl("\\.",header[trunc(i)])){
            header[trunc(i)] <- gsub(".([^\\.]+$)","", header[trunc(i)])
          } else{
            header[trunc(i)] <- NA
          }
        }
      }
      
      nested_loop(x, loop, header)  
      
    } else {
      warning("reshape option can just be used with drop=TRUE. Execute with reshape=FALSE:")
      print.magpie(x=x, drop=drop, reshape=FALSE, ...)
    }
  }
  
  unit <- getMetadata(x,"unit")
  if(!is.null(unit)) {
    factor <- ifelse(as.numeric(unit)!=1,paste0(as.character(unit),"*"),"")
    cat("Unit: ",factor,as.character(attr(unit, "units")),"\n", sep="")
  }
}
