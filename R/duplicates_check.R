# full import of data.table package is required as there seems to be no other option
# to include the data.table method for the duplicated function (which is considerably faster
# than the data.frame counterpart!)
#' @import data.table

.duplicates_check <- function(coord) {
  duplicates <- duplicated(as.data.table(coord))
  if(any(duplicates)) {
    coord <- as.data.frame(coord)
    warning("Duplicate entries found, only the last entry will be used (duplicate entries: ",paste(apply(rbind(NULL,unique(coord[duplicates,])),1,paste,collapse="|"),collapse=", "),")!")    
  } 
}
