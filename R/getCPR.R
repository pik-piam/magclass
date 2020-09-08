#' Get cells per region
#' 
#' Counts how many cells each region has and returns it as vector
#' 
#' 
#' @param x MAgPIE object or a resolution written as numeric (currently only
#' data for 0.5 degree resolution is available).
#' @return cells per region
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getRegions}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}}
#' @examples
#' 
#' # a <- read.magpie("example.mz")
#' # getCPR(a)
#' getCPR(0.5)
#' 
#' @export getCPR
getCPR <- function(x) {
  if(!is.magpie(x)) {
    if(x==0.5) {
      region.code <- magclassdata$half_deg$region
      cpr <- rep(0,length(levels(region.code)))
      names(cpr) <- levels(region.code)
      for(region in names(cpr)) {
       cpr[region] <- length(grep(region,region.code))  
      }
    } else {
      stop(paste("No cells-per-region information available for resolution",x))
    }
  } else {
    # try to reorder by cell/cluster number
    cells <- try(as.integer(getItems(x,1.2)),silent = TRUE)
    if(class(cells)!="try-error" && !anyNA(cells)) x <- x[order(cells),,]
    
    region_names <- getRegions(x)
    cpr <- rep(0,length(region_names))
    names(cpr) <- region_names
    for(region in region_names) {
      cpr[region] <- length(grep(region,dimnames(x)[[1]]))  
    }
  }
  return(cpr)
}
