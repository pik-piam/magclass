#' magpieResolution
#' 
#' Returns the Resolution of a MAgPIE object
#' 
#' 
#' @param object An MAgPIE object
#' @return "glo", "reg" or "cell"
#' @author Benjamin Bodirsky
#' @seealso \code{\link{maxample}}
#' @examples
#' 
#' magpieResolution(maxample("pop"))
#' 
#' @export magpieResolution
magpieResolution<- function(object) {
  if(!is.magpie(object)){stop("Object is no magpie object")
  } else {
    n_magpie_regions <-length(getRegions(object))
    n_magpie_cells    <-dim(object)[[1]]
    if (n_magpie_cells==1) { 
      resolution<-"glo"
    } else if(n_magpie_cells==n_magpie_regions) {
      resolution<-"reg"
    } else {
      resolution<-"cell"
    }    
  }
  return(resolution)
}
