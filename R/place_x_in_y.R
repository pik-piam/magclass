#' place_x_in_y
#' 
#' Function positions magpie object x into magpie object y.
#' 
#' 
#' @param x Object to be placed.
#' @param y Object in which x shall be placed
#' @param expand T: if x is larger than y, new columns are added.
#' @return The combination of x and y. x overwrites y values which are in the
#' same place.
#' @author Benjamin Bodirsky
#' @seealso
#' \code{\link{add_dimension}},\code{\link{add_columns}},\code{\link{mbind}}
#' @examples
#' 
#'  data(population_magpie)
#'  x <- population_magpie[,"y1995",]*0.2
#'  a <- place_x_in_y(x, population_magpie)
#' 
#' @export place_x_in_y
place_x_in_y<-function(x,y,expand=T){
  if(!all(getYears(x)%in%getYears(y))) {
    if (expand){
      print("x has years that dont exist in y. Expand y.")
      y<-add_columns(x = y,dim = 2.1,addnm = setdiff(getYears(x),getYears(y)))
      y<-magpiesort(y)
    }else{
      x<-x[,getYears(x)[getYears(x)%in%getYears(y)],]
    }
  }
  if(!all(getRegions(x)%in%getRegions(y))) {
    if (expand){
      print("x has regions that dont exist in y. Expand y.")
      y<-add_columns(x = y,dim = 2.1,addnm = setdiff(getYears(x),getYears(y)))
      y<-magpiesort(y)
    } else {
      x<-x[getRegions(x)[getRegions(x)%in%getRegions(y)],,]
    }
  }
  if(!all(getNames(x)%in%getNames(y))) {
    if (expand){
      stop("x has names that dont exist in y. Cannot handle this yet. Please improve me!")}
  } else {
    x<-x[,,getNames(x)[getNames(x)%in%getNames(y)]]
  }
  y[getRegions(x),getYears(x),getNames(x)]<-x
  y <- updateMetadata(y,x,source='merge',calcHistory='update')
  return(y)
}
