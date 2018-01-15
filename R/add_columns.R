#' add_columns
#' 
#' Function adds new columns to the existing magpie object. The new columns are
#' filled with NAs.
#' 
#' 
#' @param x MAgPIE object which should be extended.
#' @param dim The number of the dimension that should be extended
#' @param addnm The new columns within dimension "dim"
#' @return The extended MAgPIE object
#' @author Benjamin Bodirsky
#' @seealso \code{\link{add_dimension}},\code{\link{mbind}}
#' @examples
#' 
#'  data(population_magpie)
#'  a <- add_columns(population_magpie)
#'  str(a)
#'  fulldim(a)
#' 
#' @export add_columns
add_columns<-function(x,addnm=c("new"),dim=3.1){
  if(length(addnm)==0) return(x)
  dim=old_dim_convention(dim)
  if (dim==1) {
    new_columns<-x[rep(1,length(addnm)),,]
    new_columns<-setCells(new_columns,paste(substr(addnm,1,3),".",(dim(x)[dim]+1):(dim(x)[dim]+length(addnm)),sep=""))
    new_columns[,,]<-NA
  } else if (dim==2) {
    new_columns<-x[,rep(1,length(addnm)),]
    new_columns<-setYears(new_columns,addnm)
    new_columns[,,]<-NA
  } else if (dim>2) {
    new_columns<-x[,,fulldim(x)[[2]][[dim]][[1]]]
    getNames(new_columns,dim=dim-2)<-addnm[1]
    if(length(addnm)>1){
      single_column_x<-new_columns
      for (i in 2:length(addnm)){
        getNames(single_column_x,dim=dim-2)<-addnm[i]
        new_columns<-mbind(new_columns,as.magpie(single_column_x))
      }}
    new_columns[,,]<-NA
  }
  output <- mbind(x,new_columns)
  return(output)
}
