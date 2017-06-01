#' add_dimension
#' 
#' Function adds a name dimension as dimension number "dim" with the name "add"
#' with an empty data column with the name "nm".
#' 
#' 
#' @param x MAgPIE object which should be extended.
#' @param dim The dimension number of the new dimension. 4 stands for the
#' second name dimension.
#' @param add The name of the new dimension
#' @param nm The name of the first entry in dimension "add".
#' @return The extended MAgPIE object
#' @author Benjamin Bodirsky
#' @seealso \code{\link{add_columns}},\code{\link{mbind}}
#' @examples
#' 
#'  data(population_magpie)
#'  a <- add_dimension(population_magpie)
#'  str(a)
#'  fulldim(a)
#' 
#' @export add_dimension
add_dimension<-function(x,dim=3.1,add="new", nm="dummy"){
  x<-clean_magpie(x)
  dim<-as.numeric(dim)
  olddim<-old_dim_convention(dim)
  if (olddim<3) stop("Dimensions below 3 are currently not supported by add_dimensions.")
  if (is.null(getNames(x))){getNames(x)<-"NA"}
  firstnm<-nm[1]
  
  separate<-strsplit(dimnames(x)[[3]],split = "\\.")
  newnm<-NULL
  for (i in 1:length(separate)) {
    newnm<-c(newnm,paste(append(separate[[i]],firstnm,after=olddim-3),collapse="."))
  }
  dimnames(x)[[3]]<-newnm
  names(dimnames(x))[3] <- paste(append(
    strsplit(names(dimnames(x))[3],split="\\.")[[1]]
    ,add,after=olddim-3),collapse=".")

  if (length(nm)>1) {
    x<-add_columns(x,addnm = nm[2:length(nm)],dim=dim)
    tmp<-list(neu=nm[1])
    names(tmp)<-add
    x[,,]<-mselect(x,tmp)
  }
  return(x)
}
