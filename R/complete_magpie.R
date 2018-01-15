#' complete_magpie
#' 
#' MAgPIE objects can be incomplete to reduce memory. This function blows up a
#' magpie object to its real dimensions, so you can apply unwrap.
#' 
#' 
#' @param x MAgPIE object which should be completed.
#' @param fill Value that shall be written into the missing entries
#' @return The completed MAgPIE object
#' @author Benjamin Bodirsky
#' @seealso \code{\link{add_dimension}},\code{\link{clean_magpie}}
#' @examples
#' 
#'  data(population_magpie)
#'  a <- complete_magpie(population_magpie)
#'  b <- add_dimension(a)
#'  c <- add_dimension(a,nm="dummy2")
#'  incomplete<-mbind(b[,,1],c)
#'  d<-complete_magpie(incomplete)
#' 
#' @export complete_magpie
complete_magpie<-function(x,fill=NA) {
  full<-fulldim(x)[[2]]
  permute<-full[[3]]
  repeatit<-length(permute)
  if (length(full)>3) {
    for (i in 4:length(full)) {
      permute<-paste(rep(permute,each=length(full[[i]])),full[[i]],sep=".")
      repeatit<-length(permute)
    }
  }
  missing<-permute[!(permute%in%dimnames(x)[[3]])]
  if(length(missing)>0){
    add<-new.magpie(cells_and_regions = full[[1]],years = full[[2]],names = missing,fill=fill)
    out<-mbind(x,add)
  } else {out<-x}
  out<-out[,,order(getNames(out))]
  getMetadata(out) <- getMetadata(x)
  return(out)
}
