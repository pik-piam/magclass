#' old_dim_convention
#' 
#' Transforms new dim convention (e.g. 3.2) into old dim convention(e.g. 4)
#' 
#' 
#' @param dim The dim number in the new convention
#' @return The dim number according to the old convention
#' @author Benjamin Bodirsky
#' @seealso \code{\link{add_columns}},\code{\link{add_dimension}}
#' @examples
#' 
#'   dim=old_dim_convention(3.2)
#'   dim=old_dim_convention(1.1)
#' 
#' @export old_dim_convention
old_dim_convention<-function(dim){
  dim<-as.character(dim)
  elemsplit <- as.numeric(as.vector(strsplit(dim,".",fixed=TRUE)[[1]]))
  if (length(elemsplit)==1) {stop("Format has to be x.y")}
  if (elemsplit[1]==1) {
    if (elemsplit[2]==1){newdim=1} else {stop("old dim convention has only 1.1, 2.1 and 3.x")}
  } else if (elemsplit[1]==2) {
    if (elemsplit[2]==1){newdim=2} else {stop("old dim convention has only 1.1, 2.1 and 3.x")}
  } else if (elemsplit[1]==3) {
    if (elemsplit[2]==0) {stop("3.0 not supported")}
    newdim=2+elemsplit[2]
  } else {stop("dim cannot be higher than 3.x")}
  return(newdim)
}
