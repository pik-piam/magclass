#' coord
#' 
#' Extracts coordinates from a magpie object. Only works for objects
#' which provide coordinates as spatial dimensions with the names "lon"
#' and "lat"!
#' 
#' Please note that "." have to be replaced with "," in lon/lat 
#' coordinates as "." is reserved as divider between subdimensions.
#' 
#' @param x MAgPIE object
#' @return data frame with columns "lon" and "lat" containing
#' the coordinates of all spatial cells.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getItems}}
#' @examples
#' m <- new.magpie(c("0,5.0,5","0,5.-0,5","0,5.1,0","1,0.1,0"),sets=c("lon","lat","year","data"))
#' coord(m)
#' @export

coord <- function(x) {
  dim <- dimCode(c("lon","lat"),x)
  if(any(dim==0)) stop("Coordinates cannot be returned as not lon/lat information was found!")
  if(any(dim>=2)) stop("lon/lat information need to be part of the spatial dimension of the object!")
  coord <- getItems(x,c("lon","lat"),full=TRUE)
  coord$lon <- as.numeric(sub(",",".",coord$lon,fixed=TRUE))
  coord$lat <- as.numeric(sub(",",".",coord$lat,fixed=TRUE))
  return(as.data.frame(coord))
}