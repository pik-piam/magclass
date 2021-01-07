#' as.RasterLayyer
#' 
#' Convert magclass object to a RasterLayer object
#' 
#' 
#' @param x MAgPIE object
#' @param res spatial data resolution. If not provided it will be guessed.
#' @return A RasterLayer object
#' @author Jan Philipp Dietrich
#' @export


as.RasterLayer <- function (x, res=NULL) {  
      warning("Still under development! Not for productive use!")
      if (!requireNamespace("raster", quietly = TRUE)) stop("The package \"raster\" is required for conversion of raster objects!")

      .guessRes <- function(xy) {
        .tmp <- function(x) {
          return(suppressWarnings(min(diff(sort(unique(x))))))
        }
        guess <- min(.tmp(xy[[1]]),.tmp(xy[[2]]))
        if(is.infinite(guess)) guess <- 0.5
        return(guess)
      }
      
      .position <- function(xy,res,e) {
        ncol <- (e@xmax-e@xmin)/res
        nrow <- (e@ymax-e@ymin)/res
        x <- (xy$x + res/2 - e@xmin)/res
        y <- nrow - (xy$y + res/2 - e@ymin)/res 
        return(x + ncol*y)
      }
      
      xy <- getCoords(x)
      if(is.null(res)) res <- .guessRes(xy)
      out <- raster::raster(res=res)
      out[.position(xy,res,raster::extent(out))] <- as.vector(x)
      return(out)
}




