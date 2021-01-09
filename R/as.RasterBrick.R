#' as.RasterBrick
#' 
#' Convert magclass object to a RasterBrick object
#' 
#' 
#' @param x MAgPIE object
#' @param res spatial data resolution. If not provided it will be guessed.
#' @return A RasterBrick object
#' @author Jan Philipp Dietrich
#' @export


as.RasterBrick <- function (x, res=NULL) {  
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
      
      xy <- getCoords(x)
      if(is.null(res)) res <- .guessRes(xy)
      out <- raster::brick(ncols=360/res,nrows=180/res, nl=nyears(x)*ndata(x))
      m   <- wrap(x, list(1,2:3), sep="..")
      out[raster::cellFromXY(out,xy)] <- m 
      names(out) <- colnames(m)
      return(out)
}




