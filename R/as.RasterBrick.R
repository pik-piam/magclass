#' as.RasterBrick
#' 
#' Convert magclass object to a RasterBrick object
#' 
#' 
#' @param x MAgPIE object
#' @param res spatial data resolution. If not provided it will be guessed.
#' @return A RasterBrick object
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getCoords}}
#' @examples
#' 
#' if (requireNamespace("raster", quietly = TRUE)) {
#'    r <- raster::brick(ncols=360,nrows=180, nl=4)
#'    r[85:89,176:179] <- (1:20 %*% t(1:4))
#'    names(r) <- c("y2000..bla","y2001..bla","y2000..blub","y2001..blub")
#'    m <- as.magpie(r)
#'    r2 <- as.RasterBrick(m)
#' } 
#' 
#' @export


as.RasterBrick <- function (x, res=NULL) {  
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
      names(out) <- colnames(m)
      colnames(m) <- NULL
      out[raster::cellFromXY(out,xy)] <- m 
      return(out)
}




