
#' ~~ Methods for Function as.raster ~~
#' 
#' ~~ Methods for function \code{as.raster} ~~
#' 
#' 
#' @name as.raster-methods
#' @aliases as.raster-methods as.raster,ANY-method as.raster,magpie-method
#' @param x object which should be converted to an raster
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(x = \"ANY\")")}{ standard as.raster-method }
#' 
#' \item{list("signature(x = \"magpie\")")}{ Conversion takes place just by
#' removing MAgPIE-object specific elements } }
#' @keywords methods
#' @importFrom methods as setMethod signature
#' @exportMethod as.raster

setMethod("as.raster",
    signature(x = "magpie"),
    function (x) 
    {  
      if (!requireNamespace("raster", quietly = TRUE)) stop("The package \"raster\" is required for conversion of raster objects!")
      
      return(as(x,"raster"))
    }
)