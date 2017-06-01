
#' ~~ Methods for Function as.array ~~
#' 
#' ~~ Methods for function \code{as.array} ~~
#' 
#' 
#' @name as.array-methods
#' @aliases as.array-methods as.array,ANY-method as.array,magpie-method
#' @param x object which should be converted to an array
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(x = \"ANY\")")}{ standard as.array-method }
#' 
#' \item{list("signature(x = \"magpie\")")}{ Conversion takes place just by
#' removing MAgPIE-object specific elements } }
#' @keywords methods
#' @importFrom methods as setMethod signature
#' @exportMethod as.array

setMethod("as.array",
    signature(x = "magpie"),
    function (x) 
    {      
      return(as(x,"array"))
    }
)