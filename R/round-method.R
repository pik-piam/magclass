#' Round-method for MAgPIE objects
#' 
#' Round-method for MAgPIE-objects respectively. Works exactly as for arrays.
#' 
#' 
#' @name round-methods
#' @aliases round-methods round,magpie-method
#' @param x a magpie object
#' @param digits integer indicating the number of decimal places (round) or significant
#' digits (signif) to be used. Negative values are allowed. 
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{x = "magpie"}{ works as round(x) for arrays. }
#' 
#' }
#' @exportMethod round


setMethod("round",
    signature(x = "magpie"),
    function (x, digits=0) 
    {      
        x@.Data <- round(x@.Data,digits=digits)
        return(x)
    }
)