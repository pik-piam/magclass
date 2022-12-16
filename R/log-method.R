#' log-method for MAgPIE objects
#'
#' log-method for MAgPIE-objects respectively. Works exactly as for arrays.
#'
#'
#' @name log-methods
#' @aliases log-methods log,magpie-method logb-methods logb,magpie-method
#' @param x a magpie object
#' @param base ia positive or complex number: the base with respect to which
#' logarithms are computed. Defaults to e=exp(1).
#' @docType methods
#' @section Methods: \describe{
#'
#' \item{x = "magpie"}{ works as log(x) for arrays. }
#'
#' }
#' @exportMethod log


setMethod("log",
    signature(x = "magpie"),
    function(x, base = exp(1)) {
        x@.Data <- log(x@.Data, base = base)
        return(x)
    }
)

#' @exportMethod logb

setMethod("logb",
          signature(x = "magpie"),
          function(x, base = exp(1)) {
            x@.Data <- log(x@.Data, base = base)
            return(x)
          }
)
