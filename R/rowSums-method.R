#' ~~ Methods for Function rowSums and rowMeans ~~
#' 
#' ~~ Methods for function \code{rowSums} and \code{rowMeans}~~
#' 
#' 
#' @name rowSums-methods
#' @aliases rowSums-methods rowSums,ANY-method rowSums,magpie-method
#' rowMeans-methods rowMeans,ANY-method rowMeans,magpie-method
#' @param x object on which calculation should be performed
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations?
#' @param dims integer: Which dimensions are regarded as "rows" or "columns" to sum over. For row*, 
#' the sum or mean is over dimensions dims+1, ...; for col* it is over dimensions 1:dims.
#' @param ... further arguments passed to other colSums/colMeans methods
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(x = \"ANY\")")}{ normal rowSums and rowMeans method }
#' 
#' \item{list("signature(x = \"magpie\")")}{ classical method prepared to
#' handle MAgPIE objects } }
#' @keywords methods ~~ other possible keyword(s) ~~
#' @exportMethod rowSums

setMethod("rowSums",
          signature(x = "magpie"),
          function (x, na.rm = FALSE, dims = 1, ...) 
          {
            x <- rowSums(as.array(x), na.rm=na.rm, dims=dims, ...)
            return(as.magpie(as.array(x),spatial=1))
          }
          )