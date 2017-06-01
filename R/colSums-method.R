#' ~~ Methods for Function colSums and colMeans ~~
#' 
#' ~~ Methods for function \code{colSums} and \code{colMeans} ~~
#' 
#' 
#' @name colSums-methods
#' @aliases colSums-methods colSums,ANY-method colSums,magpie-method
#' colMeans-methods colMeans,ANY-method colMeans,magpie-method
#' @param x object on which calculation should be performed
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations?
#' @param dims integer: Which dimensions are regarded as "rows" or "columns" to sum over. For row*, 
#' the sum or mean is over dimensions dims+1, ...; for col* it is over dimensions 1:dims.
#' @param ... further arguments passed to other colSums/colMeans methods
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(x = \"ANY\")")}{ normal colSums and colMeans method }
#' 
#' \item{list("signature(x = \"magpie\")")}{ classical method prepared to
#' handle MAgPIE objects } }
#' @keywords methods ~~ other possible keyword(s) ~~
#' @importFrom methods new
#' @exportMethod colSums
#' 
setMethod("colSums",
          signature(x = "magpie"),
          function (x, na.rm = FALSE, dims = 1, ...) 
          {
            x_array<-as.array(x)
            x_glo<-colSums(x_array,na.rm=na.rm,...)
            x<-new("magpie",array(x_glo,dim=c(1,dim(x_glo)),dimnames=c("GLO",dimnames(x_glo))))
            return(x)
          }
          )

