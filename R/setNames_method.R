#' Get dataset names
#' 
#' Extracts dataset names of a MAgPIE-object
#' 
#' setNames is a shortcut to use a MAgPIE object with manipulated data names.
#' The setNames method uses the variable names "object" and "nm" in order to be
#' consistent to the already existing function setNames.
#' 
#' @name setNames-methods
#' @aliases setNames setNames,magpie-method
#' setNames,NULL-method
#' @param object MAgPIE object
#' @param nm a vector of names current names should be replaced with. If
#' only one data element exists you can also set the name to NULL.
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(object = \"ANY\")")}{ normal setNames method }
#' 
#' \item{list("signature(object = \"magpie\")")}{ setNames for MAgPIE objects} }
#' @seealso \code{\link{getNames}}, 
#' @keywords methods
#' @exportMethod setNames

setMethod("setNames",
    signature(object = "magpie"),
    function (object, nm) 
    {
        getNames(object) <- nm
        return(object)
    }
)
