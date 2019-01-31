#' set_magpie_units (!experimental!)
#' 
#' A pipe-friendly version of units<-.magpie. Extension of set_units from the units package to MAgPIE objects.
#' 
#' @param x MAgPIE object
#' @param value object of class units, a character of length one coercible to units via as_units, or a MAgPIE object
#' @return MAgPIE object x converted to given unit (if possible)
#' @author Stephen Bi
#' @seealso \code{\link[units]{set_units}}
#' 
#' @export
#' 
set_magpie_units <- function(x, value) {
  unit <- getMetadata(x,"unit")
  value <- install_magpie_units(value)
  if (is.magpie(value)) {
    value <- units(value)
  }
  if (!are_units_convertible(unit,value)) {
    stop(units(unit)," cannot be converted to ",units(value),"!")
  }
  multiplier <- as.numeric(unit/value)
  getMetadata(x,"unit") <- value
  return(x*multiplier)
}