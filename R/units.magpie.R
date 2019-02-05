#' units
#' 
#' units method for MAgPIE objects to update the unit of the object
#' 
#' 
#' @aliases units<-.magpie units.magpie
#' @param x MAgPIE object
#' @param value object of class units or character of length one coercible to class units via as_units
#' @return MAgPIE object converted to given unit (if possible)
#' @author Jan Philipp Dietrich, Stephen Bi
#' @seealso \code{\link[units]{units}}
#' 
#' @export 
#' 
"units<-.magpie" <- function (x, value) {
  unit <- getMetadata(x,"unit")
  value <- install_magpie_units(value)
  if (!are_units_convertible(unit,value)) {
    stop(units(unit)," cannot be converted to ",units(value),"!")
  }
  multiplier <- as.numeric(unit/value)
  getMetadata(x,"unit") <- value
  return(x*multiplier)
}

#' @export 
units.magpie <- function(x) {
  return(getMetadata(x,"unit"))
}
