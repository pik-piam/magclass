#' set_units
#' 
#' set_units method for MAgPIE objects to update the unit of the object
#' 
#' 
#' @aliases set_units.magpie
#' @param x MAgPIE object
#' @param value object of class units or symbolic_units, or in the case of set_units 
#' expression with symbols that can be resolved in \code{\link[units]{ud_units}}.
#' @param ... ignored
#' @return magpie object converted to given unit (if possible)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link[units]{set_units}}
#' @examples
#' library(units)
#' set_units(population_magpie,"1000")
#' 
#' @importFrom units set_units
#' @export 
set_units.magpie <- function (x, value, ...) {
  unit <- getMetadata(x,"unit")
  multiplier <- as.numeric(set_units(unit,value))
  getMetadata(x,"unit") <- value
  return(x*multiplier)
}