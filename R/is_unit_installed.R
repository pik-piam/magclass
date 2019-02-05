#' is_unit_installed (!experimental!)
#'
#' This function quickly checks whether a character is already recognizable as a units object.
#' If FALSE, the unit can be installed via install_magpie_units.
#' 
#' @param char A character string to be checked for units compatibility
#' @return Returns a boolean. TRUE if char is recognized by the units package and FALSE otherwise.
#' If FALSE, char can be installed as a compatible unit via install_magpie_units.
#' @author Stephen Bi
#' 
is_unit_installed <- function(char) {
  return(suppressWarnings(is(try(units::as_units(char),silent=TRUE),"units")))
}