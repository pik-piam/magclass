#' are_units_convertible (!experimental!)
#' 
#' This function checks whether two units are inter-convertible. It extends ud.are.convertible 
#' from the udunits2 package to magpie objects and newly defined units.
#' 
#' @param u1,u2 Either argument can be a character of length one, a units object or a MAgPIE object.
#' @return Returns a boolean. TRUE if u1 can be converted to u2, FALSE otherwise. 
#' @author Stephen Bi
#' @seealso \code{\link[udunits2]{ud.are.convertible}}
#' @export
#' 
are_units_convertible <- function(u1, u2) {
  if (is.magpie(u1)) {
    u1 <- units(u1)
  }
  if (is.magpie(u2)) {
    u2 <- units(u2)
  }
  if (is.character(u1)) {
    u1 <- install_magpie_units(u1)
  }else if (!is(u1,"units")) {
    stop("u1 must be either a character or units object, or a MAgPIE object with units metadata!")
  }
  if (is.character(u2)) {
    u2 <- install_magpie_units(u2)
  }else if (!is(u2,"units")) {
    stop("u2 must be either a character or units object, or a MAgPIE object with units metadata!")
  }
  return(as.character(units(u1/u2)) == "1")
}