#' install_magpie_units (!experimental!)
#' 
#' This function is currently experimental and non-functional by default! To activate it,
#' set withMetadata(TRUE).
#' 
#' The purpose of this function is to define common units used in MAgPIE and REMIND data
#' for recognizability by the udunits2 and units packages which handle unit conversions 
#' and compatibility checks for magpie objects.
#' 
#' @param x Can be a character of length one, a magpie object, or NULL (default). If a 
#' character is given, it will be temporarily installed (for the current R session) to the 
#' units database if it isn't already. If a magpie object, then the same will be done for 
#' the metadata units field. If NULL, then a set of frequently used units will be installed
#' to the database.
#' 
#' @return the newly installed units object (if a character or magpie argument was given). 
#' If x is a magpie object, the units field will be converted to a units object. If x is 
#' NULL, no output is returned. Note that the udunits2 package does not accept units which 
#' start or end with a number. The current general work-around is to add a '_' before or 
#' after the unit as necessary. Some specific cases are handled differently, e.g. 'tCO2'
#' is always defined as 'tCO2eq'.
#' 
#' @author Stephen Bi
#' @seealso \code{\link{units.magpie}}, \code{\link[units]{install_symbolic_unit}}, 
#' \code{\link[units]{install_conversion_constant}}
#' @export
#' @importFrom units install_symbolic_unit install_conversion_constant as_units
#' 
install_magpie_units <- function(x=NULL) {
  if (!withMetadata()) return(x)
  
  is.installed <- function(y) {
    return(is(try(as_units(y),silent=TRUE),"units"))
  }
  input_unit <- function(a) {
    if (grepl("[[:digit:]]$",a)) {
      if (grepl("CO2",a)) {
        a <- gsub("2","2eq",a)
      }else if (grepl("USD",a) | grepl("$",a)) {
        a <- paste0("y",gsub("\\D","",a),"_USD")
      }else {
        a <- paste0(a,"_")
      }
    }
    if (grepl("^[[:digit:]]",a)) {
      if (grepl("USD",a) | grepl("$",a)) {
        a <- paste0("y",gsub("\\D","",a),"_USD")
      }else {
        a <- paste0("_",a)
      }
    }
    a <- gsub("_"," ",a)
    a <- gsub("[[:punct:]]","",a)
    a <- gsub(" ","_",a)
    if (!is.installed(a)) {
      install_symbolic_unit(a) #,dimensionless=FALSE)
    }
    return(as_units(a))
  }
  
  if (is.null(x)) {
    if (!(is.installed("tDM") & is.installed("tCO2eq") & is.installed("million_people"))) {
      install_symbolic_unit("tDM",dimensionless = FALSE)                #tonnes of dry matter
      install_symbolic_unit("people")                                   
      install_conversion_constant("people","million_people",1e6)        #population metric, million people
      install_conversion_constant("tC","tonne",1)                       #tonnes of carbon
      install_conversion_constant("tC","tCO2eq",3.67)                   #tonnes of CO2 equivalent
      install_symbolic_unit("USD")                                      #US Dollars
    }
  }else if (is.magpie(x)) {
    u <- units(x)
    if (is.null(u)) {
      getMetadata(x,"unit") <- as_units(1)
    }else if (is.character(u)) {
      getMetadata(x,"unit") <- input_unit(u)
      return(u)
    }else if (!is(u,"units")) {
      warning("Argument must be a magpie object, a character or NULL")
    }
  }else if (is.character(x)) {
    x <- input_unit(x)
    return(x)
  }else if (!is(x,"units")) {
    warning("Argument must be a magpie object, a character or NULL")
  }
}