#' install_magpie_units (!experimental!)
#' 
#' This function is currently experimental and non-functional by default! To activate it,
#' set withMetadata(TRUE).
#' 
#' Please install the development version of the R-units package. The devtools or remotes 
#' package is a prerequisite for this - e.g. remotes::install_github("r-quantities/units")
#' 
#' The purpose of this function is to define common units used in MAgPIE and REMIND data
#' for parseability by the udunits2 and units packages which handle unit conversions 
#' and compatibility checks.
#' 
#' @param x Can be a character of length one, a magpie object, or NULL (default). If a 
#' character is given, it will be temporarily installed (for the current R session) to the 
#' units database if it isn't already. If a magpie object, then the same will be done for 
#' the metadata units field. If NULL, then a set of frequently used units will be installed
#' to the database (also temporary).
#' 
#' @return If x is a character, the newly installed units object. 
#' If x is a magpie object, a magpie object with an updated units metadata field. 
#' If x is NULL, no output is returned. 
#' Note that the udunits2 package does not accept units which start or end with a number. 
#' The current general work-around is to add a '_' before or after the unit as necessary. 
#' Some specific cases are handled differently, e.g. 'USD_2003' becomes 'y2003_USD'.
#' 
#' @author Stephen Bi
#' @seealso \code{\link{units.magpie}}, \code{\link[units]{install_symbolic_unit}}, 
#' \code{\link[units]{install_conversion_constant}}
#' @export
#' @importFrom units install_symbolic_unit install_conversion_constant as_units mixed_units
#' 
install_magpie_units <- function(x=NULL) {
  if (!withMetadata()) return(x)
  
  is.installed <- function(y) {
    return(suppressWarnings(is(try(as_units(y),silent=TRUE),"units")))
  }
  
  unit_syntax <- function(z) {
      z <- gsub(" ","_",z)
      if (grepl("mio_",z,ignore.case=TRUE)) {
        z <- gsub("mio_","million_",z,ignore.case=TRUE)
      }
      prefix <- unlist(strsplit(z,"_"))[1]
      prefix <- paste0(prefix,"_")
      if (grepl("million_",prefix,ignore.case=TRUE) | grepl("billion_",prefix,ignore.case=TRUE) |
          grepl("trillion_",prefix,ignore.case=TRUE) | grepl("quadrillion_",prefix,ignore.case=TRUE) |
          grepl("thousand_",prefix,ignore.case=TRUE) | grepl("hundred_",prefix,ignore.case=TRUE)) {
        z <- gsub(prefix,"",z)
      }else {
        prefix <- ""
      }
      suffix <- ""
      if (grepl("US$",z,fixed=TRUE) | grepl("$",z,fixed=TRUE)) {
        z <- gsub("US","",z,fixed=TRUE)
        if (grepl("\\d",z)) {
          z <- gsub("$","",z,fixed=TRUE)
          z <- paste0(gsub("\\d","",z),"y",gsub("\\D","",z),"_USD")
        }else {
          z <- gsub("$","USD",z)
        }
      }
      if(grepl("[[:digit:]]$",z)) {
        if (is.installed(gsub("[[:digit:]]$","",z))) {
          suffix <- substr(z,nchar(z),nchar(z))
          z <- gsub("[[:digit:]]$","",z)
        }else {
          z <- paste0(z,"_")
        }
      }else if (grepl("^[[:digit:]]",z)) {
        z <- paste0("_",z)
      }
      if (grepl("_per_",z,ignore.case=TRUE)) {
        z <- gsub("_per_","/",z,ignore.case=TRUE)
        return(unit_syntax(z))
      }
      if (grepl("dry_matter",z,ignore.case=TRUE)) {
        z <- gsub("dry_matter","DM",z,ignore.case=TRUE)
      }
      if (z=="-" | z=="ratio" | z=="" | z==" ") {
        z <- 1
      }
      z <- gsub("([.|()\\{}+$?]|\\[|\\])","",z)

      if (prefix!="") {
        if (!is.installed(z)) {
          install_symbolic_unit(z,dimensionless=TRUE)
        }
        if (!is.installed(paste0(prefix,z))) {
          install_conversion_constant(paste0("million_",z),z,1e6)
          install_conversion_constant(paste0("billion_",z),z,1e9)
          install_conversion_constant(paste0("trillion_",z),z,1e12)
          install_conversion_constant(paste0("quadrillion_",z),z,1e15)
          install_conversion_constant(paste0("thousand_",z),z,1e3)
          install_conversion_constant(paste0("hundred_",z),z,1e2)
        }
      }
      z <- paste0(prefix,z,suffix)
    return(z)
  }
  
  input_unit <- function(a) {
    if (!is.installed(a)) {
      if (grepl(" or ",a,fixed=TRUE)) {
        a <- gsub(" or ",",",a,fixed=TRUE)
      }
      if (grepl(",",a,fixed=TRUE)) {
        a <- unlist(strsplit(a,","))
        for (i in 1:length(a)) {
          a[i] <- unit_syntax(trimws(a[i]))
          if (!is.installed(a[i])) {
            install_symbolic_unit(a[i])
          }
        }
        return(mixed_units(1,a))
      }else if (grepl("/",a,fixed=TRUE)) {
        a <- unlist(strsplit(a,"/",fixed=TRUE))
        for (i in 1:length(a)) {
          if (!is.installed(a[i])) {
            a[i] <- unit_syntax(a[i])
            if (!is.installed(a[i])) {
              install_symbolic_unit(a[i],dimensionless=FALSE)
            }
          }
        }
        a <- paste0(a,collapse="/")
      }else {
        a <- unit_syntax(a)
        if (!is.installed(a)) {
          install_symbolic_unit(a,dimensionless=FALSE)
        }
      }
    }
    return(as_units(a))
  }

  if (!is.installed("tDM") & !is.installed("tCO2eq") & !is.installed("people")) {
    install_symbolic_unit("tDM",dimensionless = FALSE)                #tonnes of dry matter
    install_symbolic_unit("people",dimensionless=FALSE)               
    install_conversion_constant("people","person",1)
    install_conversion_constant("people","capita",1)
    install_conversion_constant("tCO2eq","tonne",1)                   #tonnes of CO2 equivalent
    install_conversion_constant("tCO2eq","tCO2_",1)                   #tonnes of CO2
    install_conversion_constant("tC","tCO2_",44/12)                   #tonnes of carbon
    install_conversion_constant("tN2O","tCO2eq",298)                  #tonnes of N2O
    install_conversion_constant("tN","tN2O",44/28)                    #tonnes of nitrogen
    install_conversion_constant("tN","tNO2_",46/14)                   #tonnes of NO2
    install_conversion_constant("tN","tNO3_",62/14)                   #tonnes of NO3
    install_conversion_constant("tN","tNH3_",17/14)                   #tonnes of NH3
    install_conversion_constant("tCH4_","tCO2eq",36)                  #tonnes of methane
    install_symbolic_unit("USD",dimensionless=FALSE)                  #US Dollars
    install_symbolic_unit("share",dimensionless=FALSE)                #Share
    
  }
  if (is.magpie(x)) {
    u <- units(x)
    if (is.null(u)) {
      getMetadata(x,"unit") <- as_units(1)
    }else if (is.character(u)) {
      getMetadata(x,"unit") <- input_unit(u)
    }else if (!is(u,"units") & !is(u,"mixed_units")) {
      warning("Argument has invalid unit field of class",class(u))
    }
  }else if (is.character(x)) {
    x <- input_unit(x)
  }else if (!is(x,"units") & !is(u,"mixed_units")) {
    warning("Invalid argument of class",class(x))
  }
  return(x)
}