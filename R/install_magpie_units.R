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
#' @importFrom units install_symbolic_unit install_conversion_constant as_units mixed_units units_options
#' 
install_magpie_units <- function(x=NULL) {
  if (!withMetadata()) return(x)
  units_options(auto_convert_names_to_symbols=FALSE, allow_mixed=TRUE)
  
  is.installed <- function(y) {
    return(suppressWarnings(is(try(as_units(y),silent=TRUE),"units")))
  }
  
  unit_syntax <- function(z) {
    if (grepl("mio",z,ignore.case=TRUE)) {
      z <- gsub("mio._","million_",z,ignore.case=TRUE)
      z <- gsub("mio_","million_",z,ignore.case=TRUE)
    }
    if (grepl("bn",z,ignore.case=TRUE)) {
      z <- gsub("bn._","billion_",z,ignore.case=TRUE)
      z <- gsub("bn_","billion_",z,ignore.case=TRUE)
    }
    if (grepl("_",z,fixed=TRUE)) {
      prefix <- unlist(strsplit(z,"_"))[1]
      prefix <- paste0(prefix,"_")
    }else {
      prefix <- ""
    }
    if (grepl("million_",prefix,ignore.case=TRUE) | grepl("billion_",prefix,ignore.case=TRUE) |
        grepl("trillion_",prefix,ignore.case=TRUE) | grepl("quadrillion_",prefix,ignore.case=TRUE) |
        grepl("thousand_",prefix,ignore.case=TRUE) | grepl("hundred_",prefix,ignore.case=TRUE)) {
      z <- gsub(prefix,"",z)
    }else {
      prefix <- ""
    }
    suffix <- ""
    if (grepl("US$",z,fixed=TRUE) | grepl("$",z,fixed=TRUE) | grepl("dollar",z,ignore.case=TRUE)) {
      z <- gsub("US_","",z,fixed=TRUE)
      z <- gsub("U.S._","",z,fixed=TRUE)
      z <- gsub("U.S.","",z,fixed=TRUE)
      z <- gsub("US","",z,fixed=TRUE)
      if (grepl("\\d",z)) {
        z <- gsub("$","",z,fixed=TRUE)
        z <- gsub("dollars","",z,ignore.case=TRUE)
        z <- gsub("dollar","",z,ignore.case=TRUE)
        if (length(grep("\\d",unlist(strsplit(z,""))))==4) {
          z <- paste0("USD",substr(gsub("\\D","",z),3,4),"_")
        }else if (length(grep("\\d",unlist(strsplit(z,""))))==2) {
          z <- paste0("USD",gsub("\\D","",z),"_")
        }
      }else {
        z <- gsub("$","USD",z,fixed=TRUE)
        z <- gsub("dollars","USD",z,ignore.case=TRUE)
        z <- gsub("dollar","USD",z,ignore.case=TRUE)
      }
    }
    if (grepl("\\d$",z)) {
      if (grepl("^",z,fixed=TRUE)) {
        z <- unlist(strsplit(z,"^",fixed=TRUE))
        suffix <- paste0("^",z[2])
        z <- z[1]
      }else if (is.installed(gsub("\\d*$","",z))) {
        suffix <- unlist(regmatches(z,gregexpr("\\d*$",z)))
        z <- gsub("\\d*$","",z)
      }else {
        z <- paste0(z,"_")
      }
    }
    if (grepl("1e",substr(z,1,2),ignore.case=TRUE)) {
      if (grepl("*",z,fixed=TRUE)) {
        z <- unlist(strsplit(z,"*",fixed=TRUE))
        prefix <- z[1]
        z <- z[2]
      }else {
        z <- gsub("1e","",z,ignore.case=TRUE)
        prefix <- paste0("1e",unlist(strsplit(z,"\\D"))[1])
        z <- gsub("^\\d*","",z)
      }
    }else if (grepl("^\\d",z)) {
      if (is.installed(gsub("^\\d*","",z))) {
        prefix <- unlist(regmatches(z,gregexpr("^\\d*",z)))
        z <- gsub("^\\d*","",z)
      }else {
        z <- paste0("_",z)
      }
    }
    if (agrepl("ton_DM",z,ignore.case=TRUE) | agrepl("tons_of_dry_matter",z,max.distance=0.12,ignore.case=TRUE) | agrepl("tonne_DM",z,ignore.case=TRUE) 
        | agrepl("tons_of_DM",z,ignore.case=TRUE) | agrepl("tonnes_of_DM",z,ignore.case=TRUE)) {
      z <- "tDM"
    }
    if (agrepl("ton_WM",z,ignore.case=TRUE) | agrepl("tons_of_wet_matter",z,max.distance=0.12,ignore.case=TRUE) | agrepl("tonne_WM",z,ignore.case=TRUE) 
        | agrepl("tons_of_WM",z,ignore.case=TRUE) | agrepl("tonnes_of_WM",z,ignore.case=TRUE)) {
      z <- "tDM"
    }
    if (z=="-" | z=="ratio" | z=="" | z==" " | z=="factor" | z=="dimensionless" | z=="unitless" | z=="none" | z=="unit") {
      z <- 1
    }
    if (grepl("%",z,fixed=TRUE)) {
      z <- gsub("_%_","_percent_",z,fixed=TRUE)
      z <- gsub("%_","percent_",z,fixed=TRUE)
      z <- gsub("_%","_percent",z,fixed=TRUE)
      z <- gsub("%","percent_",z,fixed=TRUE)
    }
    if (grepl("([.|()\\{}+$?:]|\\[|\\])",z)) {
      z <- gsub("([.|()\\{}+$?:]|\\[|\\])","",z)
      warning(paste("unit entry",z,"contained invalid characters which have been removed"))
    }
    if (prefix!="") {
      if (!is.installed(z)) {
        if (!is.installed(remove_spaces(z))) {
          install_symbolic_unit(z,dimensionless=FALSE)
        }else {
          z <- remove_spaces(z)
        }
      }
      if (!is.installed(paste0(prefix,z))) {
        if (grepl("million_",prefix,ignore.case=TRUE))  prefix <- "1e6"
        if (grepl("billion_",prefix,ignore.case=TRUE))  prefix <- "1e9"
        if (grepl("trillion_",prefix,ignore.case=TRUE))  prefix <- "1e12"
        if (grepl("quadrillion_",prefix,ignore.case=TRUE))  prefix <- "1e15"
        if (grepl("quintillion_",prefix,ignore.case=TRUE))  prefix <- "1e18"
        if (grepl("thousand_",prefix,ignore.case=TRUE))  prefix <- "1e3"
        if (grepl("hundred_",prefix,ignore.case=TRUE))  prefix <- "1e2"
      }
    }
    z <- paste0(prefix,z,suffix)
    return(z)
  }
  
  remove_spaces <- function(v) {
    v <- trimws(v)
    if (is.installed(gsub("_","",v,fixed=TRUE))) {
      return(gsub("_","",v,fixed=TRUE))
    }else if (is.installed(paste0(gsub("_","",v,fixed=TRUE),"_"))) {
      return(paste0(gsub("_","",v,fixed=TRUE),"_"))
    }
  }
  
  split_denominator <- function(b) {
    if (grepl("/",b,fixed=TRUE)) {
      b <- unlist(strsplit(b,"/",fixed=TRUE))
      for (i in 1:length(b)) {
        b[i] <- gsub("^_*","",gsub("_*$","",b[i]))
        if (!is.installed(b[i])) {
          if (is.installed(remove_spaces(b[i]))) {
            b[i] <- remove_spaces(b[i])
          }else {
            b[i] <- unit_syntax(b[i])
            if (!is.installed(b[i])) {
              if (is.installed(remove_spaces(b[i]))) {
                b[i] <- remove_spaces(b[i])
              }else {
                install_symbolic_unit(b[i],dimensionless=FALSE)
              }
            }
          }
        }
      }
      return(paste0(b,collapse="/"))
    }else {
      return(b)
    }
  }
  
  split_multiples <- function(w) {
    if (grepl("*",w,fixed=TRUE)) {
      w <- unlist(strsplit(w,"*",fixed=TRUE))
      for (ii in 1:length(w)) {
        w[ii] <- gsub("^_*","",gsub("_*$","",w[ii]))
        if (grepl("^1e",w[ii],ignore.case=TRUE)) {
          w[ii] <- paste0(w[ii],w[ii+1])
          w <- w[-(ii+1)]
        }
        if (is.na(w[ii])) {
          w <- w[-(ii)]
        }else if (!is.installed(w[ii])) {
          if (is.installed(remove_spaces(w[ii]))) {
            w[ii] <- remove_spaces(w[ii])
          }else {
            w[ii] <- unit_syntax(w[ii])
            if (!is.installed(w[ii])) {
              if (is.installed(remove_spaces(w[ii]))) {
                w[ii] <- remove_spaces(w[ii])
              }else {
                install_symbolic_unit(w[ii],dimensionless=FALSE)
              }
            }
          }
        }
        
      }
      return(paste0(w,collapse="*"))
    }else {
      return(w)
    }
  }
  
  input_unit <- function(a) {
    a <- gsub(" ","_",a)
    a <- gsub("-","_",a)
    if (!is.installed(a)) {
      if (is.installed(remove_spaces(a))) {
        return(as_units(remove_spaces(a)))
      }
      if (grepl("_or_",a,fixed=TRUE)) {
        a <- gsub("_or_",",",a,fixed=TRUE)
      }
      if (grepl("_per_",a,ignore.case=TRUE)) {
        a <- gsub("_per_","/",a,ignore.case=TRUE)
      }
      if (grepl("_times_",a,ignore.case=TRUE)) {
        a <- gsub("_times_","*",a,ignore.case=TRUE)
      }
      if (grepl(",",a,fixed=TRUE) | grepl(";",a,fixed=TRUE)) {
        a <- unlist(strsplit(a,","))
        a <- unlist(strsplit(a,";"))
        for (i in 1:length(a)) {
          a[i] <- gsub("^_*","",gsub("_*$","",a[i]))
          if (is.installed(remove_spaces(a[i]))) {
            a[i] <- remove_spaces(a[i])
          }else {
            a[i] <- split_denominator(a[i])
            a[i] <- split_multiples(a[i])
            if (!is.installed(a[i])) {
              a[i] <- unit_syntax(a[i])
              if (!is.installed(a[i])) {
                if (!is.installed(remove_spaces(a[i]))) {
                  install_symbolic_unit(a[i],dimensionless=FALSE)
                }else {
                  a[i] <- remove_spaces(a[i])
                }
              }
            }
          }
        }
        return(mixed_units(1,a))
      }
      a <- split_denominator(a)
      a <- split_multiples(a)
      if (!is.installed(a)) {
        a <- unit_syntax(a)
        if (!is.installed(a)) {
          if (!is.installed(remove_spaces(a))) {
            install_symbolic_unit(a,dimensionless=FALSE)
          }else {
            a <- remove_spaces(a)
          }
        }
      }
    }
    return(as_units(a))
  }
  
  #Initialize some commonly used units in MAGPIE and REMIND
  if (!is.installed("tDM") & !is.installed("tCO2eq") & !is.installed("people")) {
    install_symbolic_unit("tDM",dimensionless = FALSE)                #tonnes of dry matter
    install_symbolic_unit("tWM",dimensionless = FALSE)                #tonnes of wet matter
    install_symbolic_unit("people",dimensionless=FALSE)               
    install_conversion_constant("people","person",1)
    install_conversion_constant("people","capita",1)
    install_symbolic_unit("tCO2eq",dimensionless=FALSE)               #tonnes of CO2 equivalent
    install_conversion_constant("tCO2eq","tCO2_",1)                   #tonnes of CO2
    install_conversion_constant("tC","tCO2_",44/12)                   #tonnes of carbon
    install_conversion_constant("tN2O","tCO2eq",298)                  #tonnes of N2O
    install_conversion_constant("tN","tN2O",44/28)                    #tonnes of nitrogen
    install_conversion_constant("tN","tNO2_",46/14)                   #tonnes of NO2
    install_conversion_constant("tN","tNO3_",62/14)                   #tonnes of NO3
    install_conversion_constant("tN","tNH3_",17/14)                   #tonnes of NH3
    install_conversion_constant("tCH4_","tCO2eq",36)                  #tonnes of methane
    install_symbolic_unit("USD",dimensionless=FALSE)                  #US Dollars in 2018
    install_conversion_constant("USD95_","USD",1.6564)
    install_conversion_constant("USD05_","USD",1.2926)
    install_symbolic_unit("share",dimensionless=TRUE)                 #Share
    install_symbolic_unit("passengers",dimensionless=FALSE)           #passengers
    install_conversion_constant("pkm","passengers*km",1)              #passenger-kilometer pkm
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