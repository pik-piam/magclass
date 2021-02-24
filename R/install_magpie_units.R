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
#' 
install_magpie_units <- function(x=NULL) {
  if (!withMetadata()) return(x)
  if (!requireNamespace("units", versionCheck = list(name = "units", op = ">=", version = "0.7.0"), quietly = TRUE)) {
    stop("units package in version >= 0.7 required")
  }
  units::units_options(auto_convert_names_to_symbols=FALSE, allow_mixed=FALSE, negative_power=TRUE, set_units_mode="standard")
  
  unit_syntax <- function(z) {
    if (grepl("mio",z,ignore.case=TRUE)) {
      z <- gsub("mio._","million_",z,ignore.case=TRUE)
      z <- gsub("mio_","million_",z,ignore.case=TRUE)
    }
    if (grepl("bn",z,ignore.case=TRUE)) {
      z <- gsub("bn._","billion_",z,ignore.case=TRUE)
      z <- gsub("bn_","billion_",z,ignore.case=TRUE)
    }
    if (grepl("bio.",z,ignore.case=TRUE)) {
      z <- gsub("bio.","billion",z,ignore.case=TRUE)
    }
    if (grepl("1e",substr(z,1,2),ignore.case=TRUE)) {
      if (grepl("+",z,fixed=TRUE)) {
        z <- gsub("+","",z,fixed=TRUE)
      }
      if (grepl("*",z,fixed=TRUE)) {
        z <- unlist(strsplit(z,"*",fixed=TRUE))
        prefix <- z[1]
        z <- z[2]
      }else {
        z <- gsub("1e","",z,ignore.case=TRUE)
        if (grepl("^-",z)) {
          z <- gsub("^-","",z)
          prefix <- paste0("1e-",unlist(strsplit(z,"\\D"))[1])
        }else {
          prefix <- paste0("1e",unlist(strsplit(z,"\\D"))[1])
        }
        z <- gsub("^\\d*","",z)
      }
    }else {
      prefix <- ""
    }
    z <- unlist(strsplit(z,"_"))
    for (k in 1:length(z)) {
      if (grepl("illion",z[k],ignore.case=TRUE) | grepl("thousand",z[k],ignore.case=TRUE) | grepl("hundred",z[k],ignore.case=TRUE)) {
        if (prefix=="") {
          prefix <- z[k] 
        }else {
          prefix[length(prefix)+1] <- z[k]
        }
        z <- z[-k]
      }
    }
    z <- paste0(z,collapse="_")
    suffix <- ""
    if (grepl("US$",z,fixed=TRUE) | grepl("$",z,fixed=TRUE) | grepl("dollar",z,ignore.case=TRUE) | grepl("USD",z,ignore.case=TRUE)) {
      if (!grepl("USD",z,fixed=TRUE)) {
        z <- gsub("US_","",z,fixed=TRUE)
        z <- gsub("U.S._","",z,fixed=TRUE)
        z <- gsub("U.S.","",z,fixed=TRUE)
        z <- gsub("US","",z,fixed=TRUE)
        z <- gsub("usd","USD",z,fixed=TRUE)
        z <- gsub("$","USD",z,fixed=TRUE)
        z <- gsub("dollars","USD",z,ignore.case=TRUE)
        z <- gsub("dollar","USD",z,ignore.case=TRUE)
      }
      if (grepl("\\d",z)) {
        tmp <- z
        if (length(grep("\\d",unlist(strsplit(tmp,""))))==4) {
          tmp <- paste0(gsub("\\d","",z),substr(gsub("\\D","",z),3,4),"_")
        }else if (length(grep("\\d",unlist(strsplit(tmp,""))))==2) {
          tmp <- paste0(gsub("\\d","",z),gsub("\\D","",tmp),"_")
        }
      }
      if (grepl("[[:alpha:]]",gsub("y","",gsub("USD","",z)))) {
        warning(z," is an invalid unit entry! USD units can only include the currency year!")
      }else if (tmp!="")  z <- remove_spaces(tmp)
    }
    if (grepl("\u20AC",z) | grepl("Euro",z,ignore.case=TRUE) | grepl("EUR",z,ignore.case=TRUE)) {
      if (!grepl("EUR",z,fixed=TRUE)) {
        z <- gsub("Euros","EUR",z,ignore.case=TRUE)
        z <- gsub("Euro","EUR",z,ignore.case=TRUE)
        z <- gsub("eur","EUR",z)
        if (grepl("EUR",z)) {
          z <- gsub("\u20AC","",z)
        }else {
          z <- gsub("\u20AC","EUR",z)
        }
      }
      if (grepl("\\d",z)) {
        tmp <- z
        if (length(grep("\\d",unlist(strsplit(tmp,""))))==4) {
          tmp <- paste0(gsub("\\d","",z),substr(gsub("\\D","",z),3,4),"_")
        }else if (length(grep("\\d",unlist(strsplit(tmp,""))))==2) {
          tmp <- paste0(gsub("\\d","",z),gsub("\\D","",tmp),"_")
        }
      }
      if (grepl("[[:alpha:]]",gsub("y","",gsub("EUR","",z)))) {
        warning(z," is an invalid unit entry! EUR units can only include the currency year!")
      }else if (tmp!="")  z <- remove_spaces(tmp)
    }
    if (grepl("^\\d",z)) {
      if (is_unit_installed(gsub("^\\d*","",remove_spaces(z)))) {
        prefix[length(prefix)+1] <- unlist(regmatches(z,gregexpr("^\\d*",z)))
        z <- gsub("^\\d*","",z)
      }else {
        z <- paste0("_",z)
      }
    }
    if (grepl("\\d$",z)) {
      if (grepl("^",z,fixed=TRUE)) {
        z <- unlist(strsplit(z,"^",fixed=TRUE))
        suffix <- paste0("^",z[2])
        z <- z[1]
      }else if (is_unit_installed(gsub("\\d*$","",z))) {
        suffix <- unlist(regmatches(z,gregexpr("\\d*$",z)))
        z <- gsub("\\d*$","",z)
      }else {
        z <- paste0(z,"_")
      }
    }
    if (any(substr(z,1,3)==c("kWh","MWh","GWh","TWh","EWh","PWh","ZWh"))) {
      z <- substr(z,1,3)
    }else if (any(substr(z,1,2)==c("kW","MW","GW","TW","EW","PW","ZW"))) {
      z <- substr(z,1,2)
    }else if (any(substr(z,1,2)==c("kJ","MJ","GJ","TJ","PJ","EJ","ZJ"))) {
      z <- substr(z,1,2)
    }else if (any(substr(z,nchar(z)-2,nchar(z))==c("kWh","MWh","GWh","TWh","EWh","PWh","ZWh"))) {
      z <- substr(z,nchar(z)-2,nchar(z))
    }else if (any(substr(z,(nchar(z)-1),nchar(z))==c("kJ","MJ","GJ","TJ","PJ","EJ","ZJ","kW","MW","GW","TW","EW","PW","ZW"))) {
      z <- substr(z,(nchar(z)-1),nchar(z))
    }else if (any(substr(z,1,2)==c("kt","Mt","Gt","Tt","Pt","Zt"))) {
      prefix[length(prefix)+1] <- substr(z,1,1)
      z <- substr(z,2,nchar(z))
    }
    if (grepl("DM",z,fixed=TRUE) | grepl("dry_matter",z,ignore.case=TRUE)) {
      if (grepl("^t",z) | grepl("^_t",z)) {
        z <- "tDM"
      }
    }else if (grepl("Wet_matter",z,ignore.case=TRUE) | grepl("WM",z,fixed=TRUE) | grepl("Fresh_matter",z,ignore.case=TRUE)) {
      if (grepl("^t",z) | grepl("^_t",z)) {
        z <- "tWM"
      }
    }else if (grepl("oil_eq",z)) {
      if (grepl("^t",z) | grepl("^_t",z)) {
        z <- "toe"
      }else if (grepl("ton",z)) {
        prefix <- unlist(strsplit(z,"ton"))[1]
        z <- "toe"
      }
    }
    if (grepl("^ton",z,ignore.case=TRUE) | grepl("^t_",z) |
        grepl("^square_",z,ignore.case=TRUE) | grepl("^cubic_",z,ignore.case=TRUE)) {
      if (!is_unit_installed(z) & grepl("_",z,fixed=TRUE)) {
        if (prefix=="") {
          prefix <- unlist(strsplit(z,"_",fixed=TRUE))[1]
          z <- paste(unlist(strsplit(z,"_",fixed=TRUE))[-1],collapse="_")
        }else {
          prefix[length(prefix)+1] <- unlist(strsplit(z,"_",fixed=TRUE))[1]
          z <- paste(unlist(strsplit(z,"_",fixed=TRUE))[-1],collapse="_")
        }
      }
    }
    if (any(z==c("cap","Cap","CAP"))) {
      z <- "capita"
    }else if (any(z==c("ratio","factor","dimensionless","unitless"))) {
      z <- 1
    }
    if (grepl("%",z,fixed=TRUE) & nchar(z)>1) {
      z <- gsub("_%_","_percent_",z,fixed=TRUE)
      z <- gsub("%_","percent_",z,fixed=TRUE)
      z <- gsub("_%","_percent",z,fixed=TRUE)
      z <- gsub("%","percent_",z,fixed=TRUE)
    }
    if (grepl("([.|#!@&~()\\{}+$?:]|\\[|\\])",z)) {
      warning("Unit entry \"",z,"\" contained invalid special characters which have now been removed. Please revise.")
      z <- gsub("([.|#!@&~()\\{}+$?:]|\\[|\\])","",z)
    }
    if (any(paste0(prefix,z)==c("","_","__","-")))  z <- "unknown"
    z <- prefix_check(prefix,z,suffix)
    return(z)
  }
  
  prefix_check <- function(pre,base,suff) {
    SI_prefix <- ""
    multiplier <- 1
    for (jj in 1:length(pre)) {
      if (grepl("million",pre[jj],ignore.case=TRUE))  multiplier <- 1e6*multiplier
      else if (grepl("billion",pre[jj],ignore.case=TRUE))  multiplier <- 1e9*multiplier
      else if (grepl("trillion",pre[jj],ignore.case=TRUE))  multiplier <- 1e12*multiplier
      else if (grepl("quadrillion",pre[jj],ignore.case=TRUE))  multiplier <- 1e15*multiplier
      else if (grepl("quintillion",pre[jj],ignore.case=TRUE))  multiplier <- 1e18*multiplier
      else if (grepl("thousand",pre[jj],ignore.case=TRUE))  multiplier <- 1e3*multiplier
      else if (grepl("hundred",pre[jj],ignore.case=TRUE))  multiplier <- 1e2*multiplier
      else if (grepl("\\d",pre[jj])) {
        multiplier <- as.numeric(pre[jj])*multiplier
      }else if (grepl("ton",pre[jj],ignore.case=TRUE) | pre[jj]=="t") {
        if (base=="") {
          base <- "tonnes"
        }else {
          base <- paste0("t_",base)
          base <- gsub("of_","",base)
          units::install_unit(base,"tonne",1)
        }
      }else if (grepl("square",pre[jj],ignore.case=TRUE)) {
        if (suff=="") {
          suff <- "^2"
        }else {
          suff <- paste0("^",as.numeric(gsub("^","",suff))*2)
        }
      }else if (grepl("cubic",pre[jj],ignore.case=TRUE)) {
        if (suff=="") {
          suff <- "^3"
        }else {
          suff <- paste0("^",as.numeric(gsub("^","",suff))*3)
        }
      }else if (any(pre[jj]==c("k","M","G","T","P","E","Z"))) {
        SI_prefix <- pre[jj]
      }
    }
    if (base=="") {
      if (multiplier!=1) {
        base <- "1"
      }else {
        base <- "unknown"
      }
    }
    if (!is_unit_installed(base)) {
      if (is_unit_installed(remove_spaces(base))) {
        base <- remove_spaces(base)
      }else {
        warning(base," has been successfully installed but is not a recognized unit. Standardize if possible.")
        units::install_unit(base,"")
      }
    }
    return(units::as_units(multiplier,paste0(SI_prefix,base,suff)))
  }
  
  remove_spaces <- function(v) {
    v <- trimws(v)
    if (is_unit_installed(paste0(gsub("_","",v,fixed=TRUE),"_"))) {
      return(paste0(gsub("_","",v,fixed=TRUE),"_"))
    }else if (is_unit_installed(gsub("_","",v,fixed=TRUE))) {
      return(gsub("_","",v,fixed=TRUE))
    }
  }
  
  split_denominator <- function(b) {
    if (grepl("/",b,fixed=TRUE)) {
      b <- as.list(unlist(strsplit(b,"/",fixed=TRUE)))
      for (i in 1:length(b)) {
        b[[i]] <- gsub("^_*","",gsub("_*$","",b[[i]]))
        b[[i]] <- unit_syntax(b[[i]])
        if (!is_unit_installed(b[[i]])) {
          if (is_unit_installed(remove_spaces(b[[i]]))) {
            b[[i]] <- remove_spaces(b[[i]])
          }else {
            units::install_unit(b[[i]],"")
          }
        }
      }
      if (is(b[[1]],"units")) {
        if (is(b[[2]],"units")) {
          return(units::as_units(as.numeric(b[[1]]/as.numeric(b[[2]])),paste0(as.character(units(b[[1]])),"/",as.character(units(b[[2]])))))
        }else {
          return(units::as_units(as.numeric(b[[1]]),paste0(as.character(units(b[[1]])),"/",b[[2]])))
        }
      }else if (is(b[[2]],"units")) {
        return(units::as_units(as.numeric(b[[2]]),paste0(b[[1]],"/",as.character(units(b[[2]])))))
      }else {
        return(units::as_units(paste0(b,collapse="/")))
      }
    }else {
      return(b)
    }
  }
  
  split_multiples <- function(w) {
    if (grepl("*",w,fixed=TRUE)) {
      w <- as.list(unlist(strsplit(w,"*",fixed=TRUE)))
      for (ii in 1:length(w)) {
        w[[ii]] <- gsub("^_*","",gsub("_*$","",w[[ii]]))
        if (grepl("^1e",w[[ii]]) & grepl("*",w[[ii]],fixed=TRUE)) {
          w[[ii]] <- paste0(w[[ii]],w[[ii+1]])
          w <- w[[-(ii+1)]]
        }
        if (is.na(w[[ii]])) {
          w <- w[[-ii]]
        }else {
          w[[ii]] <- unit_syntax(w[[ii]])
          if (!is_unit_installed(w[[ii]])) {
            if (is_unit_installed(remove_spaces(w[[ii]]))) {
              w[[ii]] <- remove_spaces(w[[ii]])
            }else {
              units::install_unit(w[[ii]],"")
            }
          }
        }
      }
      if (is(w[[1]],"units")) {
        if (is(w[[2]],"units")) {
          return(units::as_units(as.numeric(w[[1]]*as.numeric(w[[2]])),paste0(as.character(units(w[[1]])),"*",as.character(units(w[[2]])))))
        }else {
          return(units::as_units(as.numeric(w[[1]]),paste0(as.character(units(w[[1]])),"*",w[[2]])))
        }
      }else if (is(w[[2]],"units")) {
        return(units::as_units(as.numeric(w[[2]]),paste0(w[[1]],"*",as.character(units(w[[2]])))))
      }else {
        return(units::as_units(paste0(w,collapse="*")))
      }
    }else {
      return(w)
    }
  }
  
  input_unit <- function(a) {
    a <- gsub(" ","_",a)
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
      return(units::as_units("unknown"))
    }
    if (grepl("/",a,fixed=TRUE) | grepl("*",a,fixed=TRUE)) {
      a <- split_denominator(a)
      a <- split_multiples(a)
    }else {
      a <- unit_syntax(a)
    }
    if (!is_unit_installed(a)) {
      if (!is_unit_installed(remove_spaces(a))) {
        units::install_unit(a,"")
      }else {
        a <- remove_spaces(a)
      }
    }
    return(units::as_units(a))
  }
  
  #Initialize some commonly used units in MAGPIE and REMIND
  if (!is_unit_installed("tDM") || !is_unit_installed("pkm") || !is_unit_installed("unknown")) {
    units <- readRDS(system.file("extdata/metadata/units.rds", package = "magclass"))
    for(i in 1:nrow(units)) units::install_unit(symbol = units$unit[i], def = units$def[i],  name= units$name[i])
  }
  
  if (is.null(x)) {
    return(NULL)
  }else if (is.magpie(x)) {
    u <- units(x)
    if (is.null(u) || is.na(u) || is.nan(u)) {
      getMetadata(x,"unit") <- units::as_units("unknown")
    }else if (is.character(u)) {
      if (length(u)>1) {
        if (length(unique(u))==1) {
          u <- input_unit(u[1])
          #Mixed units handling under development
        }else {
          u <- units::as_units("unknown")
        }
      }else if (length(u)==0) {
        u <- units::as_units("unknown")
      }else if (any(u==c("-",""," ","none","unit"))) {
        u <- units::as_units("unknown")
      }else {
        u <- input_unit(u)
      }
      getMetadata(x,"unit") <- u
    }else if (!is(u,"units") & !is(u,"mixed_units")) {
      warning("Argument has invalid unit field of class ",class(u))
    }
  }else if (is.character(x)) {
    if (length(x)>1) {
      if (length(unique(x))==1) {
        x <- input_unit(x[1])
      #Mixed units handling under development
      }else {
        x <- units::as_units("unknown")
      }
    }else if (length(x)==0) {
      x <- units::as_units("unknown")
    }else if (any(x==c("-",""," ","none","unit"))) {
      x <- units::as_units("unknown")
    }else {
      x <- input_unit(x)
    }
  }else if (is.nan(x) | is.na(x)) {
    x <- units::as_units("unknown")
  }else if (!is(x,"units") & !is(x,"mixed_units")) {
    warning("Invalid argument of class ",class(x))
  }
  return(x)
}