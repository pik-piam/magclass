#' Read file in report format
#'
#' This function reads the content of a reporting file (a file in the model
#' intercomparison file format *.mif) into a list of MAgPIE objects or a single
#' MAgPIE object.
#'
#'
#' @param file file name the object should be read from.
#' @param as.list if TRUE a list is returned (default), if FALSE it is tried to
#' merge all information in one MAgPIE object (still under development and
#' works currently only if the entries for the different models and scenarios
#' have exactly the same regions and years).
#' @param showSeparatorWarning Boolean (default value TRUE) that decides whether the warning
#' about the replacement of dots in variable names is displayed (default value) or not.
#'
#' @details
#'
#' The \strong{Model Intercomparison File Format (MIF)} is the default file
#' format for data produced by Integrated Assessment Models. It is based on the
#' common format used for Model Intercomparison Projects such as EMF and SSP
#' with some slight changes/clarifications in its definition. For interactions
#' between models this format should be used. For everything else it is at least
#' recommended to use this format, too.
#'
#' Aim of this standardization is to achieve a more flexible and smooth
#' communication between models and to facilitate the creation of aggregated
#' outputs from integrated assessment scenario runs which then can easily be
#' uploaded to external databases such as the EMF or SSP database. By using this
#' standard most of the required decisions for a working input output interface
#' between models have already been specified which significantly reduces the
#' required work to get a new interaction running.
#'
#' \strong{Definition}
#'
#' The format is characterized by the following features:
#'
#' \itemize{
#'   \item The file ending is ".mif"
#'   \item The file is written in ASCII format
#'   \item Entries are separated with ";", every line ends with a ";"
#'   \item The file always contains a header
#'   \item The format of the header is: \code{Model;Scenario;Region;Variable;Unit;<ADDITIONAL_COLUMNS>;<YEARS>;}
#' }
#'
#' The first 5 entries always have to exist, <ADDITIONAL_COLUMNS> is additional
#' information which can be added optionally (e.g. "Description") and <YEARS>
#' are the years for which data is delivered. <YEARS> are always written as 4
#' digit numbers. In the (very unlikely) case that a year before 1000 is used
#' the number has to start with a 0, e.g. 0950. <ADDITIONAL_COLUMNS> can be
#' anything, there are no further rules at the moment what it can contain.
#' However, there are strict rules for naming these columns. Allowed are single
#' names starting with a capital letter without special characters in it except
#' "_" which is allowed. Examples: "Description" allowed, "More Description" not
#' allowed, "More_Description" allowed, "123Description" not allowed,
#' "Description123" allowed. Scripts using this format must be able to ignore
#' additional columns. For years there are no specific limitations/requirements
#' which years should be reported. Scripts dealing with this data must be able
#' to work with different temporal resolutions. For variables basically
#' everything can be reported here. Missing values have to be marked with "N/A".
#'
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{write.report}}
#' @examples
#' \dontrun{
#' read.report("report.csv")
#' }
#'
#' @export read.report
#' @importFrom utils read.table
#'
read.report <- function(file, as.list = TRUE, showSeparatorWarning = TRUE) { # nolint

  .trim <- function(a) return(gsub("(^ +)|( +$)", "", as.character(a)))

  .returnMagpie <- function(tmp, scenario, model) {

    # replace weird degree symbol in tables
    tmp$Unit <- enc2utf8(tmp$Unit) # nolint
    tmp$Unit <- sub(pattern = "\U{00B0}C", replacement = "K", x = tmp$Unit, useBytes = TRUE) # nolint
    regions <- unique(as.character(tmp$Region))
    names(regions) <- regions
    years <- sub("X", "y", grep("^X[0-9]{4}$", dimnames(tmp)[[2]], value = TRUE))
    names <- unique(paste(tmp$Variable, "#SPLITHERE# (", tmp$Unit, ")", sep = "")) # nolint
    names(names) <- sub("#SPLITHERE#", "", names)
    names <- sub("#SPLITHERE#", "", names)
    # delete dots if they are aparently not used as dimension separator
    ndots <- nchar(gsub("[^\\.]*", "", names))
    if (any(ndots != ndots[1])) {
      names <- gsub("\\.", "p", names)
      if (showSeparatorWarning) {
        warning("Replaced all \".\" with \"p\" to prevent misinterpretation as dim separator")
      }
    }
    # replace weird ° in tables after sub function evaluation
    names <- enc2utf8(names)
    names <- sub(pattern = "\U{00B0}C", replacement = "K", x = names, useBytes = TRUE)
    names(names) <- enc2utf8(names(names))
    names(names) <- sub(pattern = "\U{00B0}C", replacement = "K", x = names(names), useBytes = TRUE)
    mag <- new.magpie(sub("ZZZZZZGLO", "GLO", (sort(sub("GLO", "ZZZZZZGLO", regions)))), years, names)
    yearelems <- grep("^X[0-9]{4}$", dimnames(tmp)[[2]])
    regions[order(sub("GLO", "ZZZZZZGLO", regions))] <- dimnames(mag)[[1]]
    mag <- as.array(mag)
    coord <- cbind(regions[tmp$Region], rep(years, each = dim(tmp)[1]),
      names[paste(tmp$Variable, " (", tmp$Unit, ")", sep = "")])
    if (dim(coord)[1] > length(mag)) {
      duplicates <- duplicated(as.data.table(coord))
      warning("Duplicate entries found for model \"", model, "\" and scenario \"", scenario,
        "\" and only the last entry found in the data will be used (duplicate entries: ",
        paste(apply(rbind(NULL, unique(coord[duplicates, c(1, 3)])), 1, paste, collapse = "|"),
          collapse = ", "), ")!")
    }
    # suppress warnings from potential NA conversions
    mag[coord] <- suppressWarnings(as.numeric(as.vector(as.matrix(tmp[, yearelems]))))
    names(dimnames(mag)) <- c("region", "year", "variable")
    mag <- as.magpie(mag, spatial = 1, temporal = 2)
    return(mag)
  }

  .readmif <- function(file) {
    defaultHeader <- c("Model", "Scenario", "Region", "Variable", "Unit", "X2005",
      "X2010", "X2020", "X2030", "X2040", "X2050", "X2060", "X2070",
      "X2080", "X2090", "X2100")
    # determine seperator
    s <- read.table(file, sep = ";", header = FALSE, nrows = 1, stringsAsFactors = FALSE)
    if (all(names(s) == "V1")) sep <- "," else sep <- ";"
    # recognize header
    s <- read.table(file, sep = sep, header = FALSE, nrows = 1, stringsAsFactors = FALSE)
    header <- (.trim(s[, 1]) == "Model" | .trim(s[, 1]) == "MODEL")
    # read in raw data
    raw <- read.table(file, sep = sep, header = header, stringsAsFactors = FALSE, na.strings = "N/A")
    uglyFormat <-  all(is.na(raw[, dim(raw)[2]]))
    if (uglyFormat) raw <- raw[, -dim(raw)[2]]

    # rename from uppercase to lowercase
    if (header & .trim(s[, 1]) == "MODEL") {
      names(raw)[1:5] <- defaultHeader[1:5]
    }

    if (!header) {
      if (dim(raw)[2] == length(defaultHeader)) {
        warning("Header is missing. Years are being guessed based on structure!")
        dimnames(raw)[[2]] <- defaultHeader
      }
      else stop("Cannot read report. No header given and report has not the standard size!")
    }

    output <- list()
    raw$Scenario <- .trim(raw$Scenario) # nolint
    raw$Model    <- .trim(raw$Model)    # nolint
    raw$Region   <- .trim(raw$Region)   # nolint
    raw$Unit     <- .trim(raw$Unit)     # nolint
    raw$Variable <- .trim(raw$Variable) # nolint

    raw$Model[is.na(raw$Model)] <- "NA"
    raw$Scenario[is.na(raw$Scenario)] <- "NA"

    raw$Region <- sub("R5\\.2", "", raw$Region)        # nolint
    raw$Region <- sub("World|glob", "GLO", raw$Region) # nolint
    models <- unique(raw$Model)
    scenarios <- unique(raw$Scenario)
    for (scenario in scenarios) {
      output[[scenario]] <- list()
      for (model in models) {
        if (nrow(raw[raw$Model == model & raw$Scenario == scenario, ]) > 0) {
          output[[scenario]][[model]] <- .returnMagpie(raw[raw$Model == model & raw$Scenario == scenario, ],
                                                       scenario, model)
          if (!as.list) {
            getNames(output[[scenario]][[model]]) <- paste(scenario, model, getNames(output[[scenario]][[model]]),
                                                           sep = ".")
          }
        }
      }
    }
    return(output)
  }

  # expand wildcards
  fileNameUnexpanded <- file
  file <- Sys.glob(file)
  if (length(file) > 1) {
    output <- NULL
    for (f in file) {
      output <- c(output, .readmif(f))
    }
  } else if (length(file) == 0) {
    stop("File ", fileNameUnexpanded, " could not be found!")
  } else {
    output <- .readmif(file)
  }

  if (!as.list) {
    regions <- Reduce(union, lapply(unlist(output, recursive = FALSE), function(x) {
      getItems(x, dim = 1.1)
    })) # make sure that magpie objects to be merged share the same regions

    .tmpFunc <- function(x) {
      data <- new.magpie(regions, getYears(x), getNames(x), fill = NA)
      data[getItems(x, dim = 1.1), getYears(x), getNames(x)] <- x[getItems(x, dim = 1.1), getYears(x), getNames(x)]
      return(data)
    }

    output <- mbind(lapply(unlist(output, recursive = FALSE), .tmpFunc))
    names(dimnames(output))[3] <- "scenario.model.variable"
  }
  return(output)
}
