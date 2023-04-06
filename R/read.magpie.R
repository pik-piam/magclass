#' Read MAgPIE-object from file
#'
#' Reads a MAgPIE-file and converts it to a 3D array of the structure
#' (cells,years,datacolumn)
#'
#' This function reads from 13 different MAgPIE file_types. "rds" is
#' a R-default format for storing R objects."cs2" or "cs2b" is the new standard
#' format for cellular data with or without
#' header and the first columns (year,regiospatial) or only (regiospatial),
#' "csv" is the standard format for regional data with or without header
#' and the first columns (year,region,cellnumber) or only (region,cellnumber).
#' "cs3" is a format similar to csv and cs2, but with the difference that it supports
#' multidimensional data in a format which can be read by GAMS, "put" is a
#' newly supported format which is mosty used for the REMIND-MAgPIE coupling.
#' This format is only partly supported at the moment.  "asc" is the AsciiGrid
#' format (for example used for Arc Gis data).  "nc" is the netCDF format (only
#' "nc" files written by write.magpie can be read).  All these variants are
#' read without further specification. "magpie" (.m) and "magpie zipped" (.mz)
#' are new formats developed to allow a less storage intensive management of
#' MAgPIE-data. The only difference between both formats is that .mz is gzipped
#' whereas .m is not compressed. So .mz needs less memory, whereas .m might
#' have a higher compatibility to other languages. \cr\cr Since library version
#' 1.4 read.magpie can also read regional or global MAgPIE csv-files.
#'
#' @param file_name file name including file ending (wildcards are supported).
#' Optionally also the full path can be specified here (instead of splitting it
#' to file_name and file_folder)
#' @param file_folder folder the file is located in (alternatively you can also
#' specify the full path in file_name - wildcards are supported)
#' @param file_type format the data is stored in. Currently 13 formats are
#' available: "rds" (recommended compressed format),
#' "cs2" & "cs2b" (cellular standard MAgPIE format), "csv" (regional standard
#' MAgPIE format), "cs3" (multidimensional format compatible to GAMS), "cs4"
#' (alternative multidimensional format compatible to GAMS, in contrast to cs3
#' it can also handle sparse data), "cs5" (more generalized version of cs4),
#' "csvr", "cs2r", "cs3r" and "cs4r" which are
#' the same formats as the previous mentioned ones with the only difference
#' that they have a REMIND compatible format, "m" (binary MAgPIE format
#' "magpie"), "mz" (compressed binary MAgPIE format "magpie zipped") "put"
#' (format used primarily for the REMIND-MAgPIE coupling) and "asc",
#' (ASCII-Grid format as used by ArcGis) . If file_type=NULL the file ending
#' of the file_name is used as format. If format is different to the formats
#' mentioned standard MAgPIE format is assumed.
#' @param as.array Should the input be transformed to an array? This can be
#' useful for regional or global inputs, but all advantages of the magpie-class
#' are lost.
#' @param comment.char character: a character vector of length one containing a
#' single character or an empty string. Use "" to turn off the interpretation
#' of comments altogether. If a comment is found it will be stored in
#' attr(,"comment"). In text files the comment has to be at the beginning of
#' the file in order to be recognized by read.magpie.
#' @param check.names logical. If TRUE then the names of the variables in the
#' data frame are checked to ensure that they are syntactically valid variable
#' names. Same functionality as in read.table.
#' @param ... additional arguments passed to specific read functions (e.g.
#' \code{varname} for specifying the variable to be read in from a multi-variable
#' NCDF file.)
#' @return \item{x}{MAgPIE-object}
#' @note
#'
#' The binary MAgPIE formats .m and .mz have the following content/structure
#' (you only have to care for that if you want to implement
#' read.magpie/write.magpie functions in other languages): \cr \cr
#' [ FileFormatVersion | Current file format version number (currently 6) | integer | 2 Byte ] \cr
#' [ ncharComment | Number of character bytes of the file comment | integer | 4 Byte ] \cr
#' [ nbyteMetadata | Number of bytes of the serialized metadata | integer | 4 Byte ] \cr
#' [ ncharSets | Number of characters bytes of all regionnames + 2 delimiter | integer | 2 Byte] \cr
#' [ nyears | Number of years | integer | 2 Byte ]\cr
#' [ yearList | All years of the dataset (0, if year is not present) | integer | 2*nyears Byte ] \cr
#' [ ncells | Number of cells | integer | 4 Byte ]\cr
#' [ nchar_cell | Number of characters bytes of all regionnames + (nreg-1) for delimiters | integer | 4 Byte ] \cr
#' [ cells | Cell names saved as cell1\\cell2 (\\n is the delimiter) | character | 1*nchar_cell Byte ] \cr
#' [ nelem | Total number of data elements | integer | 4 Byte ] \cr
#' [ ncharData | Number of char. bytes of all datanames + (ndata - 1) for delimiters | integer | 4 Byte ] \cr
#' [ datanames | Names saved in the format data1\\ndata2 (\\n as del.) | character | 1*ncharData Byte ] \cr
#' [ data | Data of the MAgPIE array in vectorized form | numeric | 4*nelem Byte ] \cr
#' [ comment | Comment with additional information about the data | character | 1*ncharComment Byte ] \cr
#' [ sets | Set names with \\n as delimiter | character | 1*ncharSets Byte] \cr
#' [ metadata | serialized metadata information | bytes | 1*nbyteMetadata Byte] \cr
#'
#' @author Jan Philipp Dietrich, Stephen Bi, Florian Humpenoeder
#' @seealso \code{"\linkS4class{magpie}"}, \code{\link{write.magpie}}
#' @importFrom methods is new
#' @importFrom utils read.csv capture.output toBibtex
#' @export
read.magpie <- function(file_name, file_folder = "", file_type = NULL, as.array = FALSE, # nolint
                        comment.char = "*", check.names = FALSE, ...) {                  # nolint

  .buildFileName <- function(fileName, fileFolder) {
    fileName <- paste0(fileFolder, fileName)
    fileNameOut <- Sys.glob(fileName)
    if (length(fileNameOut) > 1) {
      fileNameOut <- fileNameOut[1]
      warning("File name ", fileName, " is ambiguous, only first alternative is used!")
    } else if (length(fileNameOut) == 0) {
      stop("File ", fileName, " does not exist!")
    }
    return(fileNameOut)
  }
  fileName <- .buildFileName(file_name, file_folder)

  .getFileType <- function(fileType, fileName) {
    # if file-type is not mentioned file-ending is used as file-type
    fileType <- ifelse(is.null(fileType), tail(strsplit(fileName, "\\.")[[1]], 1), fileType)
    allowedTypes <- c("rds", "m", "mz", "csv", "cs2", "cs2b", "cs3", "cs4", "cs5", "csvr", "cs2r", "cs3r",
                      "cs4r", "put", "asc", "nc")
    if (!(fileType %in% allowedTypes)) stop("Unknown file type: ", fileType)
    return(fileType)
  }
  fileType <- .getFileType(file_type, fileName)

  if (fileType %in% c("m", "mz")) {
    readMagpie <- readMagpieMZ(fileName, compressed = (fileType == "mz"))
  } else if (fileType == "rds") {
    readMagpie <- readRDS(fileName)
    if (!is.magpie(readMagpie)) stop("File does not contain a magpie object!")
  } else if (fileType %in% c("cs3", "cs3r")) {
    x <- read.csv(fileName, comment.char = comment.char, check.names = check.names, stringsAsFactors = TRUE)
    datacols <- grep("^dummy\\.?[0-9]*$", colnames(x))
    xdimnames <- lapply(x[datacols], function(x) return(as.character(unique(x))))
    xdimnames[[length(xdimnames) + 1]] <- colnames(x)[-datacols]
    names(xdimnames) <- NULL
    tmparr <- array(NA, dim = sapply(xdimnames, length), dimnames = xdimnames) # nolint
    for (i in xdimnames[[length(xdimnames)]]) {
      j <- sapply(cbind(x[datacols], i), as.character) # nolint
      .duplicates_check(j)
      tmparr[j] <- x[, i]
    }
    readMagpie <- as.magpie(tmparr)
    if (length(grep("^[A-Z]+_[0-9]+$", getCells(readMagpie))) == ncells(readMagpie)) {
      getCells(readMagpie) <- sub("_", ".", getCells(readMagpie))
    }
    attr(readMagpie, "comment") <- .readComment(fileName, commentChar = comment.char)
  } else if (fileType %in% c("cs4", "cs4r")) {
    x <- read.csv(fileName, comment.char = comment.char, header = FALSE,
                  check.names = check.names, stringsAsFactors = TRUE)
    readMagpie <- as.magpie(x, tidy = TRUE)
    attr(readMagpie, "comment") <- .readComment(fileName, commentChar = comment.char)
  } else if (fileType == "cs5") {
    .metaExtract <- function(fileName, commentChar) {
      comment <- .readComment(fileName, commentChar = commentChar)
      m <- grep("^META ", comment)
      metadata <- comment[m]
      comment <- comment[-m]
      pattern <- "^META (.*?): (.*)$"
      mNames <- sub(pattern, "\\1", metadata)
      mData  <- strsplit(sub(pattern, "\\2", metadata), ", ")
      names(mData) <- mNames
      return(list(comment = comment, metadata = mData))
    }
    m <- .metaExtract(fileName, comment.char)
    x <- read.csv(fileName, comment.char = comment.char, header = FALSE,
                  check.names = check.names, stringsAsFactors = FALSE)
    colnames(x) <- m$metadata$names
    readMagpie <- as.magpie(x, tidy = TRUE,
                            spatial = grep(".spat", m$metadata$dimtype, fixed = TRUE),
                            temporal = grep(".temp", m$metadata$dimtype, fixed = TRUE),
                            data = grep(".data", m$metadata$dimtype, fixed = TRUE))
    attr(readMagpie, "comment") <- m$comment
  } else if (fileType %in% c("asc", "nc", "grd", "tif")) {
    if (!requireNamespace("raster", quietly = TRUE)) stop("The package \"raster\" is required!")
    if (fileType == "nc") {
      if (!requireNamespace("ncdf4", quietly = TRUE)) {
        stop("The package \"ncdf4\" is required!")
      }
      nc <- ncdf4::nc_open(fileName)
      var <- names(nc[["var"]])
      vdim <- vapply(nc[["var"]], function(x) return(x$ndims), integer(1))
      var <- var[vdim > 0]
      ncdf4::nc_close(nc)
      tmp <- list()
      for (v in var) {
        suppressSpecificWarnings({
          warning <- capture.output(tmp[[v]] <- raster::brick(fileName, varname = v, ...))
        }, "partial match of 'group' to 'groups'", fixed = TRUE)
        if (length(warning) > 0) {
          tmp[[v]] <- NULL
          next
        }
        name <- sub("^X([0-9]*)$", "y\\1", names(tmp[[v]]), perl = TRUE)
        if (length(name) == 1 && name == "layer") name <- "y0"
        names(tmp[[v]]) <- paste0(name, "..", v)
      }
      readMagpie <- as.magpie(raster::stack(tmp))
    } else {
      readMagpie <- as.magpie(raster::brick(fileName, ...))
    }
  } else {
    readMagpie <- readMagpieOther(fileName, fileType, comment.char = comment.char, check.names = check.names)
  }
  if (as.array) readMagpie <- as.array(readMagpie)[, , ]
  return(readMagpie)
}
