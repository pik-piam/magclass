#' Read MAgPIE-object from file
#'
#' Reads a MAgPIE-file and converts it to a 3D array of the structure
#' (cells,years,datacolumn)
#'
#' See \code{\link{write.magpie}} for a list of supported file formats.
#'
#' @param file_name file name including file ending (wildcards are supported).
#' Optionally also the full path can be specified here (instead of splitting it
#' to file_name and file_folder)
#' @param file_folder folder the file is located in (alternatively you can also
#' specify the full path in file_name - wildcards are supported)
#' @param file_type format the data is stored in. If file_type=NULL the file ending
#' of the file_name is used as format. If format is different to the formats
#' mentioned standard MAgPIE format is assumed. See \code{\link{write.magpie}}
#' for a list of supported file formats.
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
#' See \code{\link{write.magpie}} for the detailed structure of binary MAgPIE formats .m and .mz.
#'
#' @author Jan Philipp Dietrich, Stephen Bi, Florian Humpenoeder, Pascal Sauer
#' @seealso \code{"\linkS4class{magpie}"}, \code{\link{write.magpie}}
#' @export
read.magpie <- function(file_name, file_folder = "", file_type = NULL, # nolint: object_name_linter, cyclocomp_linter.
                        as.array = FALSE, comment.char = "*", check.names = FALSE, ...) { # nolint: object_name_linter.

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
    x <- utils::read.csv(fileName, comment.char = comment.char, check.names = check.names, stringsAsFactors = TRUE)
    datacols <- grep("^dummy\\.?[0-9]*$", colnames(x))
    xdimnames <- lapply(x[datacols], function(x) return(as.character(unique(x))))
    xdimnames[[length(xdimnames) + 1]] <- colnames(x)[-datacols]
    names(xdimnames) <- NULL
    tmparr <- array(NA, dim = sapply(xdimnames, length), dimnames = xdimnames) # nolint:undesirable_function_linter.
    for (i in xdimnames[[length(xdimnames)]]) {
      j <- sapply(cbind(x[datacols], i), as.character) # nolint:undesirable_function_linter.
      .duplicates_check(j)
      tmparr[j] <- x[, i]
    }
    readMagpie <- as.magpie(tmparr)
    if (length(grep("^[A-Z]+_[0-9]+$", getCells(readMagpie))) == ncells(readMagpie)) {
      getCells(readMagpie) <- sub("_", ".", getCells(readMagpie))
    }
    attr(readMagpie, "comment") <- .readComment(fileName, commentChar = comment.char)
  } else if (fileType %in% c("cs4", "cs4r")) {
    x <- utils::read.csv(fileName, comment.char = comment.char, header = FALSE,
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
    x <- utils::read.csv(fileName, comment.char = comment.char, header = FALSE,
                         check.names = check.names, stringsAsFactors = FALSE)
    colnames(x) <- m$metadata$names
    readMagpie <- as.magpie(x, tidy = TRUE,
                            spatial = grep(".spat", m$metadata$dimtype, fixed = TRUE),
                            temporal = grep(".temp", m$metadata$dimtype, fixed = TRUE),
                            data = grep(".data", m$metadata$dimtype, fixed = TRUE))
    attr(readMagpie, "comment") <- m$comment
  } else if (fileType %in% c("asc", "nc", "grd", "tif")) {
    if (fileType == "nc") {
      if (!requireNamespace("terra", quietly = TRUE)) {
        stop("The package \"terra\" is required!")
      }

      x <- terra::rast(fileName)
      if (all(grepl("Time=[0-9]+", names(x)))) {
        names(x) <- sub("(.+)_Time=([0-9]+)", "y\\2..\\1", names(x))
      } else {
        parts <- strsplit(names(x), "_")
        lastParts <- vapply(parts, function(p) p[length(p)], character(1))
        timeIndices <- suppressSpecificWarnings(as.numeric(lastParts), "NAs introduced by coercion")

        terraTime <- terra::time(x)

        if (any(is.na(terraTime)) && requireNamespace("ncdf4", quietly = TRUE)) {
          nc <- ncdf4::nc_open(fileName)
          withr::defer(ncdf4::nc_close(nc))
          if ("time" %in% names(nc$dim)) {
            terraTime <- rep_len(nc$dim$time$vals, terra::nlyr(x))
            if (terra::nlyr(x) %% nc$dim$time$len != 0) {
              warning("Found ", terra::nlyr(x), " layers, but ",
                      nc$dim$time$len, " time steps. Now using ",
                      terraTime)
            }
          }
        }

        if (all(timeIndices %in% seq_along(terraTime))) {
          names(x) <- paste0("y", terraTime[timeIndices], "..",
                             vapply(parts, function(p) paste0(p[-length(p)], collapse = "_"), character(1)))
        } else if (all(!is.na(terraTime))) {
          names(x) <- paste0("y", terraTime, "..", names(x))
        }
      }

      readMagpie <- clean_magpie(as.magpie(x))
      attr(readMagpie, "crs") <- NULL
    } else {
      if (!requireNamespace("raster", quietly = TRUE)) {
        stop("The package \"raster\" is required!")
      }
      readMagpie <- as.magpie(raster::brick(fileName, ...))
    }
  } else {
    readMagpie <- readMagpieOther(fileName, fileType, comment.char = comment.char, check.names = check.names)
  }
  if (as.array) readMagpie <- as.array(readMagpie)[, , ]
  return(readMagpie)
}
