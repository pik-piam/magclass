#' Read MAgPIE-object from file
#'
#' Reads a MAgPIE-file and converts it to a 3D array of the structure
#' (cells,years,datacolumn)
#'
#' This function reads from 13 different MAgPIE file\_types. "rds" is
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
#' to file\_name and file\_folder)
#' @param file_folder folder the file is located in (alternatively you can also
#' specify the full path in file\_name - wildcards are supported)
#' @param file_type format the data is stored in. Currently 13 formats are
#' available: "rds" (recommended compressed format),
#' "cs2" & "cs2b" (cellular standard MAgPIE format), "csv" (regional standard
#' MAgPIE format), "cs3" (multidimensional format compatible to GAMS), "cs4"
#' (alternative multidimensional format compatible to GAMS, in contrast to cs3
#' it can also handle sparse data), "csvr", "cs2r", "cs3r" and "cs4r" which are
#' the same formats as the previous mentioned ones with the only difference
#' that they have a REMIND compatible format, "m" (binary MAgPIE format
#' "magpie"), "mz" (compressed binary MAgPIE format "magpie zipped") "put"
#' (format used primarily for the REMIND-MAgPIE coupling) and "asc",
#' (ASCII-Grid format as used by ArcGis) . If file\_type=NULL the file ending
#' of the file\_name is used as format. If format is different to the formats
#' mentioned standard MAgPIE format is assumed.
#' @param as.array Should the input be transformed to an array? This can be
#' useful for regional or global inputs, but all advantages of the magpie-class
#' are lost.
#' @param old_format used to read files in old MAgPIE-format (unused space was
#' not located at the beginning of the file), will be removed soon.
#' @param comment.char character: a character vector of length one containing a
#' single character or an empty string. Use "" to turn off the interpretation
#' of comments altogether. If a comment is found it will be stored in
#' attr(,"comment"). In text files the comment has to be at the beginning of
#' the file in order to be recognized by read.magpie.
#' @param check.names logical. If TRUE then the names of the variables in the
#' data frame are checked to ensure that they are syntactically valid variable
#' names. Same functionality as in read.table.
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
#' @examples
#' \dontrun{
#' a <- read.magpie("lpj_yield_ir.csv")
#' write.magpie(a, "lpj_yield_ir.mz")
#' }
#'
#' @export read.magpie
#' @importFrom methods is new
#' @importFrom utils read.csv
#' @importFrom utils toBibtex
#'
read.magpie <- function(file_name, file_folder = "", file_type = NULL, as.array = FALSE, old_format = FALSE, # nolint
                        comment.char = "*", check.names = FALSE) {                                           # nolint

  file_name <- paste(file_folder, file_name, sep = "") # nolint

  if (length(Sys.glob(file_name)) == 0) {
    stop(paste("file", file_name, "does not exist"))
  }

  # expand wildcards
  fileNameUnexpanded <- file_name
  file_name <- Sys.glob(file_name) # nolint
  if (length(file_name) > 1) {
    file_name <- file_name[1] # nolint
    warning(paste("file name", fileNameUnexpanded, "is ambiguous, only first alternative is used!"))
  } else if (length(file) == 0) {
    stop("File ", fileNameUnexpanded, " could not be found!")
  }

  # if file-type is not mentioned file-ending is used as file-type
  if (is.null(file_type)) {
    file_type <- tail(strsplit(file_name, "\\.")[[1]], 1) # nolint
  }
  allowedTypes <- c("rds", "m", "mz", "csv", "cs2", "cs2b", "cs3", "cs4", "csvr", "cs2r", "cs3r",
    "cs4r", "put", "asc", "nc", "nc2")
  if (!(file_type %in% allowedTypes)) stop(paste("Unkown file type:", file_type))

  .readComment <- function(file_name, comment.char = "*", metaChar = "#") { # nolint
    comment <- NULL
    if (!is.null(comment.char)) {
      if (comment.char != "") {
        zz <- file(file_name)
        open(zz)
        readRepeat <- TRUE
        while (readRepeat) {
          tmp <- readLines(zz, 1)
          if (length(grep(paste("^", escapeRegex(comment.char), sep = ""), tmp)) &
            !grepl(metaChar, substr(tmp, 3, 3), fixed = TRUE)) {
            comment <- c(comment, tmp)
          } else {
            readRepeat <- FALSE
          }
        }
        close(zz)
      }
    }
    return(substring(comment, 2))
  }

  if (file.exists(file_name)) {
    if (file_type == "m" | file_type == "mz") {

      if (file_type == "mz") {
        zz <- gzfile(file_name, "rb")
      } else {
        zz <- file(file_name, "rb")
      }

      if (!old_format) {
        newestFformatVersion <- 6
        fformatVersion <- readBin(zz, integer(), 1, size = 2)
        if (fformatVersion > newestFformatVersion) {
          stop("File format is newer (v", fformatVersion, ") than the formats supported (v0-v",
            newestFformatVersion, "). ", "\nPlease update your magclass package to the newest version!")
        }
        ncharComment <- readBin(zz, integer(), 1, size = 4)
        empty <- 94
        if (fformatVersion > 2) {
          nbyteMetadata <- readBin(zz, integer(), 1, size = 4)
        }
        if (fformatVersion > 1) {
          ncharSets <- readBin(zz, integer(), 1, size = 2)
          empty <- empty - 2
        }
        if (fformatVersion < 6) readBin(zz, integer(), empty, size = 1)
      } else {
        fformatVersion <- 0
      }
      nyears    <- readBin(zz, integer(), 1, size = 2)
      yearList <- readBin(zz, integer(), nyears, size = 2)
      useBytes  <- (fformatVersion > 4)
      if (fformatVersion > 5) {
        ncells      <- readBin(zz, integer(), 1, size = 4)
        ncharCells <- readBin(zz, integer(), 1, size = 4)
        cellnames   <- strsplit(readChar(zz, ncharCells, useBytes = TRUE), "\n", useBytes = TRUE)[[1]]
      } else {
        nregions      <- readBin(zz, integer(), 1, size = 2)
        ncharRegions <- readBin(zz, integer(), 1, size = ifelse(fformatVersion > 3, 4, 2))
        regions       <- strsplit(readChar(zz, ncharRegions, useBytes = useBytes), "\n", useBytes = useBytes)[[1]]
        cpr           <- readBin(zz, integer(), nregions, size = 4)
        ncells <- sum(cpr)
        if (any(cpr != 1)) {
          cellnames   <- paste(rep(regions, cpr), 1:ncells, sep = ".")
        } else {
          cellnames   <- regions
        }
      }
      nelem <- readBin(zz, integer(), 1, size = 4)
      ncharData <- readBin(zz, integer(), 1, size = 4)

      datanames <- strsplit(readChar(zz, ncharData, useBytes = useBytes), "\n", useBytes = useBytes)[[1]]

      if (old_format) readBin(zz, integer(), 100, size = 1) # 100 Byte reserved for later file format improvements

      output <- array(readBin(zz, numeric(), nelem, size = 4), c(ncells, nyears, nelem / ncells / nyears))
      output[is.nan(output)] <- NA
      if (length(cellnames) == 1) cellnames <- list(cellnames)
      dimnames(output)[[1]] <- cellnames
      if (yearList[1] > 0) dimnames(output)[[2]] <- paste("y", yearList, sep = "")
      if (length(datanames) > 0) dimnames(output)[[3]] <- datanames

      if (fformatVersion > 0 && ncharComment > 0) {
        attr(output, "comment") <- strsplit(readChar(zz, ncharComment, useBytes = useBytes), "\n",
          useBytes = useBytes)[[1]]
      }
      if (fformatVersion > 1 && ncharSets > 0) {
        names(dimnames(output)) <- strsplit(readChar(zz, ncharSets, useBytes = useBytes), "\n",
          useBytes = useBytes)[[1]]
      }
      if (fformatVersion > 2) {
       readBin(zz, raw(), nbyteMetadata)
      }
      close(zz)
      attr(output, "FileFormatVersion") <- fformatVersion
      readMagpie <- new("magpie", output)
    } else if (file_type == "rds") {
      readMagpie <- readRDS(file_name)
      if (!is.magpie(readMagpie)) stop("File does not contain a magpie object!")
    } else if (file_type == "cs3" | file_type == "cs3r") {
      x <- read.csv(file_name, comment.char = comment.char, check.names = check.names, stringsAsFactors = TRUE)
      datacols <- grep("^dummy\\.?[0-9]*$", colnames(x))
      xdimnames <- lapply(lapply(x[datacols], unique), as.character)
      if (!is.list(xdimnames)) xdimnames <- list(xdimnames)
      xdimnames[[length(xdimnames) + 1]] <- colnames(x)[-datacols]
      names(xdimnames) <- NULL
      tmparr <- array(NA, dim = sapply(xdimnames, length), dimnames = xdimnames)
      for (i in xdimnames[[length(xdimnames)]]) {
        j <- sapply(cbind(x[datacols], i), as.character)
        .duplicates_check(j)
        tmparr[j] <- x[, i]
      }
      readMagpie <- as.magpie(tmparr)
      if (length(grep("^[A-Z]+_[0-9]+$", getCells(readMagpie))) == ncells(readMagpie)) {
        getCells(readMagpie) <- sub("_", ".", getCells(readMagpie))
      }
      attr(readMagpie, "comment") <- .readComment(file_name, comment.char = comment.char)
    } else if (file_type == "cs4" | file_type == "cs4r") {
      x <- read.csv(file_name, comment.char = comment.char, header = FALSE,
        check.names = check.names, stringsAsFactors = TRUE)
      readMagpie <- as.magpie(x, tidy = TRUE)
      attr(readMagpie, "comment") <- .readComment(file_name, comment.char = comment.char)
    } else if (file_type == "asc") {
      grid <- suppressWarnings(try(maptools::readAsciiGrid(file_name, dec = "."), silent = T))
      if (is(grid, "try-error")) {
        grid <- try(maptools::readAsciiGrid(file_name, dec = ","))
        if (is(grid, "try-error")) stop("File cannot be read. Make sure the file is in AsciiGrid format with ",
          "either '.' or ',' as decimal point character.")
      }
      if (!all(grid@grid@cellsize == 0.5)) stop("Only 0.5 degree data supported. Input data is in (",
        paste(grid@grid@cellsize, collapse = ","), ") degree (x,y).")
      # Convert to SpatialPixelsDataFrame
      sp::fullgrid(grid) <- FALSE
      magpieCoords <- as.matrix(magclassdata$half_deg[, c("lon", "lat")])
      rowmatch <- function(a, b) {
        # Rows in a that match the rows in b
        f <- function(...) paste(..., sep = ":")
        if (!is.matrix(b)) b <- matrix(b, 1, length(b))
        a <- do.call(f, as.data.frame(a))
        b <- do.call(f, as.data.frame(b))
        match(b, a)
      }
      mpRows <- rowmatch(grid@coords, magpieCoords)
      names(mpRows) <- 1:59199
      if (any(is.na(mpRows))) warning(sum(is.na(mpRows)), " magpie cells are missing in the grid file.",
        " They will be set to NA.")
      omittedCells <- which(!(seq_along(grid@data[[1]])) %in% mpRows)
      if (length(omittedCells) > 0) {
        omittedFraction <- sum(grid@data[[1]][omittedCells] / sum(grid@data[[1]]))
        warning(length(omittedCells), " of ", length(grid@data[[1]]), " cells in the file that contain data",
          " are discarded because they do not correspond to magpie cells.\n  Those cells contain ",
          omittedFraction * 100, " percent of the global sum of the input file.")
      }
      readMagpie <- rep(-1001, 59199)
      names(readMagpie) <- 1:59199
      goodcells <- names(mpRows)[which(!is.na(mpRows))]
      readMagpie[goodcells] <- grid@data[[1]][mpRows[goodcells]]
      readMagpie[is.na(mpRows)] <- NA
      names(readMagpie) <- paste(magclassdata$half_deg$region, 1:59199, sep = ".")
      readMagpie <- as.magpie(readMagpie)

    } else if (file_type == "nc") { # netcdf
      if (!requireNamespace("ncdf4", quietly = TRUE)) stop("The package ncdf4 is required for reading NCDF4 files!")
      ncFile <- ncdf4::nc_open(file_name)
      options("magclass.verbosity" = 1)

      coord <- magclassdata$half_deg[, c("lon", "lat")]

      if (!(max(ncFile$dim$lat$vals) >= max(coord$lat)) | !(min(ncFile$dim$lat$vals) <= min(coord$lat)) |
        !(max(ncFile$dim$lon$vals) >= max(coord$lon)) | !(min(ncFile$dim$lon$vals) <= min(coord$lon))) {

        stop(paste("Only netcdf files with 0.5 degree resolution with extend from", min(coord$lon), max(coord$lon),
          min(coord$lat), max(coord$lat), "are supported.", "Your file has a extend of",
          min(ncFile$dim$lon$vals), max(ncFile$dim$lon$vals), min(ncFile$dim$lat$vals),
          max(ncFile$dim$lat$vals), "."))
      }

      if (is.null(ncFile$dim$time$len)) ncFile$dim$time$len <- 1
      if (is.null(ncFile$dim$time$vals)) ncFile$dim$time$vals <- 1995

      if (length(ncFile$groups) == 1) {
        varNames <- names(ncFile$var)
      } else {
        varNames <- NULL
        for (i in 1:ncFile$nvars) {
          varName <- ncFile$var[[i]]$longname
          groupIndex <- ncFile$var[[i]]$group_index
          groupName <- ncFile$groups[[groupIndex]]$fqgn
          varNames <- c(varNames, paste(groupName, varName, sep = "/"))
          varNames <- gsub("/", ".", varNames)
        }
      }

      # create a single array of all ncdf variables
      ncData <- array(NA, dim = c(ncFile$dim$lon$len, ncFile$dim$lat$len, ncFile$dim$time$len, ncFile$nvars))
      for (i in 1:ncFile$nvars) {
        ncData[, , , i] <- ncdf4::ncvar_get(ncFile, varid = names(ncFile$var)[i])
      }

      # taking out lat and lon from nc file
      lat <- ncFile$dim$lat$vals
      lon <- ncFile$dim$lon$vals
      # coord from magclass data
      coord <- magclassdata$half_deg[, c("lon", "lat")]

      # reorder ncdf array into magpie cellular format (still as array)
      # create emtpy array in magpie cellular format
      mag <- array(NA, dim = c(59199, ncFile$dim$time$len, ncFile$nvars),
        dimnames = list(paste("GLO", 1:59199, sep = "."),
          paste("y", ncFile$dim$time$vals, sep = ""), varNames))
      # Loop over cells to give mag values taken from ncData. For each cell in mag, we know the exact
      # coordinates (coord). Hence, we can use coord to map coordinates in ncData to cells in mag.
      for (i in 1:ncells(mag)) {
        mag[i, , ] <- ncData[which(coord[i, 1] == lon), which(coord[i, 2] == lat), , ]
      }

      # convert array to magpie object
      readMagpie <- clean_magpie(as.magpie(mag, temporal = 2))
    } else {
      # check for header
      if (file_type == "put") {
        temp <- read.csv(file_name, nrow = 1, header = FALSE, sep = "\t",
          comment.char = comment.char, check.names = check.names, stringsAsFactors = TRUE)
      } else {
        temp <- read.csv(file_name, nrow = 1, header = FALSE, comment.char = comment.char,
          check.names = check.names, stringsAsFactors = TRUE)
      }

      # check for numeric elements in first row, which means a missing header
      header <- TRUE
      for (temp_elem in temp) {
        if (is.numeric(temp_elem)) header <- FALSE
      }

      if (file_type == "put") {
        temp <- read.csv(file_name, header = header, sep = "\t", comment.char = comment.char,
          check.names = check.names, stringsAsFactors = TRUE)
      } else {
        temp <- read.csv(file_name, header = header, comment.char = comment.char,
          check.names = check.names, stringsAsFactors = TRUE)
      }

      # analyse column content
      coltypes <- rep(0, dim(temp)[2])
      for (column in 1:dim(temp)[2]) {
        if (sum(coltypes == "year") == 0 &
          length(grep("^(y[0-9]{4}|[0-2][0-9]{3})$", temp[, column])) == dim(temp)[1]) {
          coltypes[column] <- "year"
        } else if (sum(coltypes == "region") == 0 & sum(coltypes == "regiospatial") == 0 &
          length(grep("^[A-Z]{3}$", temp[, column])) == dim(temp)[1]) {
          coltypes[column] <- "region"
        } else if (sum(coltypes == "regiospatial") == 0 & sum(coltypes == "region") == 0 &
          length(grep("^[A-Z]{3}_[0-9]+$", temp[, column])) == dim(temp)[1]) {
          coltypes[column] <- "regiospatial"
        } else if (!is.numeric(temp[1, column])) {
          coltypes[column] <- "other"
        } else if (sum(coltypes == "cell") == 0 & all(!is.na(temp[, column])) & all(temp[, column] != 0)) {
          if (length(temp[, column]) %% max(temp[, column]) == 0) {
            if (suppressWarnings(try(all(unique(temp[, column]) == 1:max(temp[, column])), silent = TRUE) == TRUE)) {
              coltypes[column] <- "cell"
            } else {
              coltypes[column] <- "data"
            }
          } else {
            coltypes[column] <- "data"
          }
        } else {
          coltypes[column] <- "data"
        }
      }

      if (any(coltypes == "year")) {
        temp <- temp[order(temp[, which(coltypes == "year")]), ]
        if (length(grep("y", temp[, which(coltypes == "year")])) == 0) {
          temp[, which(coltypes == "year")] <- as.factor(paste("y", temp[, which(coltypes == "year")], sep = ""))
        }
      }

      # backup check if cell column is really a cell column
      if (any(coltypes == "cell")) {
        if (dimnames(temp)[[2]][which(coltypes == "cell")] == "iteration") {
          temp[, which(coltypes == "cell")] <- paste("iter", format(temp[, which(coltypes == "cell")]), sep = "")
          coltypes[which(coltypes == "cell")] <- "other"
        } else if (header & !(dimnames(temp)[[2]][which(coltypes == "cell")] %in%
          c("dummy", "dummy.1", "dummy.2", "dummy.3", "", " ",
            "cell", "cells", "Cell", "Cells"))) {
          coltypes[which(coltypes == "cell")] <- "data"
        }
      }

      if (any(coltypes == "cell")) {
        ncells <- dim(temp)[1]
        if (any(coltypes == "year")) ncells <- ncells / length(unique(temp[, which(coltypes == "year")]))
        if (any(coltypes == "other")) ncells <- ncells / length(unique(temp[, which(coltypes == "other")]))
        if (!all(temp[1:ncells, which(coltypes == "cell")] == 1:ncells)) coltypes[which(coltypes == "cell")] <- "data"
      }

      # set all coltypes after the first occurrence of "data" to "data"
      if (any(coltypes == "data")) coltypes[min(which(coltypes == "data")):length(coltypes)] <- "data"

      # set first columntype from "cell" to "data" if it seems that the data set is just a vector of numbers
      if (all(coltypes == c("cell", rep("data", length(coltypes) - 1))) & dim(temp)[1] == 1) coltypes[1] <- "data"

      # check coltypes for consistency
      if (length(which(coltypes == "data")) == 0) {
        print(coltypes)
        stop(paste("Inconsistency in data columns! No data column found in", file_name))
      }

      if (sum(coltypes == "data") != (length(coltypes) - min(which(coltypes == "data")) + 1)) {
        print(coltypes)
        stop("Inconsistency in data columns!")
      }
      if (!all(which(coltypes == "data") == min(which(coltypes == "data")):length(coltypes))) {
        print(coltypes)
        stop("Inconsistency in data columns!")
      }
      if (sum(coltypes == "data") == 0) {
        print(coltypes)
        stop("No data column found!")
      }
      if (sum(coltypes == "other") > 1) {
        print(coltypes)
        stop("Invalid format. More than one \"other\" column is not allowed!")
      }

      if (header) {
        if (length(grep("^y+[0-9]{4}$", dimnames(temp)[[2]][which(coltypes == "data")[1]])) == 1) {
          headertype <- "year"
        } else if (length(grep("^[A-Z]{3}$", dimnames(temp)[[2]][which(coltypes == "data")[1]])) == 1) {
          headertype <- "region"
        } else {
          headertype <- "other"
        }
      } else {
        headertype <- "none"
      }

      if (any(coltypes == "other")) {
        othernames <- levels(as.factor(temp[, which(coltypes == "other")]))
        nother <- length(othernames)
        if (header) {
          if (headertype == "other") {
            elemnames <- dimnames(temp)[[2]][which(coltypes == "data")]
            elemnames <- paste(rep(othernames, each = length(elemnames)), elemnames, sep = ".")
          } else {
            elemnames <- othernames
          }
        } else {
          if (sum(coltypes == "data") == 1) {
            elemnames <- othernames
          } else {
            elemnames <- 1:sum(coltypes == "data")
            elemnames <- paste(rep(othernames, each = length(elemnames)), elemnames, sep = ".")
          }
        }
        ncols <- length(elemnames)
      } else {
        nother <- 1
        if (header) {
          if (headertype == "other") {
            elemnames <- dimnames(temp)[[2]][which(coltypes == "data")]
            ncols <- length(elemnames)
          } else {
            elemnames <- NULL
            ncols <- 1
          }
        } else {
          elemnames <- NULL
          ncols <- sum(coltypes == "data")
        }
      }


      if (any(coltypes == "year")) {
        yearnames <- levels(temp[, which(coltypes == "year")])
        nyears <- length(yearnames)
      } else if (headertype == "year") {
        yearnames <- dimnames(temp)[[2]][which(coltypes == "data")]
        nyears <- length(yearnames)
      } else {
        yearnames <- NULL
        nyears <- 1
      }

      if (any(coltypes == "cell")) {
        ncells <- max(temp[, which(coltypes == "cell")])
      } else {
        if (headertype != "year") {
          ncells <- dim(temp)[1] / (nyears * nother)
        } else {
          ncells <- dim(temp)[1] / nother
        }
      }

      if (any(coltypes == "regiospatial")) {
        cellnames <- gsub("_", ".", temp[1:ncells, which(coltypes == "regiospatial")], fixed = TRUE)
      } else {
        if (any(coltypes == "region")) {
          tmpRegionnames <- levels(temp[, which(coltypes == "region")])
          regionnames <- tmpRegionnames[temp[, which(coltypes == "region")]]
          if (ncells == length(tmpRegionnames)) regionnames <- unique(regionnames)
        } else if (headertype == "region") {
          regionnames <- dimnames(temp)[[2]][which(coltypes == "data")]
          ncells <- ncells * length(regionnames)
        } else {
          regionnames <- "GLO"
        }
        if (length(unique(regionnames)) < length(regionnames)) {
          cellnames <- paste(regionnames, 1:ncells, sep = ".")
        } else {
          cellnames <- regionnames
        }
      }
      if (length(cellnames) == 1) cellnames <- list(cellnames)

      if (any(coltypes == "other") & (headertype == "other" | headertype == "none")) {
        output <- array(NA, c(ncells, nyears, ncols))
        dimnames(output)[[1]] <- cellnames
        dimnames(output)[[2]] <- yearnames
        dimnames(output)[[3]] <- elemnames
        counter <- 0
        for (other.elem in othernames) {
          output[, , (1:sum(coltypes == "data")) + counter] <- array(as.vector(
            as.matrix(temp[which(temp[, which(coltypes == "other")] == other.elem),
              which(coltypes == "data")])), c(ncells, nyears, sum(coltypes == "data")))
          counter <- counter + sum(coltypes == "data")
        }
      } else if (!any(coltypes == "other") & headertype == "region") {
        output <- array(NA, c(ncells, nyears, ncols))
        dimnames(output)[[1]] <- cellnames
        dimnames(output)[[2]] <- yearnames
        dimnames(output)[[3]] <- elemnames
        for (i in seq_along(cellnames)) {
          output[i, , 1] <- temp[, which(coltypes == "data")[i]]
        }
      } else if (!any(coltypes == "other") & headertype == "year") {
        output <- array(NA, c(ncells, nyears, ncols))
        dimnames(output)[[1]] <- cellnames
        dimnames(output)[[2]] <- yearnames
        dimnames(output)[[3]] <- elemnames
        for (year in yearnames) {
          output[, year, 1] <- temp[, year]
        }
      } else if (any(coltypes == "other") & headertype == "region") {
        output <- array(NA, c(ncells, nyears, ncols))
        dimnames(output)[[1]] <- cellnames
        dimnames(output)[[2]] <- yearnames
        dimnames(output)[[3]] <- elemnames
        for (i in seq_along(cellnames)) {
          for (elem in elemnames) {
            output[i, , elem] <- temp[which(temp[, which(coltypes == "other")] == elem), which(coltypes == "data")[i]]
          }
        }
      } else if (any(coltypes == "other") & headertype == "year") {
        output <- array(NA, c(ncells, nyears, ncols))
        dimnames(output)[[1]] <- cellnames
        dimnames(output)[[2]] <- yearnames
        dimnames(output)[[3]] <- elemnames
        for (year in yearnames) {
          for (elem in elemnames) {
            output[, year, elem] <- temp[which(temp[, which(coltypes == "other")] == elem), year]
          }
        }
      } else {
        output <- array(as.vector(as.matrix(temp[, which(coltypes == "data")])), c(ncells, nyears, ncols))
        dimnames(output)[[1]] <- cellnames
        dimnames(output)[[2]] <- yearnames
        dimnames(output)[[3]] <- elemnames
      }
      readMagpie <- output
      attr(readMagpie, "comment") <- .readComment(file_name, comment.char = comment.char)
    }
  } else {
    warning(paste("File", file_name, "does not exist"))
    readMagpie <- NULL
  }
  readMagpie <- as.magpie(readMagpie)
  if (as.array) readMagpie <- as.array(readMagpie)[, , ]
  return(readMagpie)
}
