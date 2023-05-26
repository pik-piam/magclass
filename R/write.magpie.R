#' Write MAgPIE-object to file
#'
#' Writes a MAgPIE-3D-array (cells,years,datacolumn) to a file in one of three
#' MAgPIE formats (standard, "magpie", "magpie zipped")
#'
#' This function can write 13 different MAgPIE file_types. "cs2" is the new
#' standard format for cellular data with or without header and the first
#' columns (year,regiospatial) or only (regiospatial), "cs2b" is identical to
#' "cs2" except that it will suppress the data name if it has only 1 element
#' in the data dimension. "csv" is the standard format for regional
#' data with or without header and the first columns (year,region,cellnumber)
#' or only (region,cellnumber), "cs3" is another csv format which is
#' specifically designed for multidimensional data for usage in GAMS.
#' All these variants are written without further specification. "rds" is
#' a R-default format for storing R objects.
#' "magpie" (.m) and "magpie zipped" (.mz) are new formats developed to allow a
#' less storage intensive management of MAgPIE-data. The only difference
#' between both formats is that .mz is gzipped whereas .m is not compressed. So
#' .mz needs less memory, whereas .m might have a higher compatibility to other
#' languages. "asc" is the ASCII grid format. "nc" is the netCDF format.  It
#' can only be applied for half degree data and writes one file per year per
#' data column. In the case that more than one year and data column is supplied
#' several files are written with the structure filename_year_datacolumn.asc
#'
#' @param x a magclass object. An exception is that formats written via the raster package
#' (currently "nc", "asc", "grd" and "tif") also accept RasterBrick objects which have
#' been previously created from a magclass object via as.RasterBrick)
#' @param file_name file name including file ending (wildcards are supported).
#' Optionally also the full path can be specified here (instead of splitting it
#' to file_name and file_folder)
#' @param file_folder folder the file should be written to (alternatively you
#' can also specify the full path in file_name - wildcards are supported)
#' @param file_type Format the data should be stored as. Currently the following formats
#' are available: "rds" (default R-data format), "cs2" (cellular standard
#' MAgPIE format), "cs2b" (cellular standard MAgPIE format with suppressed header ndata=1),
#' "csv" (regional standard MAgPIE format), "cs3" (Format for multidimensional MAgPIE
#' data, compatible to GAMS), "cs4" (alternative multidimensional format compatible
#' to GAMS, in contrast to cs3 it can also handle sparse data), "cs5" (more generalized version of cs4),
#' "csvr", "cs2r", "cs3r" and "cs4r" which are the same formats as the previous mentioned ones
#' with the only difference that they have a REMIND compatible format, "m"
#' (binary MAgPIE format "magpie"), "mz" (compressed binary MAgPIE format
#' "magpie zipped"), "asc" (ASCII grid format), "nc" (netCDF format), "tif"
#' (GEOtiff format) and "grd" (native raster format). If file_type=NULL
#' the file ending of the file_name is used as format. If
#' format is different to the formats mentioned standard MAgPIE format is
#' assumed. Please be aware that the file_name is independent of the
#' file_type you choose here, so no additional file ending will be added!
#' @param append Decides whether an existing file should be overwritten (FALSE)
#' or the data should be added to it (TRUE). Append = TRUE only works if the
#' existing data can be combined with the new data using the mbind function
#' @param comment Vector of strings: Optional comment giving additional
#' information about the data. If different to NULL this will overwrite the
#' content of attr(x,"comment")
#' @param comment.char character: a character vector of length one containing a
#' single character or an empty string. Use "" to turn off the interpretation
#' of comments altogether.
#' @param mode File permissions the file should be written with as 3-digit
#' number (e.g. "777" means full access for user, group and all, "750" means
#' full access for user, read access for group and no acess for anybody else).
#' Set to NULL system defaults will be used. Access codes are identical to the
#' codes used in unix function chmod.
#' @param zname Time variable for the writeRaster function
#' @param ... additional arguments passed to specific write functions
#' @note
#'
#' The binary MAgPIE formats .m and .mz have the following content/structure
#' (you only have to care for that if you want to implement
#' read.magpie/write.magpie functions in other languages): \cr \cr
#' [ FileFormatVersion | Current file format version number (currently 6) | integer | 2 Byte ] \cr
#' [ nchar_comment | Number of character bytes of the file comment | integer | 4 Byte ] \cr
#' [ nbyte_metadata | Number of bytes of the serialized metadata (currently = 0) | integer | 4 Byte ] \cr
#' [ nchar_sets | Number of characters bytes of all regionnames + 2 delimiter | integer | 2 Byte] \cr
#' [ nyears | Number of years | integer | 2 Byte ]\cr
#' [ yearList | All years of the dataset (0, if year is not present) | integer | 2*nyears Byte ] \cr
#' [ ncells | Number of cells | integer | 4 Byte ]\cr
#' [ nchar_cell | Number of characters bytes of all regionnames + (nreg-1) for delimiters | integer | 4 Byte ] \cr
#' [ cells | Cell names saved as cell1\\cell2 (\\n is the delimiter) | character | 1*nchar_cell Byte ] \cr
#' [ nelem | Total number of data elements | integer | 4 Byte ] \cr
#' [ nchar_data | Number of char. bytes of all datanames + (ndata - 1) for delimiters | integer | 4 Byte ] \cr
#' [ datanames | Names saved in the format data1\\ndata2 (\\n as del.) | character | 1*nchar_data Byte ] \cr
#' [ data | Data of the MAgPIE array in vectorized form | numeric | 4*nelem Byte ] \cr
#' [ comment | Comment with additional information about the data | character | 1*nchar_comment Byte ] \cr
#' [ sets | Set names with \\n as delimiter | character | 1*nchar_sets Byte] \cr
#' [ metadata | serialized metadata information (currently not in use) | bytes | 1*nbyte_metadata Byte] \cr
#'
#' @author Jan Philipp Dietrich, Stephen Bi, Florian Humpenoeder
#' @seealso \code{"\linkS4class{magpie}"},
#' \code{\link{read.magpie}},\code{\link{mbind}}
#' @examples
#'
#' # a <- read.magpie("lpj_yield_ir.csv")
#' # write.magpie(a,"lpj_yield_ir.mz")
#' @importFrom utils write.csv write.table
#' @export
write.magpie <- function(x, file_name, file_folder = "", file_type = NULL, append = FALSE, comment = NULL, # nolint
                         comment.char = "*", mode = NULL, zname = "Time",...) {       # nolint
  umask <- Sys.umask()
  if (!is.null(mode)) {
    umaskMode <- as.character(777 - as.integer(mode))
    Sys.umask(umaskMode)
  } else {
    mode <- as.character(777 - as.integer(as.character(umask)))
  }
  if (is.null(x)) x <- as.magpie(numeric(0))
  if (is.magpie(x) || inherits(x, "RasterBrick")) {
    years <- !(is.null(dimnames(x)[[2]]))

    # if file-type is not mentioned file-ending is used as file-type
    if (is.null(file_type)) {
      file_type <- tail(strsplit(file_name, "\\.")[[1]], 1) # nolint
    }
    if (inherits(x, "RasterBrick") && !(file_type %in% c("nc", "asc", "grd", "tif"))) {
      stop("RasterBrick format is only allowed for file types: nc, asc, grd and tif")
    }
    if (!file_folder == "") {
      filePath <- paste(file_folder, file_name, sep = "/")
    } else {
      filePath <- file_name # nolint
    }

    # look for comment/additional information
    if (is.null(comment) && !is.null(attr(x, "comment"))) comment <- attr(x, "comment")
    if (is.null(comment)) comment <- ""
    comment <- unlist(strsplit(comment, "\n"))

    # expand wildcards
    filePath <- file.path(Sys.glob(dirname(filePath)), basename(filePath))
    if (length(filePath) > 1) {
      filePath <- filePath[1]
      warning("file name is ambiguous, only first alternative is used!")
    }

    if (append && file.exists(filePath)) {
      x2 <- read.magpie(filePath)
      x <- mbind(x2, x)
    }

    if (file_type %in% c("m", "mz")) {
      fformatVersion <- "6"  # File format version (oldest data has version 0)
      comment <- paste(comment, collapse = "\n")
      ncells <- dim(x)[1]
      nyears <- dim(x)[2]
      ndata  <- dim(x)[3]
      cells <- dimnames(x)[[1]]
      cellsCollapsed <- paste(cells, collapse = "\n")
      datanames <- dimnames(x)[[3]]
      datanamesCollapsed <- paste(datanames, collapse = "\n")
      setsCollapsed <- paste(getSets(x, fulldim = FALSE), collapse = "\n")

      if (years) {
        yearList <- as.integer(substr(dimnames(x)[[2]], 2, 5))
      } else {
        yearList <- 0
      }

      if (file_type == "mz") {
        zz <- gzfile(filePath, "wb")
      } else {
        zz <- file(filePath, "wb")
      }

      writeBin(as.integer(fformatVersion), zz, size = 2)
      writeBin(as.integer(nchar(comment, type = "bytes")), zz, size = 4)
      writeBin(as.integer(0), zz, size = 4)
      writeBin(as.integer(nchar(setsCollapsed, type = "bytes")), zz, size = 2)
      writeBin(as.integer(c(nyears, yearList)), zz, size = 2)
      writeBin(as.integer(c(ncells, nchar(cellsCollapsed, type = "bytes"))), zz, size = 4)
      writeChar(cellsCollapsed, zz, eos = NULL)
      writeBin(as.integer(c(ndata * ncells * nyears, nchar(datanamesCollapsed, type = "bytes"))), zz, size = 4)
      if (datanamesCollapsed != "") writeChar(datanamesCollapsed, zz, eos = NULL)
      writeBin(as.numeric(as.vector(x)), zz, size = 4)
      if (comment != "") writeChar(comment, zz, eos = NULL)
      if (nchar(setsCollapsed, type = "bytes") > 0) writeChar(setsCollapsed, zz, eos = NULL)
      close(zz)
      Sys.chmod(filePath, mode)
    } else if (file_type %in% c("asc", "grd", "tif")) {
      if (!requireNamespace("raster", quietly = TRUE)) {
        stop("The package \"raster\" is required!")
      }
      format <- c(asc = "ascii", grd = "raster", tif = "GTiff")
      if (is.magpie(x)) {
        if (ndata(x) != 1) stop("Currently no support for multiple variables for file type \"", file_type,
                                "\". Please store each variable separately.")
        varname <- getItems(x, dim = 3)
        zunit <- ifelse(all(isYear(getYears(x))), "years", "")
        x <- as.RasterBrick(x)
      } else if (inherits(x, "RasterBrick")) {
        tmp <- names(x)
        tmp <- strsplit(tmp, "\\..")
        years <- sort(unique(unlist(lapply(tmp, function(x) x[1]))))
        varname <- sort(unique(unlist(lapply(tmp, function(x) x[2]))))
        zunit <- ifelse(all(isYear(years)), "years", "")
        if (length(varname) != 1) stop("Currently no support for multiple variables for file type \"", file_type,
                                "\". Please store each variable separately.")
      }
      if (file_type == "asc") {
        if (dim(x)[3] != 1) stop("asc does not support multiple year layers. Please choose just one!")
        x <- x[[1]]
      }
      if (is.null(varname)) varname <- "Variable"
      raster::writeRaster(x, filename = filePath, format = format[file_type], overwrite = TRUE,
                          zname = zname, zunit = zunit, varname = varname, ...)
    } else if (file_type == "nc") {

      if (!requireNamespace("ncdf4", quietly = TRUE) || !requireNamespace("raster", quietly = TRUE)) {
        stop("The packages \"ncdf4\" and \"raster\" are required!")
      }
      .sub <- function(rx, name) {
        layer <- sub("^.*\\.\\.", "", names(rx))
        if (length(unique(layer)) == 1) return(rx)
        return(rx[[which(layer == name)]])
      }
      if (is.magpie(x)) {
        varnames <- getItems(x, dim = 3)
        zunit <- ifelse(all(isYear(getYears(x))), "years", "")
        years <- getYears(x, as.integer = TRUE)
        x <- as.RasterBrick(x)
      } else if (inherits(x, "RasterBrick")) {
        tmp <- names(x)
        tmp <- strsplit(tmp, "\\..")
        years <- sort(unique(unlist(lapply(tmp, function(x) x[1]))))
        varnames <- sort(unique(unlist(lapply(tmp, function(x) x[2]))))
        zunit <- ifelse(all(isYear(years)), "years", "")
        years <- as.numeric(gsub("y", "", years))
      }
      if (is.null(varnames)) varnames <- "Variable"
      if (is.null(comment)) {
       unit <- "not specified"
      } else {
        indicators <- sub(":.*$", "", comment)
        units <- sub("^.*: ", "", comment)
        if (any(grepl("unit", indicators))) {
          unit <- units[grep("unit", indicators)]
        } else {
          unit <- "not specified"
        }
      }
      # raster is using partial matching resulting in a warning if warnPartialMatchDollar is set
      suppressSpecificWarnings({
        raster::writeRaster(.sub(x, varnames[1]), filename = filePath, format = "CDF", overwrite = TRUE,
                            compression = 9, zname = zname, zunit = zunit, varname = varnames[1], varunit = unit, ...)
      }, "partial match of 'group' to 'groups'", fixed = TRUE)
      nc <- ncdf4::nc_open(filePath, write = TRUE)
      if (zunit == "years") {
        try(ncdf4::ncvar_put(nc, zname, years), silent = TRUE)
      }
      if (length(varnames) > 1) {
        for (i in varnames[-1]) {
          suppressSpecificWarnings({
            nc <- ncdf4::ncvar_add(nc, ncdf4::ncvar_def(i, unit, nc$dim, compression = 9))
          }, "partial match of 'group' to 'groups'", fixed = TRUE)
          ncdf4::ncvar_put(nc, i, aperm(as.array(.sub(x, i)), c(2, 1, 3)))
        }
      }
      ncdf4::nc_close(nc)
    } else if (file_type == "rds") {
      saveRDS(object = x, file = filePath, ...)
    } else if (file_type == "cs3" || file_type == "cs3r") {
      if (file_type == "cs3r") dimnames(x)[[2]] <- sub("y", "", dimnames(x)[[2]])
      if (dim(x)[3] != prod(sapply(getItems(x, dim = 3, split = TRUE), length))) { # nolint
        stop("Input data seems to be sparse but ", file_type, " does not support sparse data. Please use ",
             sub("3", "4", file_type), " instead!")
      }
      x <- unwrap(x)
      if (dim(x)[1] == 1 && (is.null(dimnames(x)[[1]]) || length(grep("GLO", dimnames(x)[[1]])) == 1)) {
        dimnames(x)[[1]] <- "TODELETE"
      } else {
        if (nregions(x) == dim(x)[1]) {
          dimnames(x)[[1]] <- sub("\\..*$", "", dimnames(x)[[1]])
        } else {
          dimnames(x)[[1]] <- sub("\\.", "_", dimnames(x)[[1]])
        }
      }
      x <- wrap(x, map = list(NA, length(dim(x))))
      dimnames(x)[[1]] <- sub("^([^\\.]*)\\.([^\\.]*)", "\\2\\.\\1", dimnames(x)[[1]])

      dimnames(x)[[1]] <- gsub("TODELETE", "", dimnames(x)[[1]])
      dimnames(x)[[1]] <- gsub("\\.\\.", "\\.", dimnames(x)[[1]])
      dimnames(x)[[1]] <- gsub("^\\.", "", dimnames(x)[[1]])
      dimnames(x)[[1]] <- gsub("\\.$", "", dimnames(x)[[1]])
      dimnames(x)[[1]] <- gsub("\\.", ",", dimnames(x)[[1]])


      header <- dimnames(x)[[2]]
      x <- cbind(dimnames(x)[[1]], x)
      dimnames(x)[[2]] <- c(gsub("[^,]*(,|$)", "dummy\\1", x[1, 1]), header)
      zz <- file(filePath, open = "w")
      if (any(comment != "")) writeLines(paste(comment.char, comment, sep = ""), zz)
      write.csv(x, file = zz, quote = FALSE, row.names = FALSE)
      close(zz)
      Sys.chmod(filePath, mode)
    } else if (file_type == "cs4" || file_type == "cs4r") {
      printCells <- nregions(x) < ncells(x)
      printRegions <- (!is.null(getItems(x, dim = 1.1)) && getItems(x, dim = 1.1)[1] != "GLO")
      printData <- ((ndata(x) > 1) | !is.null(getNames(x)))

      output <- as.data.frame(x)
      output <- output[c("Year", "Region", "Cell", names(output)[-c(1:3)])]

      if (!printCells) output["Cell"] <- NULL
      if (!printRegions) output["Region"] <- NULL
      if (!printData) output["Data1"] <- NULL
      if (!years) {
        output["Year"] <- NULL
      } else {
        if (file_type == "cs4") levels(output[["Year"]]) <- paste0("y", levels(output[["Year"]]))
      }
      zz <- file(filePath, open = "w")
      if (any(comment != "")) writeLines(paste(comment.char, comment, sep = ""), zz)
      write.table(output, file = zz, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
      close(zz)
      Sys.chmod(filePath, mode)
    } else if (file_type == "cs5") {
      output <- as.data.frame(x, rev = 3, raw = TRUE)

      .formatMeta <- function(meta, commentChar) {
        out <- NULL
        for (n in names(meta)) {
          out <- c(out, paste0(commentChar, "META ", n, ": ", paste(meta[[n]], collapse = ", ")))
        }
        return(out)
      }

      zz <- file(filePath, open = "w")
      if (any(comment != "")) writeLines(paste(comment.char, comment, sep = ""), zz)
      writeLines(.formatMeta(attributes(output)[c("names", "dimtype")], comment.char), zz)
      write.table(output, file = zz, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
      close(zz)

    } else {
      printCells <- nregions(x) < ncells(x)
      printRegions <- (!is.null(getItems(x, dim = 1.1)) && getItems(x, dim = 1.1)[1] != "GLO")
      printData <- ((ndata(x) > 1) | !is.null(getNames(x)))

      if (file_type == "cs2b" && ndata(x) == 1) getNames(x) <- NULL

      # non-cellular data
      if (!printCells && (!printData || !years || !printRegions)) {
        if (file_type == "csvr" || file_type == "cs2r") dimnames(x)[[2]] <- sub("y", "", dimnames(x)[[2]])
        if (!printData) {
          output <-  array(x, dim = dim(x)[1:2], dimnames = list(dimnames(x)[[1]], dimnames(x)[[2]]))
          output <- aperm(output)
          if (printRegions) {
            if (all(dimExists(c(1.2, 1.3), x) == c(TRUE, FALSE))) {
              tmp <- sub("\\.[^.]*$", "", dimnames(x)[[1]])
            } else {
              tmp <- dimnames(x)[[1]]
            }
            output <- rbind(tmp, output)
            if (years) output <- cbind(c("dummy", dimnames(x)[[2]]), output)
          } else {
            if (years) output <- cbind(dimnames(x)[[2]], output)
          }
          header <- FALSE
        } else if (!years) {
          output <-  array(x, dim = dim(x)[c(1, 3)], dimnames = list(dimnames(x)[[1]], dimnames(x)[[3]]))
          header <- !is.null(dimnames(output)[[2]])
          if (printRegions) output <- cbind(substring(dimnames(x)[[1]], 1, 3), output)
          if (header && !printRegions) {
            output <- t(output)
            header <- FALSE
            output <- cbind(dimnames(x)[[3]], output)
          }
        } else {
          output <-  array(x, dim = dim(x)[2:3], dimnames = list(dimnames(x)[[2]], dimnames(x)[[3]]))
          header <- !is.null(dimnames(output)[[2]])
          output <- cbind(dimnames(x)[[2]], output)
          dimnames(output)[[2]][1] <- "dummy"
        }
        if (header && printRegions) dimnames(output)[[2]][1] <- "dummy"
        zz <- file(filePath, open = "w")
        if (any(comment != "")) writeLines(paste(comment.char, comment, sep = ""), zz)
        write.table(output, zz, sep = ",", col.names = header, row.names = FALSE, quote = FALSE)
        close(zz)
        Sys.chmod(filePath, mode)
      } else {
        if (file_type == "csvr" || file_type == "cs2r") dimnames(x)[[2]] <- sub("y", "", dimnames(x)[[2]])
        if (file_type %in% c("cs2", "cs2b", "cs2r")) printRegions <- FALSE
        output <- array(NA, c(dim(x)[1] * dim(x)[2], dim(x)[3] + printRegions + printCells + years))
        output[, (1 + printRegions + printCells + years):dim(output)[2]] <- as.vector(as.matrix(x))
        if (years) {
          yearvec <- c()
          for (year in dimnames(x)[[2]]) yearvec <- c(yearvec, rep(year, dim(x)[1]))
          output[, 1] <- yearvec
        }
        if (printRegions) output[, 1 + years] <- substring(rep(dimnames(x)[[1]], dim(x)[2]), 1, 3)
        if (printCells) {
          if (file_type %in% c("cs2", "cs2b", "cs2r")) {
            output[, 1 + printRegions + years] <- rep(gsub(".", "_", dimnames(x)[[1]], fixed = TRUE), dim(x)[2])
          } else {
            output[, 1 + printRegions + years] <- rep(seq_len(dim(x)[1]), dim(x)[2])
          }
        }
        if (!is.null(dimnames(x)[[3]])) {
          dimnames(output)[[2]] <- c(rep("dummy", printRegions + printCells + years), dimnames(x)[[3]])
          header <- TRUE
        } else {
          header <- FALSE
        }
        zz <- file(filePath, open = "w")
        if (any(comment != "")) writeLines(paste0(comment.char, comment, sep = ""), zz)
        write.table(output, zz, sep = ",", col.names = header, row.names = FALSE, quote = FALSE)
        close(zz)
        Sys.chmod(filePath, mode)
      }
    }
  } else {
    stop("Input is not in MAgPIE-format!")
  }
  if (!is.null(mode)) Sys.umask(umask)
}
