
readMagpieMZ <- function(fileName, compressed) {

  if (compressed) {
    zz <- gzfile(fileName, "rb")
  } else {
    zz <- file(fileName, "rb")
  }
  on.exit(close(zz))

  newestFformatVersion <- 6
  fformatVersion <- readBin(zz, integer(), 1, size = 2)
  if (fformatVersion > newestFformatVersion) {
    stop("File format is newer (v", fformatVersion, ") than the formats supported (v1-v",
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

  nyears    <- readBin(zz, integer(), 1, size = 2)
  yearList <- readBin(zz, integer(), nyears, size = 2)
  useBytes  <- (fformatVersion > 4)
  if (fformatVersion > 5) {
    ncells      <- readBin(zz, integer(), 1, size = 4)
    ncharCells  <- readBin(zz, integer(), 1, size = 4)
    cellnames   <- strsplit(readChar(zz, ncharCells, useBytes = TRUE), "\n", useBytes = TRUE)[[1]]
  } else {
    nregions      <- readBin(zz, integer(), 1, size = 2)
    ncharRegions  <- readBin(zz, integer(), 1, size = ifelse(fformatVersion > 3, 4, 2))
    regions       <- strsplit(readChar(zz, ncharRegions, useBytes = useBytes), "\n", useBytes = useBytes)[[1]]
    cpr           <- readBin(zz, integer(), nregions, size = 4)
    ncells <- sum(cpr)
    if (any(cpr != 1)) {
      cellnames   <- paste(rep(regions, cpr), seq_len(ncells), sep = ".")
    } else {
      cellnames   <- regions
    }
  }
  nelem <- readBin(zz, integer(), 1, size = 4)
  ncharData <- readBin(zz, integer(), 1, size = 4)

  datanames <- strsplit(readChar(zz, ncharData, useBytes = useBytes), "\n", useBytes = useBytes)[[1]]

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
  attr(output, "FileFormatVersion") <- fformatVersion
  return(new("magpie", output))
}
