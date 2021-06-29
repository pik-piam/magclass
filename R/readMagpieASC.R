readMagpieASC <- function(fileName) {

    grid <- suppressWarnings(try(maptools::readAsciiGrid(fileName, dec = "."), silent = T))
    if (is(grid, "try-error")) {
      grid <- try(maptools::readAsciiGrid(fileName, dec = ","))
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
    return(as.magpie(readMagpie))
}
