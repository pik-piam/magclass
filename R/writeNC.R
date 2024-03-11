#' Write a magpie object to a netCDF file
#'
#' @param x A magpie object
#' @param filename Name of the netCDF file to write
#' @param unit Unit of the data, to omit pass "" (empty string)
#' @param ... For future expansion
#' @param compression Level of compression to use (1-9), NA for no compression
#' @param missval The value that encodes NA in the resulting netCDF file
#' @param gridDefinition A vector of 5 numeric values: c(xMin, xMax, yMin, yMax, resolution).
#' Use c(-179.75, 179.75, -89.75, 89.75), 0.5) to write a standard 0.5-degree-resolution
#' lon/lat grid. If NULL, use min/max of coordinates in x and guessResolution
#' @param zname Name of the z dimension in the netCDF file
#' @param chunkSize Data is written in dense grid chunks of this size,
#' smaller values reduce memory footprint, but increase write time
#' @author Pascal Sauer
writeNC <- function(x, filename, unit, ..., compression = 2, missval = NA,
                    gridDefinition = NULL, zname = "time", chunkSize = 250000) {
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("The ncdf4 package is required to write netCDF files, please install it.")
  }
  # fail immediately if arguments are not set
  stopifnot(is.character(filename), is.character(unit))
  if (!(...length() == 0 || all(...names() == "verbose"))) {
    stop("Unknown argument passed to writeNC: ", paste(...names(), collapse = ", "))
  }

  if (is.null(gridDefinition)) {
    coords <- getCoords(x)
    firstX <- min(coords$x)
    lastX <- max(coords$x)
    firstY <- min(coords$y)
    lastY <- max(coords$y)
    res <- guessResolution(coords)
  } else {
    stopifnot(length(gridDefinition) == 5,
              gridDefinition[1] < gridDefinition[2],
              gridDefinition[3] < gridDefinition[4])
    firstX <- gridDefinition[1]
    lastX <- gridDefinition[2]
    firstY <- gridDefinition[4]
    lastY <- gridDefinition[3]
    res <- gridDefinition[5]
  }
  xCoords <- seq(min(coords$x), max(coords$x), res)
  yCoords <- seq(max(coords$y), min(coords$y), -res)

  if (zname != "time") {
    message("terra will not recognize zname != 'time' as time dimension")
  }

  if (is.null(getItems(x, 3))) {
    getItems(x, 3) <- sub("\\.nc$", "", basename(filename))
  }

  # create netCDF file
  dimVars <- list(ncdf4::ncdim_def("lon", "degrees_east", xCoords),
                  ncdf4::ncdim_def("lat", "degrees_north", yCoords))
  hasTime <- !is.null(getItems(x, 2))
  if (hasTime) {
    dimVars <- c(dimVars, list(ncdf4::ncdim_def(zname, "years since 0", getYears(x, as.integer = TRUE), unlim = TRUE)))
  }
  vars <- lapply(getItems(x, 3), function(vname) {
    return(ncdf4::ncvar_def(vname, units = unit, dim = dimVars,
                            missval = missval, compression = compression))
  })

  nc <- ncdf4::nc_create(filename, vars = vars)
  withr::defer(ncdf4::nc_close(nc))

  if (hasTime && zname == "time") {
    ncdf4::ncatt_put(nc, "time", "axis", "T")
  }

  nYears <- max(1, length(getYears(x)))
  rowsPerChunk <- max(1, floor(chunkSize / nYears / length(xCoords)))
  for (vname in getItems(x, 3)) {
    for (i in seq.int(1, length(yCoords), rowsPerChunk)) {
      chunk <- extend(x[, , vname], c(firstX, lastX),
                      c(yCoords[i], yCoords[min(i + rowsPerChunk - 1, length(yCoords))]),
                      res = res, checkInRange = FALSE)
      ncdf4::ncvar_put(nc, vname, chunk,
                       start = c(1, i, if (hasTime) 1),
                       count = c(-1, length(chunk) / nYears / length(xCoords),
                                 if (hasTime) -1))
    }
  }
}
