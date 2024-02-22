#' Write a magpie object to a netCDF file
#'
#' @param x A magpie object
#' @param filename Name of the netCDF file to write
#' @param unit Unit of the data, to omit pass "" (empty string)
#' @param ... For future expansion.
#' @param compression Level of compression to use (1-9), NA for no compression
#' @param missval The value that encodes NA in the resulting netCDF file
#' @param res Resolution of the data, if not provided it will be guessed
#' @param zname Name of the z dimension in the netCDF file
#' @author Pascal Sauer
#' @export
writeNC <- function(x, filename, unit, ..., compression = 2, missval = NA,
                    lonCoords = NULL, latCoords = NULL, res = NULL, zname = "time") {
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("The ncdf4 package is required to write netCDF files, please install it.")
  }
  # fail immediately if arguments are not set
  stopifnot(is.character(filename), is.character(unit), ...length() == 0)

  # magclass objects are sparse, fill gaps with NA
  if (is.null(lonCoords) || is.null(latCoords)) {
    xy <- getCoords(x)
    if (is.null(res)) {
      res <- guessResolution(xy)
    }
    lonCoords <- if (is.null(lonCoords)) seq(min(xy$x), max(xy$x), res) else lonCoords
    latCoords <- if (is.null(latCoords)) seq(max(xy$y), min(xy$y), -res) else latCoords
  } else if (!is.null(res)) {
    warning("res is ignored, because both lonCoords and latCoords were passed explicitly.")
  }

  coords <- expand.grid(x = lonCoords, y = latCoords)
  coords <- paste0(coords$x, "|", coords$y)
  coords <- gsub("\\.", "p", coords)
  coords <- sub("\\|", ".", coords)

  getItems(x, 1, raw = TRUE) <- paste0(getItems(x, "x", full = TRUE), ".", getItems(x, "y", full = TRUE))
  names(dimnames(x))[1] <- "x.y"

  extended <- new.magpie(coords, getItems(x, 2), getItems(x, 3))
  extended[getItems(x, 1), , ] <- x
  x <- extended

  if (zname != "time") {
    message("terra will not recognize zname != 'time' as time dimension")
  }

  if (is.null(getItems(x, 3))) {
    getItems(x, 3) <- sub("\\.nc$", "", basename(filename))
  }

  # create netCDF file
  dimVars <- list(ncdf4::ncdim_def("lon", "degrees_east", lonCoords),
                  ncdf4::ncdim_def("lat", "degrees_north", latCoords))
  if (!is.null(getItems(x, 2))) {
    dimVars <- c(dimVars, list(ncdf4::ncdim_def(zname, "years since 0", getYears(x, as.integer = TRUE), unlim = TRUE)))
  }
  vars <- lapply(getItems(x, 3), function(vname) {
    return(ncdf4::ncvar_def(vname, units = unit, dim = dimVars,
                            missval = missval, compression = compression))
  })

  nc <- ncdf4::nc_create(filename, vars = vars)
  withr::defer(ncdf4::nc_close(nc))

  if (!is.null(getItems(x, 2)) && zname == "time") {
    ncdf4::ncatt_put(nc, "time", "axis", "T")
  }

  for (vname in getItems(x, 3)) {
    ncdf4::ncvar_put(nc, vname, x[, , vname])
  }
}
