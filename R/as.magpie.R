#' @importFrom methods new setGeneric
#' @importFrom data.table as.data.table tstrsplit melt

#' @exportMethod as.magpie
setGeneric(
  "as.magpie", function(x, ...) standardGeneric("as.magpie"))

setMethod("as.magpie", signature(x = "magpie"), function(x) return(x))


tmpfilter <- function(x, sep = ".", replacement = "_") {
  .tmpfilter <- function(x, sep = ".", replacement = "_") {
    .tmp <- function(x, sep, replacement) {
      if (sep != replacement) x <- gsub(sep, replacement, x, fixed = TRUE, useBytes = TRUE)
      x[x == ""] <- " "
      return(x)
    }
    if (is.factor(x)) {
      levels(x) <- .tmp(levels(x), sep, replacement)
    } else if (is.character(x)) {
      x <- .tmp(x, sep, replacement)
    }
    return(x)
  }
  cl <- class(x)
  cn <- colnames(x)
  rn <- rownames(x)
  x <- as.data.frame(lapply(x, .tmpfilter, sep = sep, replacement = replacement))
  colnames(x) <- cn
  rownames(x) <- rn
  class(x) <- cl
  return(x)
}

setMethod("as.magpie",
  signature(x = "lpj"),
  function(x, unit = "unknown", ...) {
    xdimnames <- dimnames(x)
    xdim <- dim(x)
    x <- array(x[magclassdata$half_deg$lpj_index, , , ], dim = c(dim(x)[1:2], dim(x)[3] * dim(x)[4]))
    # convert to previous default standard 59199 cells half degree resolution
    dimnames(x) <- list(paste(magclassdata$half_deg$region, 1:59199, sep = "."),
      xdimnames[[2]],
      paste(rep(xdimnames[[3]], xdim[4]), rep(xdimnames[[4]], each = xdim[3]), sep = "."))
    out <- new("magpie", x)
    return(out)
  }
)

setMethod("as.magpie",
          signature(x = "LPJmLData"),
          function(x) {
            if (!requireNamespace("lpjmlkit", quietly = TRUE)) {
              stop("The package \"lpjmlkit\" is required for LPJmLData conversions!")
            }
            y <- lpjmlkit::transform(x, to = c("cell", "time"))
            y <- lpjmlkit::as_array(y)
            if (!is.null(x$grid)) {
              grid <- paste0(sub(".", "p", x$grid$data[, 1], fixed = TRUE), ".",
                             sub(".", "p", x$grid$data[, 2], fixed = TRUE))
              names(grid) <- rownames(x$grid$data)
              dimnames(y)[[1]] <- paste0(grid[dimnames(y)[[1]]], ".", dimnames(y)[[1]])
              names(dimnames(y))[1] <- c("x.y.cell")
            }
            out <- new("magpie", y)
            return(out)
          }
)

setMethod("as.magpie", # nolint
  signature(x = "array"),
  function(x, spatial = NULL, temporal = NULL, unit = "unknown", ...) {
    storeAttributes <- copy.attributes(x, 0)

    # Add the sets as name to the dimnames, if existent
    if (is.null(names(dimnames(x))) & !is.null(attr(x, "sets"))) {
      tmp <- dimnames(x)
      names(tmp) <- attr(x, "sets")
      dimnames(x) <- tmp
    }
    # This part of the function analyses what structure the input has
    d <- list()  # list of dimension types found in the array
    if (!is.null(temporal)) d$temporal <- temporal
    if (!is.null(spatial)) d$regiospatial <- spatial
    for (i in seq_along(dim(x))) {
      if (!is.null(dimnames(x)[[i]])) {
        if (is.null(spatial)) {
          if (length(grep("^(([A-Z]{3})|(glob))$", dimnames(x)[[i]])) == dim(x)[i]) {
            d$regional <- c(d$regional, i)  # regional information
          }
          if (length(grep("^[A-Z]+[\\._][0-9]+$", dimnames(x)[[i]])) == dim(x)[i]) {
            d$regiospatial <- c(d$regiospatial, i)  # regio-spatial information
          }
        }
        if (is.null(temporal)) {
          if (is.temporal(dimnames(x)[[i]])) d$temporal <- c(d$temporal, i) # temporal information
        }
      } else if (dim(x)[i] == 1) {
        d$nothing <- c(d$nothing, i)   # dimension with no content
      }
    }

    if (!is.null(spatial)) {
      if (spatial == 0) {
        d$regiospatial <- NULL
        d$regional <- NULL
      }
    }

    if (!is.null(temporal)) {
      if (temporal == 0) {
        d$temporal <- NULL
      }
    }

    # Write warning when any type (except type "nothing") is found more than once
    nOccurrence <- lapply(d, length) > 1
    nOccurrence <- nOccurrence[names(nOccurrence) != "nothing"]
    if (any(nOccurrence) == TRUE) warning("No clear mapping of dimensions to dimension types. First detected ",
      "possibility is used! Please use arguments temporal and spatial to specify",
      " which dimensions are what!")
    for (i in which(nOccurrence)) {
      d[[i]] <- d[[i]][1]
    }

    # If a regional dimension exists, test whether "glob" appears in the dimnames and rename it with "GLO"
    if (!is.null(d$regional)) {
      for (i in d$regional) {
        dimnames(x)[[i]] <- sub("^glob$", "GLO", dimnames(x)[[i]])
      }
    }

    # make sure that temporal dimension uses dimnames of the form y0000
    if (!is.null(d$temporal)) {
      for (i in d$temporal) {
        dimnames(x)[[i]] <- sub("^[a-z]?([0-9]{4})$", "y\\1", dimnames(x)[[i]])
      }
    }

    # make sure that spatial dimension uses dimnames of the form XXX.123
    if (!is.null(d$regiospatial)) {
      for (i in d$regiospatial) {
        ntmp <- names(dimnames(x))[1]
        if (!is.null(ntmp) && !is.na(ntmp) && names(dimnames(x))[1] == "j") names(dimnames(x))[1] <- "i.j"
      }
    }


    # If no temporal dimension is defined, but a dimension of type nothing exists,
    # use this dimension as temporal dimension
    if (is.null(d$temporal)) {
      if (length(d$nothing) > 0) {
        d$temporal <- d$nothing[1]
        d$nothing <- d$nothing[-1]
        if (length(d$nothing) == 0) d$nothing <- NULL
      } else {
        d$temporal <- 0
      }
    }

    # try to create regiospatial dimension if possible
    if (is.null(d[["regiospatial"]])) {
      # regional dimension exists
      if (!is.null(d$regional)) {
        d$regiospatial <- d$regional
      } else {
        d$regiospatial <- 0
      }
    }
    d$regional <- NULL

    # Starting from here d$temporal and d$regiospatial should be defined both
    # If any of these two could neither be found nor created the value should be 0

    if (d$regiospatial == 0) {
      if (is.null(dimnames(x))) {
        x <- array(x, c(dim(x), 1))
        dimnames(x)[[length(dim(x))]] <- list("GLO")
      } else {
        x <- array(x, c(dim(x), 1), c(dimnames(x), "GLO"))
      }
      d$regiospatial <- length(dim(x))
    }

    if (d$temporal == 0) {
      x <- array(x, c(dim(x), 1), c(dimnames(x), NULL))
      d$temporal <- length(dim(x))
    }

    # Check if third dimension exists. If not, create it
    if (length(dim(x)) == 2) {
      x <- array(x, c(dim(x), 1), c(dimnames(x), NULL))
    }

    # Now temporal and regiospatial dimension should both exist
    # Return MAgPIE object
    out <- copy.attributes(storeAttributes, new("magpie", wrap(x, list(d$regiospatial, d$temporal, NA))))
    return(out)
  }
)

setMethod("as.magpie",
  signature(x = "numeric"),
  function(x, unit = "unknown", ...) {
    return(copy.attributes(x, as.magpie(as.array(x), ...)))
  }
)

setMethod("as.magpie",
          signature(x = "logical"),
          function(x, unit = "unknown", ...) {
            return(copy.attributes(x, as.magpie(as.array(x), ...)))
          }
)

setMethod("as.magpie",
  signature(x = "NULL"),
  function(x) {
    return(NULL)
  }
)

setMethod("as.magpie",
  signature(x = "data.frame"),
  function(x, datacol = NULL, tidy = FALSE, sep = ".", replacement = "_", unit = "unknown", filter = TRUE, ...) {
    # filter illegal characters
    if (isTRUE(filter)) {
      x <- tmpfilter(x, sep = sep, replacement = replacement)
    }

    if (tidy) return(tidy2magpie(x, ...))
    if (dim(x)[1] == 0) return(copy.attributes(x, new.magpie(NULL)))
    if (is.null(datacol)) {
      isNumericlike <- function(x) {
        .tmp <- function(x) return(all(!is.na(suppressWarnings(as.numeric(x[!is.na(x)])))))
        if (.isFALSE(.tmp(x[1]))) return(FALSE)
        return(.tmp(x))
      }
      for (i in rev(seq_len(dim(x)[2]))) {
        if (!is.factor(x[[i]]) && isNumericlike(x[[i]]) && !is.temporal(x[[i]])) {
          datacol <- i
        } else {
          break
        }
      }
    }
    if (!is.null(datacol)) {
      if (is.character(datacol)) {
        datacol <- which(colnames(x) == datacol)
        if (length(datacol) != 1) {
          stop("datacol does not clearly match one column name.")
        }
      }
      if (datacol == 1) return(copy.attributes(x, as.magpie(as.matrix(x), ...)))
      if (datacol == dim(x)[2]) return(tidy2magpie(x, ...))
      x[[datacol - 1]] <- as.factor(x[[datacol - 1]])
    }

    helper <- suppressWarnings(data.table::setDF(data.table::melt(data.table::as.data.table(x))))
    # The warning that is suppressed above is:
    #  In melt.data.table(data.table::as.data.table(x)) :
    #   id.vars and measure.vars are internally guessed when both are 'NULL'. All non-numeric/integer/logical type
    #   columns are considered id.vars, which in this case are columns {...}. Consider providing at least one of
    #   'id' or 'measure' vars in future.

    out <- copy.attributes(x, tidy2magpie(suppressMessages((helper)), ...))
    return(out)
  }
)

setMethod("as.magpie",
  signature(x = "quitte"),
  function(x, sep = ".", replacement = "_", filter = TRUE, ...) {
    isQuitte <- function(x) {
      # object is formally defined as quitte but it has to
      # be checked whether it follows all structural
      # rules of a quitte object

      mandatoryColumns <- c("model", "scenario", "region", "variable", "unit", "period", "value")
      factorColumns    <- c("model", "scenario", "region", "variable", "unit")
      isQuitte <- all(mandatoryColumns %in% names(x)) && all(vapply(x[factorColumns], is.factor, logical(1))) &&
                  is.numeric(x$value) && (methods::is(x$period, "POSIXct") || is.integer(x$period))
      return(isQuitte)
    }

    if (!isQuitte(x)) {
      warning("Input does not follow the full quitte class definition! Fallback to data.frame conversion.")
      class(x) <- "data.frame"
      return(as.magpie(x, ...))
    }
    x$period <- format(x$period, format = "y%Y")
    # filter illegal characters

    if (isTRUE(filter)) {
      x <- tmpfilter(x, sep = sep, replacement = replacement)
    }

    if (length(grep("^cell$", names(x), ignore.case = TRUE)) > 0) {
      i <- grep("^cell$", names(x), ignore.case = TRUE, value = TRUE)
      x$region <- paste(x$region, x[[i]], sep = ".")
      x <- x[names(x) != i]
    }
    # remove NA columns and <NA> columns that have been replaced by forcats::fct_explicit_na()
    naString <- "(Missing)"
    x <- x[colSums(!(naString == x | is.na(x))) != 0]

    # put value column as last column
    x <- x[c(which(names(x) != "value"), which(names(x) == "value"))]
    out <- tidy2magpie(x, spatial = "region", temporal = "period")
    return(out)
  }
)

setMethod("as.magpie",
  signature(x = "tbl_df"),
  function(x, unit = "unknown", ...) {
    if ("quitte" %in% class(x)) {
      class(x) <- c("quitte", "data.frame")
      out <- as.magpie(x, ...)
      return(out)
    } else {
      class(x) <- "data.frame"
      out <- as.magpie(x, ...)
      return(out)
    }
  }
)


.raster2magpie <- function(x, unit = "unknown", temporal = NULL, spatial = NULL) {
  geometry <- NULL
  idVars <- NULL
  crs <- NULL
  if (inherits(x, "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) stop("The package \"terra\" is required for raster conversions!")
    df <- as.data.frame(x, na.rm = TRUE, xy = TRUE)
    df$x <- sub(".", "p", df$x, fixed = TRUE)
    df$y <- sub(".", "p", df$y, fixed = TRUE)
    idVars <- c("x", "y")
    crs <- terra::crs(x)
  } else if (inherits(x, "SpatVector")) {
    if (!requireNamespace("terra", quietly = TRUE)) stop("The package \"terra\" is required for raster conversions!")
    df <- as.data.frame(x, geom = "WKT")
    geometry <- df$geometry
    df$geometry <- NULL
    crs <- terra::crs(x)
  } else {
    if (!requireNamespace("raster", quietly = TRUE)) stop("The package \"raster\" is required for raster conversions!")
    # na.rm = TRUE seems to remove all cells in which at least one layer has an NA. Hence, use na.rm = FALSE
    # and remove all cells which have NAs in ALL layers afterwards!
    df <- as.data.frame(x, na.rm = FALSE)
    df <- df[rowSums(!is.na(df)) != 0, , drop = FALSE]

    co <- raster::coordinates(x)[as.integer(rownames(df)), ]
    co <- matrix(sub(".", "p", co, fixed = TRUE), ncol = 2)
    colnames(co) <- c("x", "y")
    df <- cbind(co, df)
    idVars <- c("x", "y")
  }

  # check for additional spatial identifiers
  if (is.null(spatial)) {
    # select all layers with a leading dot in its name as spatial dimension
    spatial <- grep("^\\.", names(x), value = TRUE)
  } else {
    if (all(is.integer(spatial))) {
      spatial <- names(x)[spatial]
    }
  }
  names(df) <- sub("^\\.", "", names(df))
  spatial  <- sub("^\\.", "", spatial)

  idVars <- c(spatial, idVars)

  if (length(idVars) == 0) {
    df <- cbind(id = df[[1]], df)
    idVars <- "id"
  }

  df <- as.data.table(df)
  df <- melt(df, id.vars = idVars)
  variable <- as.data.table(tstrsplit(df$variable, "..", fixed = TRUE))
  if (!is.null(temporal)) temporal <- temporal + length(idVars)
  if (ncol(variable) == 1) {
    if (all(grepl("^[yX][0-9]*$", variable[[1]], perl = TRUE))) {
      variable[[1]] <- sub("^X", "y", variable[[1]], perl = TRUE)
      names(variable) <- "year"
      if (is.null(temporal)) temporal <- length(idVars) + 1
    } else {
      names(variable) <- "data"
    }
  } else if (ncol(variable) == 2) {
    names(variable) <- c("year", "data")
    if (is.null(temporal)) temporal <- length(idVars) + 1
  } else {
    stop("Reserved dimension separator \"..\" occurred more than once in layer names! Cannot convert raster object")
  }
  df <- as.data.frame(df)
  baseDims <- which(!(colnames(df) %in% c("variable", "value")))
  df <- cbind(df[, baseDims, drop = FALSE], variable, df[, "value", drop = FALSE])
  out <- tidy2magpie(df, spatial = idVars, temporal = temporal)
  if (!is.null(geometry)) {
    names(geometry) <- getItems(out, dim = 1)
    attr(out, "geometry") <- geometry
  }
  if (!is.null(crs)) {
    attr(out, "crs") <- crs
  }
  return(out)
}

setMethod("as.magpie",
  signature(x = "RasterBrick"),
  function(x, unit = "unknown", temporal = NULL, ...) {
    return(.raster2magpie(x, unit = unit, temporal = temporal))
  }
)

setMethod("as.magpie",
  signature(x = "RasterStack"),
  function(x, unit = "unknown", temporal = NULL, ...) {
    return(.raster2magpie(x, unit = unit, temporal = temporal))
  }
)

setMethod("as.magpie",
  signature(x = "RasterLayer"),
  function(x, unit = "unknown", temporal = NULL, ...) {
    return(.raster2magpie(x, unit = unit, temporal = temporal))
  }
)

setMethod("as.magpie",
          signature(x = "SpatRaster"),
          function(x, unit = "unknown", temporal = NULL, ...) {
            return(.raster2magpie(x, unit = unit, temporal = temporal))
          }
)

setMethod("as.magpie",
          signature(x = "SpatVector"),
          function(x, unit = "unknown", temporal = NULL, spatial = NULL, ...) {
            return(.raster2magpie(x, unit = unit, temporal = temporal, spatial = spatial))
          }
)
