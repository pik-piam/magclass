#' extend
#'
#' Extend a magpie object to a dense grid based on the given xRange, yRange and resolution.
#' This is e.g. required when writing netCDF files. Extending a sparse magpie object to a dense grid
#' requires much more memory, so use with caution.
#'
#' @param x A magpie object
#' @param gridDefinition A vector of 5 numeric values: c(xMin, xMax, yMin, yMax, resolution).
#' Use c(-179.75, 179.75, -89.75, 89.75, 0.5) to extend to a standard 0.5-degree-resolution
#' lon/lat grid. If NULL, use min/max of coordinates in x and guessResolution.
#' @param crop If TRUE, discard cells from x which are not in the gridDefinition grid. If FALSE,
#' throw an error if the coordinates of x are not a subset of the extended coordinates.
#' @return Magpie object x with dense grid according to gridDefinition, gaps filled with NA.
#' @author Pascal Sauer
#' @export
extend <- function(x, gridDefinition = NULL, crop = FALSE) {
  if (is.null(gridDefinition)) {
    coords <- getCoords(x)
    firstX <- min(coords$x)
    lastX <- max(coords$x)
    firstY <- max(coords$y)
    lastY <- min(coords$y)
    res <- guessResolution(coords)
  } else {
    stopifnot(length(gridDefinition) == 5)
    firstX <- gridDefinition[1]
    lastX <- gridDefinition[2]
    firstY <- gridDefinition[3]
    lastY <- gridDefinition[4]
    res <- gridDefinition[5]
  }

  coords <- expand.grid(x = seq(firstX, lastX, if (firstX < lastX) res else -res),
                        y = seq(firstY, lastY, if (firstY < lastY) res else -res))
  coords <- paste0(coords$x, "|", coords$y)
  coords <- gsub("\\.", "p", coords)
  coords <- sub("\\|", ".", coords)

  sparseCoords <- paste0(getItems(x, "x", full = TRUE),
                         ".",
                         getItems(x, "y", full = TRUE))
  if (crop) {
    x <- x[sparseCoords %in% coords, , ]
    if (length(x) > 0) {
      sparseCoords <- paste0(getItems(x, "x", full = TRUE),
                             ".",
                             getItems(x, "y", full = TRUE))
    } else {
      sparseCoords <- character(0)
    }
  } else {
    if (!all(sparseCoords %in% coords)) {
      stop("The coordinates of the input object are not a subset of the extended coordinates. ",
           "Try changing res, xRange or yRange.")
    }
  }

  subdims1 <- getSets(x)[grep("^d1\\.", names(getSets(x)))]
  notCoords <- subdims1[!subdims1 %in% c("x", "y")]

  dim1 <- paste0(coords, paste(rep(".NA", length(notCoords)), collapse = ""))

  extended <- new.magpie(dim1, getItems(x, 2), getItems(x, 3))

  # dimOrder subdims x and y to where they are in x
  perm <- match(getSets(x), c("x", "y"))[seq_along(subdims1)]
  perm[is.na(perm)] <- seq_along(notCoords) + 2
  extended <- dimOrder(extended, perm, dim = 1)

  getSets(extended) <- getSets(x)

  posInCoords <- match(sparseCoords, coords)

  # fill subdims for dim 1 other than x and y (e.g. country code) if they were set in x
  dimnames(extended)[[1]][posInCoords] <- getItems(x, 1)

  # fill extended with data from x where available
  extended[posInCoords, , ] <- x

  return(extended)
}
