extend <- function(x,
                   xRange = c(-179.75, 179.75),
                   yRange = c(89.75, -89.75),
                   res = NULL) {
  stopifnot(length(xRange) == 2, length(yRange) == 2)
  if (is.null(res)) {
    res <- guessResolution(getCoords(x))
  }
  coords <- expand.grid(x = seq(xRange[1], xRange[2], if (xRange[1] < xRange[2]) res else -res),
                        y = seq(yRange[1], yRange[2], if (yRange[1] < yRange[2]) res else -res))
  coords <- paste0(coords$x, "|", coords$y)
  coords <- gsub("\\.", "p", coords)
  coords <- sub("\\|", ".", coords)

  sparseCoords <- paste0(getItems(x, "x", full = TRUE),
                         ".",
                         getItems(x, "y", full = TRUE))
  if (!all(sparseCoords %in% coords)) {
    stop("The coordinates of the input object are not a subset of the extended coordinates. ",
         "Try changing res, xRange or yRange.")
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
