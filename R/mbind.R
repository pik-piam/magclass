#' mbind
#'
#' Merges MAgPIE-objects with identical structure in two dimensions. If data
#' differs in the temporal or spatial dimension each year or region/cell must
#' appear only once!
#'
#' @param ... MAgPIE objects or a list of MAgPIE objects that should be merged.
#' @return The merged MAgPIE object
#' @author Jan Philipp Dietrich, Misko Stevanovic
#' @seealso \code{"\linkS4class{magpie}"}
#' @examples
#'
#' m <- new.magpie(c("AFR", "CPA", "EUR"), c(1995, 2005), "Data1", fill = c(1, 2, 3, 4, 5, 6))
#' ms <- dimSums(m, dim = 3.1)
#' mbind(m, ms)
#' my <- new.magpie(getRegions(m), 2010, getNames(m), fill = c(6, 6, 4))
#' mbind(m, my)
#' md <- new.magpie(getRegions(m), getYears(m), "Data2", fill = c(7, 6, 5, 7, 8, 9))
#' mbind(m, md)
#'
#' pop <- maxample("pop")
#' a <- mbind(pop, pop)
#' dim(pop)
#' dim(a)
#' @importFrom methods new
#' @importFrom abind abind
#' @export

mbind <- function(...) { #nolint
  inputs <- list(...)
  if (length(inputs) == 1 && is.list(inputs[[1]])) inputs <- inputs[[1]]
  # Remove NULL elements from list
  for (i in rev(seq_along(inputs))) {
    if (is.null(inputs[[i]])) {
      inputs[[i]] <- NULL
    } else if (prod(dim(inputs[[i]])) == 0) {
      inputs[[i]] <- NULL
      warning("You are trying to mbind an empty magclass object. Is that really intended?")
    }
  }

  # if all inputs are NULL, return NULL
  if (0 == length(inputs))
    return(NULL)

  # store total number of elements to ensure that they remain unchanged
  nElems <- sum(vapply(inputs, length, integer(1)))

  cells <- NULL
  elems <- NULL
  years <- NULL
  diffspat <- FALSE
  difftemp <- FALSE
  diffdata <- FALSE
  for (i in seq_along(inputs)) {
    if (!is.magpie(inputs[[i]])) stop("Inputs must all be MAgPIE-objects")
    for (j in 1:3) {
      if (is.null(dimnames(inputs[[i]])[[j]])) {
        dimnames(inputs[[i]])[[j]] <- paste("dummy", c("", seq_len(dim(inputs[[i]])[j] - 1)), sep = "")
      }
    }
    # Check which dimensions differ
    if (suppressWarnings(any(sort(dimnames(inputs[[1]])[[1]]) != sort(dimnames(inputs[[i]])[[1]])))) diffspat <- TRUE
    if (suppressWarnings(any(sort(dimnames(inputs[[1]])[[2]]) != sort(dimnames(inputs[[i]])[[2]])))) difftemp <- TRUE
    if (suppressWarnings(any(sort(dimnames(inputs[[1]])[[3]]) != sort(dimnames(inputs[[i]])[[3]])))) diffdata <- TRUE
    years <- c(years, getYears(inputs[[i]]))
    elems <- c(elems, getNames(inputs[[i]]))
    cells <- c(cells, getCells(inputs[[i]]))
    if (!diffspat && ncells(inputs[[1]]) > 1) inputs[[i]] <- inputs[[i]][getCells(inputs[[1]]), , ]
    if (!difftemp && nyears(inputs[[1]]) > 1) inputs[[i]] <- inputs[[i]][, getYears(inputs[[1]]), ]
    if (!diffdata &&  ndata(inputs[[1]]) > 1) inputs[[i]] <- inputs[[i]][, , getNames(inputs[[1]])]
  }

  if (!(length(grep(".", cells, fixed = TRUE)) %in% c(0, length(cells)))) {
    stop("Mixture of regional (no cell numbers) and cellular (with cell numbers)",
         " data objects! Cannot handle this case!")
  }

  if (diffspat && difftemp) stop("Cannot handle objects! Spatial as well as temporal dimensions differ!")
  if (difftemp && diffdata) stop("Cannot handle objects! Data as well as temporal dimensions differ!")
  if (diffdata && diffspat) stop("Cannot handle objects! Data as well as spatial dimensions differ!")
  if (difftemp) {
    if (length(years) != length(unique(years))) stop("Some years occur more than once!",
                                                     " Cannot handle this case!")
    output <- new("magpie", abind::abind(inputs, along = 2))
  } else if (diffspat) {
    if (length(cells) != length(unique(cells))) stop("Some regions/cells occur more than once!",
                                                     " Cannot handle this case!")
    output <- new("magpie", abind::abind(inputs, along = 1))
  } else {
    tmp <- function(x) return(length(getNames(x, fulldim = TRUE)))
    tmp <- sapply(inputs, tmp) #nolint
    if (length(unique(tmp)) > 1) warning("mbind most likely returned an erronous magpie object due to",
                                         " different numbers of data subdimensions in inputs!")
    output <- new("magpie", abind::abind(inputs, along = 3))
  }
  for (j in 1:3) {
    if (length(grep("^dummy[0-9]*$", getItems(output, dim = j), perl = TRUE)) == dim(output)[j]) {
      getItems(output, dim = j, raw = TRUE) <- NULL
    }
  }
  names(dimnames(output)) <- names(dimnames(inputs[[1]]))

  if (length(output) != nElems) {
    stop("Invalid object (number of values changed during mbind). Does the data contain duplicates?")
  }
  return(output)
}
