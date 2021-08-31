#' MAgPIE-Clean
#'
#' Function cleans MAgPIE objects so that they follow some extended magpie
#' object rules (currently it makes sure that the dimnames have names and
#' removes cell numbers if it is purely regional data)
#'
#'
#' @param x MAgPIE object which should be cleaned.
#' @param what term defining what type of cleaning should be performed. Current
#' modes are "cells" (removes cell numbers if the data seems to be regional -
#' this should be used carefully as it might remove cell numbers in some cases
#' in which they should not be removed), "sets" (making sure that all
#' dimensions have names), "items" (replace empty elements with single spaces " ")
#' and "all" (performing all available cleaning methods)
#' @return The eventually corrected MAgPIE object
#' @author Jan Philipp Dietrich
#' @seealso \code{"\linkS4class{magpie}"}
#' @examples
#'
#' pop <- maxample("pop")
#' a <- clean_magpie(pop)
#' @export clean_magpie
clean_magpie <- function(x, what = "all") { # nolint
  availableTypes <- c("cells", "items", "sets")
  if ("all" %in% what) what <- availableTypes
  if (any(!is.element(what, availableTypes))) stop('Unknown setting for argument what ("', what, '")!')

  # remove cell numbers if data is actually regional
  if (is.element("cells", what) && ncells(x) == nregions(x)) {
      items <- getItems(x, dim = 1.1, full = TRUE)
      names(items) <- NULL
      getItems(x, dim = 1) <- items
  }
  # make sure that all dimensions have names
  if ("sets" %in% what) {

    if (is.null(names(dimnames(x)))) names(dimnames(x)) <- rep(NA, 3)

    .countSubdim <- function(x, sep = "\\.") {
      o <- nchar(gsub(paste0("[^", sep, "]*"), "", x)) + 1
      if (length(o) == 0) o <- 0
      return(o)
    }

    .fixNames <- function(names, ndim, key = "data") {
      if (is.na(names) || names == "" || names == "NA") {
        tmp <- rep(key, max(1, ndim))
        names <- paste(make.unique(tmp, sep = ""), collapse = ".")
      } else {
        cdim <- .countSubdim(names)
        if (ndim != cdim) {
          if (ndim > cdim) {
            names <- paste(c(names, rep(key, ndim - cdim)), collapse = ".")
          } else {
            search <- paste0(c(rep("\\.[^\\.]*", cdim - ndim), "$"), collapse = "")
            names <- sub(search, "", names)
          }
          names <- paste0(make.unique(strsplit(names, "\\.")[[1]], sep = ""), collapse = ".")
        }
      }
      return(names)
    }

    names <- names(dimnames(x))
    keys <- c("region", "year", "data")
    for (i in 1:3) {
      names[i] <- .fixNames(names[i], ndim = .countSubdim(dimnames(x)[[i]][1]), key = keys[i])
    }
    names(dimnames(x)) <- names
  }

  if ("items" %in% what) {
    .fixEmptySubims <- function(x, dim) {
      if (is.null(dimnames(x)[[dim]])) return(x)
      pattern <- "(^|\\.)(\\.|$)"
      while (any(grepl(pattern, dimnames(x)[[dim]]))) {
        dimnames(x)[[dim]] <- gsub(pattern, "\\1 \\2", dimnames(x)[[dim]], perl = TRUE)
      }
      return(x)
    }
    for (i in 1:3) {
      x <- .fixEmptySubims(x, dim = i)
    }
  }

  return(x)
}
