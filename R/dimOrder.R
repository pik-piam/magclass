#' @title dimOrder
#' @description Changes the order of the sub-dimension in a magpie object
#' similar to unwrapping and applying the aperm command, but more efficient.
#'
#' @param x magpie object
#' @param perm vector with the new order of the sub-dimension. Missing
#' sub-dimensions will added automatically at the end
#' @param dim main dimension in which the order of sub-dimensions should
#' be changed (1, 2 or 3)
#'
#' @return magpie object
#' @author Jan Philipp Dietrich, Benjamin Leon Bodirsky
#' @examples
#' a <- maxample("animal")
#' head(a)
#' head(dimOrder(a, perm = 3:1, dim = 1))
#' head(dimOrder(a, perm = c(2,1,3), dim = 3))
#' @export

dimOrder <- function(x, perm, dim = 3) {
  ndim <- ndim(x, dim)
  if (any(!(perm %in% 1:ndim))) {
    stop("Values of perm must be integers in the range from 1 to ", ndim)
  }
  if (ndim != length(perm)) {
    perm <- c(perm, setdiff(1:ndim, perm))
  }
  if (ndim > 1) {
    pattern <- paste0("^([^\\.]*)\\.", paste0(rep("([^\\.]*)\\.", ndim - 2), collapse = ""), "([^\\.]*)$")
    order <- paste0(paste0("\\", perm), collapse = ".")
    if (!is.null(dimnames(x)[[dim]])) {
      dimnames(x)[[dim]] <- gsub(pattern = pattern, replacement = order, dimnames(x)[[dim]], perl = TRUE)
    }
    if (!is.null(names(dimnames(x))[dim])) {
       names(dimnames(x))[dim] <- gsub(pattern = pattern, replacement = order, names(dimnames(x))[dim], perl = TRUE)
    }
  }
  return(x)
}
