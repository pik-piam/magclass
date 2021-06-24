.dimSumsFallback <- function(x, na.rm = FALSE, dims = NULL, ...) { # nolint
  dims <- as.integer(round((dims - 3) * 10 + 2))
  # fallback option if data dimension is sparse
  if (!is.magpie(x)) stop("Input is not a MAgPIE object!")
  f <- fulldim(x)
  ndim <- length(f[[1]])
  if (any(dims > ndim)) stop("Invalid dimension(s) specified")

  if (any(dims < 3)) stop("not implemented yet")

  # remove names from dimensions that should be summed up
  s <- paste0("^", paste(rep("([^\\.]*)", ndim - 2), collapse = "\\."), "$")
  d <- setdiff(1:ndim, dims) - 2
  d <- d[d > 0]
  r <- paste0("\\", d, collapse = "\\.")

  getNames(x) <- sub(s, r, getNames(x))
  getSets(x, fulldim = FALSE)[3] <- sub(s, r, getSets(x, fulldim = FALSE)[3])
  if (getSets(x, fulldim = FALSE)[3] == "") getSets(x, fulldim = FALSE)[3] <- "data1"
  un <- unique(getNames(x))

  out <- new.magpie(cells_and_regions = getCells(x), years = getYears(x), names = un, sets = getSets(x))
  names(dimnames(out)) <- names(dimnames(x))
  x <- as.array(x)
  for (i in un) {
    j <- which(dimnames(x)[[3]] == i)
    out[, , i] <- dimSums(x[, , j, drop = FALSE], na.rm = na.rm, dim = 3, ...)
  }
  if (ndata(out) == 1) if (getNames(out) == "") getNames(out) <- NULL

  return(out)
}





#' Summation over dimensions
#'
#' This function sums over any dimension of a magpie object or an array
#'
#'
#' @param x A MAgPIE-object or an array
#' @param dim The dimensions(s) to sum over. A vector of integers or characters
#' (dimension names). If the MAgPIE object has more than 1 actual dimension
#' collected in the third real dimension, each actual dimension can be summed
#' over using the corresponding dim code (see \code{\link{dimCode}} for more
#' information)
#' @param na.rm logical. Should missing values (including NaN) be omitted from
#' the calculations?
#' @param ...  Further arguments passed to rowSums internally
#' @return \item{value}{A MAgPIE object or an array (depending on the format of
#' x) with values summed over the specified dimensions}
#' @author Markus Bonsch, Ina Neher, Benjamin Bodirsky, Jan Philipp Dietrich
#' @seealso \code{\link{rowSums}}, \code{\link{dimSums}}, \code{\link{dimCode}}
#' @examples
#'
#' test <- as.magpie(array(1:4, dim = c(2, 2)))
#' dimSums(test, dim = c(1, 3))
#' dimSums(test[, , 1], na.rm = TRUE, dim = c(1, 2))
#' @export dimSums
dimSums <- function(x, dim = 3, na.rm = FALSE, ...) { # nolint
  if (is.magpie(x)) {
    dim <- dimCode(dim, x)
    if (prod(fulldim(x)[[1]]) != prod(dim(x))) {
      if (any(dim > 3)) {
        tmp <- .dimSumsFallback(x, na.rm = na.rm, dims = dim[dim > 3], ...)
      } else {
        tmp <- x
      }
      dim <- dim[dim <= 3]
      if (length(dim) == 0) {
        return(tmp)
      } else {
        tmp <- as.array(tmp)
      }
    } else {
      if (all(dim != 3)) {
        tmp <- unwrap(x)
        dim[dim > 3] <- as.integer(round((dim[dim > 3] - 3) * 10 + 2))
      } else {
        tmp <- as.array(x)
        dim <- dim[dim <= 3]
      }
    }
  } else if (is.array(x)) {
    tmp <- x
  } else {
    stop("Input is neither an array nor a MAgPIE object!")
  }

  if (any(dim == 0) || any(dim > length(dim(tmp))))
    stop("Invalid dimension(s) specified")
  unchangedDims <- which(!seq_along(dim(tmp)) %in% dim)
  out <- aperm(tmp, perm = c(unchangedDims, dim))
  out <- rowSums(out, na.rm = na.rm, dims = length(unchangedDims),
    ...)
  remainingDims <- match(seq_along(dim(tmp)), unchangedDims,
    nomatch = 0)
  remainingDims <- remainingDims[remainingDims > 0]

  if (is.magpie(x)) {
    spatial <- ifelse(1 %in% dim, 0, 1)
    temporal <- ifelse(2 %in% dim, 0, ifelse(1 %in% dim, 1, 2))
    out <- as.magpie(aperm(as.array(out), perm = remainingDims), spatial = spatial, temporal = temporal)
    if (1 %in% dim && nregions(x) == 1)
      dimnames(out)[[1]] <- getRegions(x)
    if (2 %in% dim && nyears(x) == 1)
      dimnames(out)[[2]] <- getYears(x)
    out <- clean_magpie(out)
  } else {
    out <- aperm(as.array(out), perm = remainingDims)
  }
  return(out)
}
