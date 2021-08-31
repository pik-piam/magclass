#' Get Items
#'
#' Extract items of a given (sub-)dimension of a MAgPIE-object
#'
#' @aliases getItems<-
#' @param x MAgPIE object
#' @param dim Dimension for which the items should be returned. Either number or
#' name of dimension or a vector of these. See \code{\link{dimCode}} for more details.
#' @param split Boolean which determines whether a main dimension should be split in subdimensions.
#' Only applicable to main dimensions (1,2,3) and ignored for all other.
#' @param full if TRUE dimension names are returned as they are (including repetitions), if FALSE only
#' the dimension elements (unique list of entries) are returned.
#' @param maindim main dimension the data should be added to (does not need to be set if \code{dim} exists
#' in the data. Should be set if \code{dim} might not exist, or if \code{dim} might potentially exist
#' in a different main dimension than the one anticipated).
#' @param raw if set to FALSE inputs will be corrected (e.g. dots replaced by the letter "p") if necessary. If
#' TRUE data will be written as is (risking the creation of inconsistent objects).
#' @param value a vector with the length of the main dimension the dimnames should be replaced in / added to.
#' If set to NULL the corresponding dimension will be removed.
#' @return items of the requested dimension in the MAgPIE-object. If split=TRUE and applied to a
#' main dimension (1,2,3) a list of items for each sub-dimension.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{dimCode}}
#' @importFrom data.table tstrsplit
#' @examples
#' x <- maxample("pop")
#' getItems(x, "scenario")
#' getItems(x, 3.1)
#' getItems(x, "i") <- paste0("REG", seq_len(ncells(x)))
#' getItems(x, "i")
#' y <- x[, 1, ]
#' getItems(y, "t") <- NULL
#' @export
getItems <- function(x, dim = NULL, split = FALSE, full = FALSE) { # nolint
  if (is.null(dim)) dim <- 1:3
  dim <- dimCode(dim, x, missing = "stop")
  if (setequal(dim, 1:3) && !split) return(dimnames(x))
  if (length(dim) > 1) {
    out <- list()
    for (i in dim) out[[as.character(i)]] <- getItems(x, dim = i, split = split, full = full)
    if (all(dim == floor(dim))) {
      names(out) <- NULL
    } else if (hasSets(x)) {
      sets <- c(d1 = "", d2 = "", d3 = "", getSets(x))
      names(out) <- sets[paste0("d", dim)]
    }
    return(out)
  }

  if (dim == floor(dim) && !split) return(dimnames(x)[[dim]])
  if (dim == floor(dim) && split) {
    if (is.null(dimnames(x)[[dim]])) {
      out <- list(NULL)
      if (hasSets(x)) {
        names(out) <- getSets(x, fulldim = FALSE)[dim]
      }
      return(out)
    }
    tmp <- tstrsplit(dimnames(x)[[dim]], ".", fixed = TRUE, names = TRUE)
    if (!isTRUE(full)) tmp <- lapply(tmp, unique)
    if (hasSets(x)) {
      tmp2 <- strsplit(getSets(x, fulldim = FALSE)[dim], ".", fixed = TRUE)[[1]]
      if (length(tmp2) == length(tmp)) names(tmp) <- tmp2
    }
    return(tmp)
  }
  if (!dimExists(dim, x)) stop("Subdimension ", dimCode(dim, x), " does not exist!")
  tmp <- dimnames(x)[[as.integer(dim)]]
  if (is.null(tmp)) return(NULL)
  subdim <- as.integer(substring(dim, 3))
  reg <- paste0(rep("([^\\.]*)", subdim), collapse = "\\.")
  out <- sub(paste0("^", reg, ".*$"), paste0("\\", subdim), tmp)
  if (isTRUE(full)) return(out)
  return(unique(out))
}

#' @describeIn getItems set dimension names
#' @export
"getItems<-" <- function(x, dim, full = NULL, maindim = NULL, raw = FALSE, value) { # nolint
  if (!is.null(full) && !isTRUE(full)) stop("Currently only full = NULL or TRUE supported!")
  if (length(dim) > 1) stop("dim with length > 1 is currently not supported when setting items.")
  dc <- dimCode(dim, x)
  if (dc == 0 && is.null(maindim)) {
    stop("Dimension does not exist in object and cannot be added as main dimension is not specified!")
  }
  if (!is.null(maindim)) {
    if (!(maindim %in% 1:3)) stop("Unsupported maindim (can only be 1, 2 or 3!)")
    if (dc == 0) {
      dc <- maindim + 0.99999
    } else {
      if (floor(dc) != maindim) stop("Specified dimension (dim) found in main dimension different to maindim!")
    }
  } else {
    maindim <- floor(dc)
  }

  if (!is.null(value)) {
    if (length(value) != dim(x)[maindim]) {
      elems <- try(getItems(x, dim = dim), silent = TRUE)
      if ("try-error" %in% class(elems) || length(elems) != length(value)) stop("Wrong number of items supplied!")
      # try to expand value input to full length of main dimension
      if (is.null(names(value))) {
        names(value) <- elems
      } else if (!setequal(names(value), elems)) {
        stop("Names of input vector do not match existing dimension names!")
      }
      elemsFull <- getItems(x, dim = dim, full = TRUE)
      value <- unname(value[elemsFull])
    }
    if (!isTRUE(raw)) {
      nv           <- names(value)
      value        <- gsub(".", "p", value, fixed = TRUE)
      names(value) <- nv
    }

    .sortvalues <- function(value, x, dim) {
      if (length(value) == 1) return(list(value))
      if (!is.null(names(value))) {
        order <- getItems(x, dim)
        if (!all(order %in% names(value))) {
          stop("Input vector is named but not all names match items of the dimension to be replaced!")
        }
        value <- value[order]
        names(value) <- NULL
      }
      return(value)
    }
    if (dc == maindim) {
      dimnames(x)[[maindim]] <- .sortvalues(value, x, maindim)
      if (!isTRUE(raw)) names(dimnames(x))[maindim] <- gsub(".", "p", names(dimnames(x))[maindim], fixed = TRUE)
    } else if (!dimExists(dc, x)) {
      if (!is.null(names(value))) {
        warning("Names of input vector are being ignored as dimension is not yet existing!")
        names(value) <- NULL
      }
      dimnames(x)[[maindim]] <- paste0(dimnames(x)[[maindim]], ".", value)
      if (!is.character(dim)) dim <- "newdim"
      names(dimnames(x))[maindim] <- paste0(names(dimnames(x))[maindim], ".", dim)
    } else {
      tmp <- getItems(x, maindim, split = TRUE, full = TRUE)
      subdim <- as.integer(substring(dc, 3))
      tmp[[subdim]] <- .sortvalues(value, x, dc)
      .paste <- function(...) return(paste(..., sep = "."))
      dimnames(x)[[maindim]] <- do.call(.paste, tmp)
    }
  } else {
    if (dc == maindim) {
      if (.isFALSE(raw) && dim(x)[maindim] > 1) {
        stop("Cannot unset dimension names for dimensions with more than 1 element!")
      }
      if (maindim == 1) dimnames(x) <- c(d1 = list(NULL), dimnames(x)[2:3])
      if (maindim == 2) dimnames(x) <- c(dimnames(x)[1], d2 = list(NULL), dimnames(x)[3])
      if (maindim == 3) dimnames(x) <- c(dimnames(x)[1:2], d3 = list(NULL))
    } else if (!dimExists(dc, x)) {
      warning("Nothing to do here as dimension does not exist!")
    } else {
      tmp <- getItems(x, maindim, split = TRUE, full = TRUE)
      subdim <- as.integer(substring(dc, 3))
      tmp[[subdim]] <- NULL
      .paste <- function(...) return(paste(..., sep = "."))
      dimnames(x)[[maindim]] <- do.call(.paste, tmp)
      subdim <- as.integer(substring(dc, 3))
      names(dimnames(x))[maindim] <- paste(strsplit(names(dimnames(x))[maindim], ".", fixed = TRUE)[[1]][-subdim],
                                           collapse = ".")
    }
  }
  return(x)
}
