#' Reconstructs full dimensionality of MAgPIE objects
#'
#' If a MAgPIE object is created from a source with more than one data
#' dimension, these data dimensions are combined to a single dimension. fulldim
#' reconstructs the original dimensionality and reports it.
#'
#'
#' @param x A MAgPIE-object
#' @param sep A character separating joined dimension names
#' @return A list containing in the first element the dim output and in the
#' second element the dimnames output of the reconstructed array.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{as.magpie}},\code{\link{unwrap}},\code{\link{wrap}}
#' @export
fulldim <- function(x, sep = ".") { #nolint
  .Deprecated("getItems")
  if (!is.null(dimnames(x)[[3]])) {
    elemsplit <- strsplit(dimnames(x)[[3]], sep, fixed = TRUE)
    tmp <- sapply(elemsplit, length) #nolint
  } else {
    tmp <- 0
  }
  if (length(tmp) == 1 && tmp == 0) tmp <- 1
  if (tmp[1] == 1) {
    # no need for splitting
    return(list(dim(x), dimnames(x)))
  } else if (any(tmp != rep(tmp[1], length(tmp)))) {
    # data dimension cannot be splitted return dim(x)
    warning("Data dimension cannot be splitted due to inconsistent number of subdimensions!")
    return(list(dim(x), dimnames(x)))
  } else {
    nElemDims <- tmp[1]
    tmp <- t(matrix(unlist(elemsplit), nElemDims))
    dimnames <- list()
    dimnames[[1]] <- dimnames(x)[[1]]
    dimnames[[2]] <- dimnames(x)[[2]]
    dim <- dim(x)[1:2]
    for (i in seq_len(nElemDims)) {
      dimnames[[i + 2]] <- unique(tmp[, i])
      dim <- c(dim, length(dimnames[[i + 2]]))
    }
    tmp <- getSets(x, sep = sep)
    if (length(tmp) == length(dimnames) || is.null(tmp)) {
      names(dimnames) <- tmp
    } else if (length(tmp) == length(dimnames) + 1 && length(grep(sep, names(dimnames(x))[1], fixed = TRUE)) > 0) {
        names(dimnames) <- c(names(dimnames(x))[1], tmp[3:length(tmp)])
    }
    return(list(dim, dimnames))
  }
}
