#' @title magpply
#' @description apply command for magpieobjects. Very efficient for replacing loops.
#'
#' @param X magpie object
#' @param FUN function that shall be applied X
#' @param MARGIN dimension over which FUN shall be applied (like a loop over that dimension).
#' This dimension will be preserved in the output object (see also \code{DIM}).
#' @param DIM dimension in which FUN shall be applied. This dimension will be missing in the output. DIM and MARGIN
#' are opposite ways of expressing the dimensions to be addressed and you must only use one of them with MARGIN
#' excluding dimensions from the calculation and DIM including them.
#' @param ... further parameters passed on to FUN
#' @param INTEGRATE if TRUE, the output will be filled into an magpie object of the same dimensionality as X
#'
#' @return magpie object
#' @author Jan Philipp Dietrich, Benjamin Leon Bodirsky
#' @examples
#'
#' pop <- maxample("pop")
#' magpply(pop, FUN = sum, MARGIN = 2)
#' fourdim <- pop * setNames(pop, c("jkk", "lk"))
#' magpply(fourdim, FUN = sum, MARGIN = c(1, 3.1))
#' @export magpply

magpply <- function(X, FUN, MARGIN = NULL, DIM = NULL, ..., INTEGRATE = FALSE) { # nolint
  if (!is.magpie(X)) stop("Input is not a MAgPIE object!")
  if (!is.null(MARGIN) && !is.null(DIM)) stop("Please specify either MARGIN or DIM, not both at the same time!")
  if (!is.null(MARGIN)) {
    # converting MARGIN to DIM
    MARGIN <- dimCode(MARGIN, X) #nolint
    DIM <- NULL #nolint
    for (i in 1:3) {
      if (!is.element(i, floor(MARGIN))) {
        DIM <- c(DIM, i) #nolint
      } else if (is.element(i, floor(MARGIN)) && !is.element(i, MARGIN)) {
        DIM <- c(DIM, setdiff(i + seq_len(ndim(X, dim = i)) / 10, MARGIN)) #nolint
      }
    }
  }
  dim <- sort(dimCode(DIM, X), decreasing = TRUE)
  if (any(dim == 0)) stop("Invalid dimension(s) specified")
  if (length(X) == 0) return(NULL)
  if (INTEGRATE) xIn <- X
  for (d in dim) getItems(X, dim = d, raw = TRUE) <- NULL
  noNames <- which(sapply(dimnames(X), is.null)) # nolint
  for (i in noNames) {
    getItems(X, dim = i) <- rep("dummy", dim(X)[i])
  }
  xd <- as.data.frame.table(X)
  out <- new("magpie", tapply(xd[[4]], xd[1:3], FUN, ...))
  for (i in noNames) {
    getItems(out, dim = i) <- NULL
  }
  if (INTEGRATE) out <- magpie_expand(out, xIn)
  return(out)
}
