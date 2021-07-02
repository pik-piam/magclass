#' Get dataset names
#'
#' Extracts dataset names of a MAgPIE-object
#'
#' setNames is a shortcut to use a MAgPIE object with manipulated data names.
#' The setNames method uses the variable names "object" and "nm" in order to be
#' consistent to the already existing function setNames.
#'
#' @aliases getNames getNames<-
#' @param x MAgPIE object
#' @param fulldim specifies, how the object is treated. In case of FALSE, it is
#' assumed that x is 3 dimensional and dimnames(x)[[3]] is returned. In case of
#' TRUE, the dimnames of the real third dimension namesare returned
#' @param dim Argument to choose a specific data dimension either by name of
#' the dimension or by number of the data dimension.
#' @param value a vector of names current names should be replaced with. If
#' only one data element exists you can also set the name to NULL.
#' @return getNames returns data names of the MAgPIE-object, whereas setNames
#' returns the MAgPIE object with the manipulated data names.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{setNames-methods}}, \code{\link{getRegions}}, \code{\link{getYears}},
#' \code{\link{getCPR}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}},\code{\link{ndata}},
#' \code{"\linkS4class{magpie}"}
#' @examples
#'
#' a <- as.magpie(1)
#' getNames(a)
#' setNames(a, "bla")
#'
#' x <- new.magpie("GLO", 2000, c("a.o1", "b.o1", "a.o2"))
#' getNames(x, dim = 2)
#'
#' getSets(x, fulldim = FALSE)[3] <- "bla.blub"
#' getNames(x, dim = "bla")
#'
#' getSets(x)[4] <- "ble"
#' getNames(x, dim = "ble") <- c("Hi", "Bye")
#' x
#' @export
getNames <- function(x, fulldim = FALSE, dim = NULL) {
  return(getItems(x, dim = .convertDim(dim), split = fulldim)) #nolint
}

#' @describeIn getNames set names
#' @export
"getNames<-" <- function(x, dim = NULL, value) { # nolint
  if (is.null(dim) && is.null(value) && dim(x)[3] > 1) stop("Cannot unset names!")
  return(setItems(x, dim = .convertDim(dim), value = value, raw = TRUE))
}

.convertDim <- function(dim) {
  if (is.null(dim)) {
    dim <- 3
  } else if (is.numeric(dim)) {
    if (length(dim) != 1 || !is.element(dim, 1:9)) stop("Unsupported dim selection!")
    dim <- 3 + dim / 10
  }
  return(dim)
}
