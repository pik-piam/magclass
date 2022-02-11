#' dimReduce
#'
#' Remove dimensions which contain identical data for all elements in it
#'
#'
#' @param x MAgPIE object which should be reduced
#' @param dim_exclude Vector with names of dimensions which must not be reduced
#' @return The reduced MAgPIE object
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{add_dimension}}
#' @note This function has some similarities to \code{\link{collapseDim}}, but
#' serves a different purpose. While \code{\link{collapseDim}} only removes
#' dimensions which contain only a single element or which it is
#' specifically told to remove, \code{\link{dimReduce}} looks whether the
#' entries of a multi-entry dimension are all the same and removes dimensions
#' for which this is the case. In some cases both will lead to the same result
#' but in many other cases the results will differ.
#' @examples
#'
#' # create data with 5 identical scenarios
#' p <- add_dimension(maxample("pop")[1:3, 1:3, ], nm = paste0("scen", 1:2))
#' str(p)
#' str(dimReduce(p))
#'
#' # set years to same value
#' p[, , ] <- setYears(p[, 1, ], NULL)
#' str(p)
#' str(dimReduce(p))
#'
#' # set regions to same value
#' p[, , ] <- setCells(p[1, , ], "GLO")
#' str(p)
#' str(dimReduce(p))
#' @export

dimReduce <- function(x, dim_exclude = NULL) { # nolint
  x <- collapseDim(x)
  x <- clean_magpie(x)
  fd <- unlist(getItems(x, split = TRUE), recursive = FALSE)
  for (s in setdiff(getSets(x), dim_exclude)) {
    tmp <- list()
    tmp[[s]] <- fd[[s]][1]
    xSingle <- mselect(x, tmp)
    # same information in all dimension entries?
    if (all(x - xSingle  == 0, na.rm = TRUE)) {
      x <- xSingle
    }
  }
  x <- collapseDim(x)
  if (dim(x)[1] == 1) getCells(x) <- "GLO"
  return(x)
}
