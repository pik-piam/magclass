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
#' @examples
#'
#' # create data with 5 identical scenarios
#' p <- add_dimension(maxample("pop"), nm = paste0("scen", 1:5))
#' p
#' dimReduce(p)
#'
#' # set years to same value
#' p[, , ] <- setYears(p[, 1, ], NULL)
#' p
#' dimReduce(p)
#'
#' # set regions to same value
#' p[, , ] <- setCells(p[1, , ], "GLO")
#' p
#' dimReduce(p)
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
