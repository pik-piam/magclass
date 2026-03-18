#' condReplace
#'
#' This function allows to replace parts of a magpie object based on a condition.
#'
#' @param x MAgPIE object to be modified
#' @param condition Either a function that returns TRUE/FALSE or a magpie object
#' with TRUE/FALSE or 1/0 content. Can have lower dimensionality than x.
#' @param replace MAgPIE object that shall replace the content of x if condition
#' is TRUE. Can have lower dimensionality than x.
#'
#' @return The modified MAgPIE object
#' @author Benjamin Bodirsky
#' @examples
#' replaceSingle <- condReplace(population_magpie, population_magpie > 1000, 1000)
#' replaceRow <- condReplace(population_magpie, dimSums(population_magpie, dim = 1) < 7000, NA)
#' containsNAs <- replaceRow
#' replaceBasedOnFunction <- condReplace(containsNAs, is.na, 0)
#' replaceUsingMean <- condReplace(containsNAs, is.na,
#'                                 magpply(X = containsNAs, FUN = mean, DIM = 2, na.rm = TRUE))
#'
#' @family SelectionCalculation
#' @export



condReplace <- function(x, condition, replace) {

  if (is.function(condition)) {
    replace <- magpie_expand(x = as.magpie(replace), ref = x)
    x[condition(x)] <- replace[condition(x)]
  } else {
    replace <- magpie_expand(x = as.magpie(replace), ref = x)
    condition <- magpie_expand(x = as.magpie(condition), ref = x)
    x[as.logical(condition)] <- replace[as.logical(condition)]
  }
  return(x)
}
