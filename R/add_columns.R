#' add_columns
#'
#' Function adds new columns to the existing magpie object. The new columns are
#' filled with NAs.
#'
#'
#' @param x MAgPIE object which should be extended.
#' @param dim The number of the dimension that should be extended
#' @param addnm The new columns within dimension "dim"
#' @return The extended MAgPIE object
#' @author Benjamin Bodirsky
#' @seealso \code{\link{add_dimension}},\code{\link{mbind}}
#' @examples
#'
#' pop <- maxample("pop")
#' a <- add_columns(pop)
#' str(a)
#' getItems(a, split = TRUE)
#' @export add_columns
add_columns <- function(x, addnm = c("new"), dim = 3.1) { # nolint
  if (length(addnm) == 0) return(x)
  oldDim <- old_dim_convention(dim)
  if (oldDim == 1) {
    newColumns <- x[rep(1, length(addnm)), , ]
    newColumns <- setCells(newColumns, paste(substr(addnm, 1, 3), ".",
                                             (oldDim(x)[oldDim] + 1):(oldDim(x)[oldDim] + length(addnm)), sep = ""))
    newColumns[, , ] <- NA
  } else if (oldDim == 2) {
    newColumns <- x[, rep(1, length(addnm)), ]
    newColumns <- setYears(newColumns, addnm)
    newColumns[, , ] <- NA
  } else if (oldDim > 2) {
    newColumns <- x[, , getItems(x, dim = dim)[1]]
    getNames(newColumns, dim = oldDim - 2) <- addnm[1]
    if (length(addnm) > 1) {
      singleColumnX <- newColumns
      for (i in 2:length(addnm)) {
        getNames(singleColumnX, dim = oldDim - 2) <- addnm[i]
        newColumns <- mbind(newColumns, as.magpie(singleColumnX))
      }
    }
    newColumns[, , ] <- NA
  }
  output <- mbind(x, newColumns)
  return(output)
}
