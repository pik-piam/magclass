#' joins a data.frame or vector of strings with variable and unit separated into
#' a data.frame with variable and unit joined as 'variable (unit)'.
#' Use magclass::unitsplit to split them again
#'
#' @param x data.frame or vector of strings
#' @param col column name. Default: variable
#' @param unit vector of strings. If NULL, col 'unit' in x is used
#' @return data.frame or vector of strings, dependent on x
#'
#' @export
unitjoin <- function(x, unit = NULL, col = "variable") {
  # check whether is a data.frame
  if (is.null(ncol(x))) {
    if (length(x) == 0 || is.null(unit)) {
      # return empty variable vector or if no unit specified
      return(x)
    } else {
      #  paste unit to it, keeping factor type
      if (is.factor(x)) {
        return(as.factor(paste0(x, " (", unit, ")")))
      } else {
        return(paste0(x, " (", unit, ")"))
      }
    }
  } else { # is data.frame
    # no unit is specified -> take from unit column
    stopifnot(col %in% colnames(x))
    if (is.null(unit)) {
      x[col] <- unitjoin(x[[col]], x[["unit"]])
      x["unit"] <- NULL
    } else {
      # use what is specified
      x[col] <- unitjoin(x[[col]], unit)
    }
    return(x)
  }
}
