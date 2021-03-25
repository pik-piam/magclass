#' Replace Non-Finite Data
#' 
#' Replaces all instances of non-finite data (`NA`, `NaN`, `Inf`, and `-Inf`). 
#' 
#' @param x A vector or [`magpie`][magclass::magclass] object.
#' @param replace A value to replace non-finite data with.
#'
#' @return A vector or [`magpie`][magclass::magclass] object, same as `x`.
#' 
#' @author Michaja Pehl
#' @md
#' 
#' @examples
#' part  <- new.magpie(letters[1:3], years = 'y1995', names = 'foo')
#' total <- new.magpie(letters[1:3], years = 'y1995', names = 'foo')
#' 
#' part[,,]  <- c(0, 1, 2)
#' total[,,] <- c(0, 10, 10)
#' 
#' part / total
#' 
#' replace_non_finite(part / total)
#' 

#' @export
replace_non_finite <- function(x, replace = 0) {
  x[!is.finite(x)] <- replace
  
  return(x)
}
