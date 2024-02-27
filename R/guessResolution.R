#' guessResolution
#'
#' Guess the resolution of the given magpie object/coordinates by looking at the minimum
#' difference between unique sorted values. Fall back to 0.5 if guess is infinite.
#'
#' @param x A magpie object or the coordinates of a magpie object (the result
#' of \code{\link{getCoords}})
#' @return The guessed resolution of the data
#' @author Jan Philipp Dietrich, Pascal Sauer
#' @export
guessResolution <- function(x) {
  if (is.magpie(x)) {
    xy <- getCoords(x)
  } else {
    xy <- x
  }
  .tmp <- function(x) {
    return(min(diff(sort(unique(x)))))
  }
  guess <- min(.tmp(xy[[1]]), .tmp(xy[[2]]))
  # use 0.5deg as guess if it cannot be determined otherwise as this is
  # the default spatial resolution in the magpie universe.
  if (is.infinite(guess)) guess <- 0.5
  return(guess)
}
