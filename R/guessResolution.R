guessResolution <- function(xy) {
  .tmp <- function(x) {
    return(min(diff(sort(unique(x)))))
  }
  guess <- min(.tmp(xy[[1]]), .tmp(xy[[2]]))
  # use 0.5deg as guess if it cannot be determined otherwise as this is
  # the default spatial resolution in the magpie universe.
  if (is.infinite(guess)) guess <- 0.5
  return(guess)
}
