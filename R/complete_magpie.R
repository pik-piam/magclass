#' complete_magpie
#'
#' MAgPIE objects can be incomplete to reduce memory. This function blows up a
#' magpie object to its real dimensions, so you can apply unwrap.
#'
#'
#' @param x MAgPIE object which should be completed.
#' @param fill Value that shall be written into the missing entries
#' @param dim dimensions in which the completion should take place (1, 2 and/or 3). For
#' full completion use \code{1:3}
#' @return The completed MAgPIE object
#' @author Jan Philipp Dietrich, Benjamin Bodirsky
#' @seealso \code{\link{add_dimension}},\code{\link{clean_magpie}}
#' @examples
#'
#' pop <- maxample("pop")
#' complete_magpie(pop)
#'
#' ani <- maxample("animal")
#' complete_magpie(ani)
#' @export

complete_magpie <- function(x, fill = NA, dim = 3) { #nolint
  .expand <- function(x) {
    grid <- expand.grid(x, stringsAsFactors = FALSE)
    return(sort(apply(grid, 1, paste, collapse = ".")))
  }
  .completeSize <- function(x, dimIn, dim) {
    .prod <- function(x) return(prod(vapply(x, length, integer(1))))
     out <- dimIn
     out[dim] <- vapply(x[dim], .prod, double(1))
     out[out == 0] <- 1
     return(out)
  }
  dim <- sort(unique(dim))
  if (any(!is.element(dim, seq_len(3)))) stop("Invalid dim selection (can only be set to 1, 2, or 3)!")
  items <- getItems(x, split = TRUE)
  dimOut <- .completeSize(items, dim(x), dim)
  if (all(dimOut == dim(x))) return(x)
  dimnamesOut <-  dimnames(x)
  for (i in dim) {
      dimnamesOut[[i]] <- .expand(items[[i]])
  }
  out <- new("magpie", array(data = fill, dim = dimOut, dimnames = dimnamesOut))
  dimnamesIn <- dimnames(x)
  for (i in 1:3) {
    if (is.null(dimnamesIn[[i]])) dimnamesIn[[i]] <- seq_len(dim(x)[i])
  }
  out[dimnamesIn[[1]], dimnamesIn[[2]], dimnamesIn[[3]]] <- x
  getSets(out) <- getSets(x)
  return(out)
}
