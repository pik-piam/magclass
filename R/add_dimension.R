#' add_dimension
#'
#' Function adds a name dimension as dimension number "dim" with the name "add"
#' with an empty data column with the name "nm".
#'
#'
#' @param x MAgPIE object which should be extended.
#' @param dim The dimension number of the new dimension (e.g. 3.1)
#' @param add The name of the new dimension
#' @param nm The name of the first entry in dimension "add".
#' @return The extended MAgPIE object
#' @author Jan Philipp Dietrich, Benjamin Bodirsky
#' @seealso \code{\link{add_columns}},\code{\link{mbind}}
#' @examples
#'
#' a <- maxample("animal")
#' str(add_dimension(a, dim=3.2))
#' str(add_dimension(a, dim=2.3))
#' @export
add_dimension <- function(x, dim = 3.1, add = "new", nm = "dummy") { #nolint
  maindim <- round(dim)
  subdim  <- as.integer(sub("^.\\.","",dim))
  items <- getItems(x, dim=maindim, split = TRUE, full=TRUE)
  olddims <- seq_along(items)
  items[[add]] <- rep(nm, dim(x)[maindim])
  reorder <- c(olddims[olddims<subdim], length(items), olddims[olddims>=subdim])
  items <- items[reorder]
  getItems(x, dim=maindim, raw=TRUE) <- apply(as.data.frame(items),1,paste, collapse=".")
  getSets(x, fulldim = FALSE)[maindim] <- paste(names(items), collapse=".")
  return(x)
}
  