#' where
#'
#' Analysis function for magpie objects
#'
#'
#' @param x A logical statement with a magpie object
#' @param plot deprecated. Use the function whereplot in package luplot.
#' @return A list of analysis parameters
#' @author Benjamin Leon Bodirsky, Jan Philipp Dietrich
#' @seealso whereplot in package luplot
#' @examples
#'
#' p <- maxample("pop")
#' where(p > 500)
#' @export
where <- function(x, plot = NULL) {
  if (!is.null(plot)) warning("Argument plot is deprecated. Use whereplot() for plot=T and where() for plot=F.")

  analysis <- list()

  .whichX <- function(x) {
    tmp <- which(x, arr.ind = TRUE)
    for (i in 1:3) {
      items <- getItems(x, dim = i)
      if (is.null(items)) items <- "dummy"
      tmp[, i] <- items[as.numeric(tmp[, i])]
    }
    out <- list(individual = tmp)
    out$regions <- unique(out$individual[, 1])
    out$years   <- unique(out$individual[, 2])
    out$data    <- unique(out$individual[, 3])
    return(out)
  }

  analysis$true  <- .whichX(x == TRUE)
  analysis$false <- .whichX(x == FALSE)
  analysis$na    <- .whichX(is.na(x))

  # check how many T, F, NAs and NANs-

  tmp <- c(sum((x == TRUE) * 1, na.rm = TRUE), sum((x == FALSE) * 1, na.rm = TRUE), sum(is.na(x) * 1, na.rm = TRUE))
  other <- length(x) - sum(tmp)
  if (other != 0) {
    warning("function is made to analyse logical statements. Your values contain values that are not 1/0/NA")
  }
  tmp <- c(tmp, other)
  names(tmp) <- c("TRUE", "FALSE", "NA", "other")
  analysis$summary <- tmp

  return(analysis)
}
