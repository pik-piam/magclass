#' Write file in report format
#'
#' This function is deprecated, please use \code{\link{write.report}} instead.
#'
#'
#' @param ... arguments are forwarded to \code{\link{write.report}}
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{write.report}}
#' @export
write.report2 <- function(...) {
  .Deprecated("write.report")
  return(write.report(...))
}
