#' suppressSpecificWarnings
#'
#' Like \code{\link{suppressWarnings}}, but instead of suppressing all warnings this only suppresses
#' warnings if they match the given pattern.
#'
#' @param expr The expression/code to evaluate, can be a block of code inside curly braces.
#' @param regularExpr Only warnings matching this regular expression are suppressed.
#' @param fixed Match the literal string given by regularExpr instead of interpreting it
#' as a regular expression. Passed to \code{\link{grepl}}.
#' @return The result of evaluating expr.
#'
#' @author Pascal Sauer
#' @seealso \code{\link{suppressWarnings}}
suppressSpecificWarnings <- function(expr, regularExpr, fixed = FALSE) {
  withCallingHandlers(expr, warning = function(m) {
    if (grepl(regularExpr, m[["message"]], fixed = fixed)) {
      invokeRestart("muffleWarning")
    }
  })
}
