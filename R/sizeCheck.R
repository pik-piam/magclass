#' sizeCheck
#'
#' Calculates expected magclass object length and checks that it stays below
#' the limit defined with magclass_sizeLimit (default = 10^9). This is useful
#' to prevent out of memory errors in case of unwanted object expansions
#' Ignored if \code{getOption("magclass_sizeLimit")} is negative.
#'
#' @param dim dimensions of the current object as returned by function \code{dim}
#' @author Jan Philipp Dietrich
#' @examples
#'
#' pop <- maxample("pop")
#' magclass:::sizeCheck(dim(pop))
sizeCheck <- function(dim) {
  if (is.null(getOption("magclass_sizeLimit"))) options(magclass_sizeLimit = 10^9) #nolint
  # estimate new object size and check against size limit
  if (getOption("magclass_sizeLimit") > 0) {
    size <- prod(dim)
    if (size > getOption("magclass_sizeLimit")) {
        stop("magclass object size limit reached! getOption(\"magclass_sizeLimit\")=", getOption("magclass_sizeLimit"))
    }
  }
}
