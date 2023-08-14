#' str
#'
#' str method for MAgPIE objects for conventient display of the structure of a magpie object.
#'
#' In contrast to the default str this will not show the attributes of object@.Data as these
#' contain only a duplicate of dimnames. Also, dim is not shown, because the information it
#' provides is implicitly included in dimnames.
#'
#' @param object MAgPIE object
#' @param ... arguments to be passed to or from other methods.
#' @author Pascal Sauer
#' @seealso \code{\link[utils]{str}}
#' @examples
#' str(maxample("pop"))
#' @export
str.magpie <- function(object, ...) {
  cat("A magpie object (package: magclass)\n")
  cat(" @ .Data: ")
  utils::str(c(object@.Data)) # c() is important here to strip attributes
  attrib <- attributes(object)
  attrib <- attrib[setdiff(names(attrib), c("class", "dim"))]
  cat(paste(utils::capture.output({
    utils::str(attrib)
  })[-1], collapse = "\n"))
  cat("\n")
  return(invisible(NULL))
}
