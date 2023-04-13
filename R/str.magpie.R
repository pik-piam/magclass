#' @export
str.magpie <- function(object, ...) {
  cat("A magpie object (package: magclass)\n")
  cat(" @ .Data: ")
  str(c(object@.Data)) # c() is important here to strip attributes
  attrib <- attributes(object)
  cat(paste(capture.output({
    str(attrib[setdiff(names(attrib), c("class", "dim"))])
  })[-1], collapse = "\n"))
  cat("\n")
  return(invisible(NULL))
}
