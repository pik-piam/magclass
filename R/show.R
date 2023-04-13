#' @exportMethod show
setMethod("show", signature(object = "magpie"), function(object) {
  print(object)
  return(invisible(NULL))
})
