#' ~~ Method for function \code{show} ~~
#'
#' Show a magpie object by calling the default show method, print, or str,
#' depending on options("magclass_show_func").
#'
#' @name show-methods
#' @aliases show show-methods show,ANY-method show,magpie-method
#' @docType methods
#' @param object A MAgPIE-object
#' @keywords methods
#' @author Pascal Führlich
#'
#' @exportMethod show
methods::setMethod("show", methods::signature(object = "magpie"), function(object) {
  # callNextMethod calls the show method that would be called if this method here would not exist
  switch(getOption("magclass_show_func", default = "show"),
         show = methods::callNextMethod(object),
         print = print(object),
         str = utils::str(object),
         stop("options('magclass_show_func') must be set to 'show', 'print', or 'str'"))
  return(invisible(NULL))
})
