#' @importFrom methods new
#' @exportMethod is.na
setMethod("is.na",
          signature(x = "magpie"),
          function(x) {
            return(as(is.na(x@.Data), "magpie"))
          }
)

#' @exportMethod is.nan
setMethod("is.nan",
          signature(x = "magpie"),
          function(x) {
            return(as(is.nan(x@.Data), "magpie"))
          }
)

#' @exportMethod is.infinite
setMethod("is.infinite",
          signature(x = "magpie"),
          function(x) {
            return(as(is.infinite(x@.Data), "magpie"))
          }
)

#' @exportMethod is.finite
setMethod("is.finite",
          signature(x = "magpie"),
          function(x) {
            return(as(is.finite(x@.Data), "magpie"))
          }
)
