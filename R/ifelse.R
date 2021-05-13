#' @importFrom methods new
#' @exportMethod ifelse
setMethod("ifelse",
          signature(test = "magpie"),
          function(test, yes, no) {
            if (!all(test %in% c(0,1))) stop("'test' must only contain booleans!")
            if (anyNA(yes) || anyNA(no)) stop("NA values not supported!")
            if (any(is.infinite(yes)) || any(is.infinite(no))) stop("Infinite values not supported!")
            return(test*yes + (1 - test)*no)
          }
)