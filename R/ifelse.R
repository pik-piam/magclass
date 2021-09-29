#' @importFrom methods new
#' @exportMethod ifelse
setMethod("ifelse",
          signature(test = "magpie"),
          function(test, yes, no) {
            if (!all(suppressWarnings(as.integer(test)) %in% c(0, 1))) {
              stop("'test' must only contain booleans!")
            }
            yes <- test * yes
            no <- (1 - test) * no
            yes[!test] <- 0
            no[test] <- 0
            return(yes + no)
          }
)
