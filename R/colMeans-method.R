#' @importFrom methods new
#' @exportMethod colMeans
setMethod("colMeans",
  signature(x = "magpie"),
  function(x, na.rm = FALSE, dims = 1, ...) { #nolint
    xGlo <- colMeans(as.array(x), na.rm = na.rm, ...)
    out <- new("magpie", array(xGlo, dim = c(1, dim(xGlo)), dimnames = c("GLO", dimnames(xGlo))))
    return(out)
  }
)
