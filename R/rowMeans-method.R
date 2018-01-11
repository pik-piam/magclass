#' @exportMethod rowMeans
#' 
setMethod("rowMeans",
          signature(x = "magpie"),
          function (x, na.rm = FALSE, dims = 1, ...) 
          {
            out <- rowMeans(as.array(x), na.rm=na.rm, dims=dims, ...)
            out <- as.magpie(as.array(x))
            getMetadata(out) <- getMetadata(x)
            return(out)
          }
          )