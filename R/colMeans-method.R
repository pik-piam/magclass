#' @importFrom methods new
#' @exportMethod colMeans
setMethod("colMeans",
          signature(x = "magpie"),
          function (x, na.rm = FALSE, dims = 1, ...) 
          {
            x_array<-as.array(x)
            x_glo<-colMeans(x_array,na.rm=na.rm,...)
            out<-new("magpie",array(x_glo,dim=c(1,dim(x_glo)),dimnames=c("GLO",dimnames(x_glo))))
            getMetadata(out) <- getMetadata(x)
            return(out)
          }
          )

