#' @importFrom methods Ops callGeneric new
#' @importFrom units set_units
#' @exportMethod Ops
setMethod(Ops, signature(e1='magpie', e2='magpie'),
          function(e1, e2){
            if(isTRUE(getOption("magclass_metadata"))) {
              eq  <- .Generic %in% c("+", "-", "==", "!=", "<", ">", "<=", ">=") # requiring identical units
              prd <- .Generic %in% c("*", "/", "%/%")                            # product-type
              pw  <- .Generic %in% c( "**", "^")                                 # power-type
              mod <- .Generic == "%%"                                            # modulo
              if(eq) {
                e2 <- set_units(e2,getMetadata(e1,"unit"))
              }
              if(pw) {
                e2 <- set_units(e2,"1")
              }
            }
            
            
            if(is.null(dim(e1)) & is.null(dim(e2))) {
              return(callGeneric(e1@.Data,e2@.Data))
            }
            e2 <- magpie_expand(e2,e1)  
            e1 <- magpie_expand(e1,e2)
            if(any(unlist(dimnames(e1))!=unlist(dimnames(e2)))) stop("MAgPIE objects after MAgPIE object expansion do not agree in dimnames! magpie_expand seems to be bugged!\n e1:",
                                                                      paste(unlist(dimnames(e1))[unlist(dimnames(e1))!=unlist(dimnames(e2))],collapse=" "),"\n e2:",paste(unlist(dimnames(e2))[unlist(dimnames(e1))!=unlist(dimnames(e2))],collapse=" ")) 
            out <- new("magpie",callGeneric(e1@.Data,e2@.Data))
            if(isTRUE(getOption("magclass_metadata"))) {
              out <- updateMetadata(out,list(e1,e2), calcHistory = "merge", source = "merge")
              if(eq | mod) getMetadata(out,"unit") <- getMetadata(e1,"unit")
              if(prd) getMetadata(out,"unit") <- callGeneric(getMetadata(e1,"unit"),getMetadata(e2,"unit"))
              if(pw) getMetadata(out,"unit") <- "mixed"
              print(getMetadata(out))
            }
            return(out)  
          }
)  
