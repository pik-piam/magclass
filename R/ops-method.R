#' @importFrom methods Ops callGeneric new
#' @exportMethod Ops
setMethod(Ops, signature(e1='magpie', e2='magpie'),
          function(e1, e2){
            if(is.null(dim(e1)) & is.null(dim(e2))) {
              return(callGeneric(e1@.Data,e2@.Data))
            }
            e2 <- magpie_expand(e2,e1)
            e1 <- magpie_expand(e1,e2)
            if(any(unlist(dimnames(e1))!=unlist(dimnames(e2)))) stop("MAgPIE objects after MAgPIE object expansion do not agree in dimnames! magpie_expand seems to be bugged!\n e1:",
                                                                      paste(unlist(dimnames(e1))[unlist(dimnames(e1))!=unlist(dimnames(e2))],collapse=" "),"\n e2:",paste(unlist(dimnames(e2))[unlist(dimnames(e1))!=unlist(dimnames(e2))],collapse=" ")) 
            
            out <- new("magpie",callGeneric(e1@.Data,e2@.Data))
            if(isTRUE(getOption("magclass_metadata"))){
              e1unit <- getMetadata(e1,"unit")
              e2unit <- getMetadata(e2,"unit")
              if (!is(e1unit,"units"))  e1unit <- make_unit(e1unit)
              if (!is(e2unit,"units"))  e2unit <- make_unit(e2unit)
              outUnit <- callGeneric(e1unit, e2unit)
              out <- updateMetadata(out,list(e1,e2),unit=outUnit,source="merge",calcHistory="merge",description="merge")
            }
            return(out)
          }
)  
