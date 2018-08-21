#' @importFrom methods Ops callGeneric new
#' @importFrom udunits2 ud.are.convertible ud.is.parseable ud.convert
#' @importFrom units as_units install_symbolic_unit 
#' @exportMethod Ops
setMethod(Ops, signature(e1='magpie', e2='magpie'),
          function(e1, e2){
            
            if (withMetadata()) {
              u1 <- getMetadata(e1,"unit")
              if (!ud.is.parseable(u1)) {
                u1 <- gsub(" ","_",as.character(u1))
                install_symbolic_unit(u1) #,dimensionless=FALSE)
                u1 <- as_units(u1)
              }
              if (!is.null(e2)) {
                u2 <- getMetadata(e2,"unit")
                if (!ud.is.parseable(u2)) {
                  u2 <- gsub(" ","_",as.character(u2))
                  install_symbolic_unit(u2) #,dimensionless=FALSE)
                  u2 <- as_units(u2)
                }
                if (units(u2)!=units(u1)) {
                  if (ud.are.convertible(u1,u2)) {
                    conv_factor <- as.numeric(ud.convert(u2,units(u2),units(u1))/u2)
                    units(u2) <- units(u1)
                  }else if (as.character(units(u2))!="1") {
                    if (.Generic == "%%") {
                      stop("modulo operation is only allowed for objects with inter-convertible or unitless units")
                    }
                  }else  conv_factor <- 1
                }else  conv_factor <- 1
                units_out <- callGeneric(u1,u2)
              }else {
                units_out <- callGeneric(u1)
                conv_factor <- 1
              }
              units_out <- units_out/(as.numeric(units_out))
            }else {
              conv_factor <- 1
              units_out <- as_units(1)
            }
            
            if(is.null(dim(e1)) & is.null(dim(e2))) {
              return(callGeneric(e1@.Data,e2@.Data*conv_factor))
            }
            e2 <- magpie_expand(e2,e1)  
            e1 <- magpie_expand(e1,e2)
            if(any(unlist(dimnames(e1))!=unlist(dimnames(e2)))) stop("MAgPIE objects after MAgPIE object expansion do not agree in dimnames! magpie_expand seems to be bugged!\n e1:",
                                                                      paste(unlist(dimnames(e1))[unlist(dimnames(e1))!=unlist(dimnames(e2))],collapse=" "),"\n e2:",paste(unlist(dimnames(e2))[unlist(dimnames(e1))!=unlist(dimnames(e2))],collapse=" "))
            
            out <- new("magpie",callGeneric(e1@.Data,e2@.Data*conv_factor))
            return(updateMetadata(out,list(e1,e2),unit=units_out,calcHistory="merge"))
          }
)  
