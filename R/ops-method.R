#' @importFrom methods Ops callGeneric new
#' @importFrom udunits2 ud.are.convertible ud.convert
#' @importFrom units units_options mixed_units
#' @exportMethod Ops
setMethod(Ops, signature(e1='magpie',e2='magpie'),
          function(e1, e2){
            if (withMetadata()) {
              units_options(auto_convert_names_to_symbols=FALSE, allow_mixed=TRUE)
              e1 <- install_magpie_units(e1)
              u1 <- units(e1)
              e2 <- install_magpie_units(e2)
              u2 <- units(e2)
              #Mixed units handling does not account for scaling factors if units are converted during addition/subtraction
              if (is(u1,"mixed_units") | is(u2,"mixed_units")) {
                units_out <- vector()
                k <- 1
                for (i in 1:length(u1)) {
                  for (j in 1:length(u2)) {
                    units_out[k] <- as.character(units(callGeneric(as_units(u1[i]),as_units(u2[j]))))
                    k <- k+1
                  }
                }
                if (length(unique(units_out)) > 1) {
                  units_out <- mixed_units(1,unique(units_out))
                }else {
                  units_out <- unique(units_out)
                }
                conv_factor <- 1
              }else {
                if (units(u2)!=units(u1)) {
                  if (ud.are.convertible(as.character(units(u1)),as.character(units(u2)))) {
                    conv_factor <- as.numeric(ud.convert(u2,units(u2),units(u1))/u2)
                    units(u2) <- units(u1)
                  }else if (as.character(units(u2))!="1") {
                    if (.Generic == "%%") {
                      stop("modulo operation is only allowed for objects with inter-convertible or unitless units")
                    }else  conv_factor <- 1
                  }else  conv_factor <- 1
                }else  conv_factor <- 1
                units_out <- callGeneric(u1,u2)
                units_out <- units_out/(as.numeric(units_out))
                e2 <- e2*conv_factor
              }
            }else {
              units_out <- 'keep'
            }
            if(is.null(dim(e1)) & is.null(dim(e2))) {
              return(callGeneric(e1@.Data,e2@.Data))
            }
            e2 <- magpie_expand(e2,e1)  
            e1 <- magpie_expand(e1,e2)
            if(any(unlist(dimnames(e1))!=unlist(dimnames(e2)))) stop("MAgPIE objects after MAgPIE object expansion do not agree in dimnames! magpie_expand seems to be bugged!\n e1:",
                                                                     paste(unlist(dimnames(e1))[unlist(dimnames(e1))!=unlist(dimnames(e2))],collapse=" "),"\n e2:",paste(unlist(dimnames(e2))[unlist(dimnames(e1))!=unlist(dimnames(e2))],collapse=" "))
            out <- new("magpie",callGeneric(e1@.Data,e2@.Data))
            return(updateMetadata(out,list(e1,e2),unit=units_out))
          }
)  

setMethod(Ops, signature(e1='magpie',e2='numeric'),
          function(e1, e2){
            if (withMetadata()) {
              units_options(allow_mixed=TRUE)
              e1 <- install_magpie_units(e1)
              u1 <- units(e1)
              units_out <- callGeneric(u1,e2)
              units_out <- units_out/(as.numeric(units_out))
            }else {
              units_out <- 'keep'
            }
            if(is.null(dim(e1))) {
              return(callGeneric(e1@.Data,e2@.Data))
            }
            out <- new("magpie",callGeneric(e1@.Data,e2))
            return(updateMetadata(out,e1,unit=units_out))
          }
)
