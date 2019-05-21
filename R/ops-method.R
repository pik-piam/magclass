#' @importFrom methods Ops callGeneric new
#' @exportMethod Ops
setMethod(Ops, signature(e1='magpie',e2='magpie'),
          function(e1, e2){
            if (withMetadata()) {
              units::units_options(auto_convert_names_to_symbols=FALSE, allow_mixed=FALSE, negative_power=TRUE, set_units_mode="standard")
              e1 <- install_magpie_units(e1)
              u1 <- units(e1)
              e2 <- install_magpie_units(e2)
              u2 <- units(e2)
              if (units(u2)!=units(u1)) {
                if (are_units_convertible(u1,u2)) {
                  e2 <- set_magpie_units(e2,u1)
                }else if (.Generic %in% c("<", ">", "==", "!=", "<=", ">=", "+", "-")) {
                  stop(.Generic," operation cannot be performed because units ",as.character(units(u1)),"and ",as.character(units(u2))," are not inter-convertible!")
                }else if (as.character(units(u2))!="1") {
                  if (.Generic == "%%") {
                    stop("modulo operation is only allowed for objects with inter-convertible or unitless units")
                  }
                }
              }else if (as.character(units(u1))=="unknown") {
                if (length(sys.calls())<3)  warning("Units for both operands are unknown! Are you sure they are compatible?")
              }
              if (any(.Generic == c("-","+"))) {
                if (as.numeric(u1)>as.numeric(u2)) {
                  e2 <- e2*as.numeric(u2)/as.numeric(u1)
                  u2 <- u2-u2
                }else {
                  e1 <- e1*as.numeric(u1)/as.numeric(u2)
                  u1 <- u1-u1
                }
              }
              units_out <- callGeneric(u1,u2)
              if (is(units_out,"units")) {
                if (as.character(units(units_out))=="1") {
                  if (as.numeric(units_out)<1e3) {
                    e2 <- e2*as.numeric(units_out)
                    units_out <- units_out/as.numeric(units_out)
                  }
                }else if (gsub("\\d","",gsub("[[:punct:]]","",as.character(units(units_out))))=="unknown") {
                  units_out <- units::as_units(as.numeric(units_out),"unknown")
                }
              }else if (is.logical(units_out))  units_out <- "keep"
            }else  units_out <- "keep"
            if(is.null(dim(e1)) & is.null(dim(e2))) {
              return(callGeneric(e1@.Data,e2@.Data))
            }
            e2 <- magpie_expand(e2,e1)  
            e1 <- magpie_expand(e1,e2)
            # if one object is empty, return it without additional calculations
            if(length(e1)==0) return(e1)
            if(length(e2)==0) return(e2)
            if(any(unlist(dimnames(e1))!=unlist(dimnames(e2)))) stop("MAgPIE objects after MAgPIE object expansion do not agree in dimnames! magpie_expand seems to be bugged!\n e1:",
                                                                     paste(unlist(dimnames(e1))[unlist(dimnames(e1))!=unlist(dimnames(e2))],collapse=" "),"\n e2:",paste(unlist(dimnames(e2))[unlist(dimnames(e1))!=unlist(dimnames(e2))],collapse=" "))
            out <- new("magpie",callGeneric(e1@.Data,e2@.Data))
            if (is(units_out,"units")) {
              if (as.numeric(units_out) > 1e3) {
                exponent <- log(as.numeric(units_out),10)
                if (exponent %% 3 != 0) {
                  adj <- exponent %% 3
                  units_out <- units_out*(10^(-adj))
                  out <- out*(10^adj)
                }
              }
            }
            if (.Generic %in% c("==",">","<","<=",">=","!="))  calcHistory <- "copy"
            else if (length(sys.calls())>2 && as.character(sys.call(-2))[1] %in% c("/","*","+","-","^","%%","%/%","==","<",">","<=","=>","!="))  calcHistory <- "copy"
            else  calcHistory <- "update"
            return(updateMetadata(out,list(e1,e2),unit=units_out,calcHistory=calcHistory))
          }
)  

setMethod(Ops, signature(e1='magpie',e2='numeric'),
          function(e1, e2){
            if (withMetadata() & !is.null(units(e1))) {
              units::units_options(auto_convert_names_to_symbols=FALSE, allow_mixed=FALSE, negative_power=TRUE, set_units_mode="standard")
              e1 <- install_magpie_units(e1)
              u1 <- units(e1)
              if (.Generic %in% c("<", ">", "==", "!=", "<=", ">=", "+", "-", "%%")) {
                units_out <- "keep"
                calcHistory <- "copy"
              }else {
                units_out <- callGeneric(u1,e2)
                if (length(sys.calls())>2 && as.character(sys.call(-2))[1] %in% c("/","*","+","-","^","%%","%/%","==","<",">","<=","=>","!=")) {
                  calcHistory <- "copy"
                }else  calcHistory <- "update"
              }
              if (is(units_out,"units") && grepl("unknown",as.character(units(units_out)))) {
                units_out <- units::as_units(as.numeric(units_out),"unknown")
              }
              if (.Generic=="*") {
                if (e2!=0) {
                  units_out <- units_out/e2
                }
              }else if (.Generic=="/") {
                units_out <- units_out*e2
              }else if (.Generic=="%%") {
                e1 <- e1*as.numeric(u1)
              }
              if(is.null(dim(e1))) {
                return(callGeneric(e1@.Data,e2))
              }
              out <- new("magpie",callGeneric(e1@.Data,e2))
              if (is(units_out,"units")) {
                if (as.numeric(units_out) > 1e3) {
                  exponent <- log(as.numeric(units_out),10)
                  if (exponent %% 3 != 0) {
                    adj <- exponent %% 3
                    units_out <- units_out*(10^(-adj))
                    out <- out*(10^adj)
                  }
                }
              }else if (.Generic=="%%") {
                out <- out/as.numeric(u1)
              }
            }else {
              if(is.null(dim(e1))) {
                return(callGeneric(e1@.Data,e2))
              }
              out <- new("magpie",callGeneric(e1@.Data,e2))
              units_out <- "keep"
              calcHistory <- "keep"
            }
            return(updateMetadata(out,e1,unit=units_out,calcHistory=calcHistory))
          }
)

setMethod(Ops, signature(e1='numeric',e2='magpie'),
          function(e1, e2){
            if (withMetadata() & !is.null(units(e2))) {
              units::units_options(auto_convert_names_to_symbols=FALSE, allow_mixed=FALSE, negative_power=TRUE, set_units_mode="standard")
              e2 <- install_magpie_units(e2)
              u2 <- units(e2)
              if (.Generic %in% c("<", ">", "==", "!=", "<", ">", "<=", ">=", "+", "-")) {
                units_out <- "keep"
                calcHistory <- "copy"
              }else {
                units_out <- callGeneric(u2,e1)
                if (length(sys.calls())>2 && as.character(sys.call(-2))[1] %in% c("/","*","+","-","^","%%","%/%","==","<",">","<=","=>","!=")) {
                  calcHistory <- "copy"
                }else  calcHistory <- "update"
              }
              if (is(units_out,"units") && grepl("unknown",as.character(units(units_out)))) {
                units_out <- units::as_units(as.numeric(units_out),"unknown")
              }
              if (.Generic=="*") {
                if (e1!=0) {
                  units_out <- units_out/e1
                }
                }else if (.Generic=="/") {
                units_out <- units_out*e1
              }
              if(is.null(dim(e2))) {
                return(callGeneric(e1,e2@.Data))
              }
              out <- new("magpie",callGeneric(e1,e2@.Data))
              if (is(units_out,"units")) {
                if (as.numeric(units_out) > 1e3) {
                  exponent <- log(as.numeric(units_out),10)
                  if (exponent %% 3 != 0) {
                    adj <- exponent %% 3
                    units_out <- units_out*(10^(-adj))
                    out <- out*(10^adj)
                  }
                }
              }
            }else {
              if(is.null(dim(e2))) {
                return(callGeneric(e1,e2@.Data))
              }
              units_out <- "keep"
              calcHistory <- "keep"
              out <- new("magpie",callGeneric(e1,e2@.Data))
            }
            return(updateMetadata(out,e2,unit=units_out,calcHistory=calcHistory))
          }
)
