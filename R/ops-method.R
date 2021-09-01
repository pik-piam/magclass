#' @importFrom methods Ops callGeneric new
#' @exportMethod Ops
setMethod(Ops, signature(e1 = "magpie", e2 = "magpie"),
  function(e1, e2) {
    e2 <- magpie_expand(e2, e1)
    e1 <- magpie_expand(e1, e2)
    # if one object is empty, return it without additional calculations
    if (length(e1) == 0) return(e1)
    if (length(e2) == 0) return(e2)
    if (any(unlist(dimnames(e1)) != unlist(dimnames(e2)))) {
      stop("MAgPIE objects after MAgPIE object expansion do not agree in dimnames! magpie_expand seems to be bugged!")
    }
    return(new("magpie", callGeneric(e1@.Data, e2@.Data)))
  }
)

setMethod(Ops, signature(e1 = "magpie", e2 = "numeric"),
  function(e1, e2) {
    return(new("magpie", callGeneric(e1@.Data, e2)))
  }
)

setMethod(Ops, signature(e1 = "numeric", e2 = "magpie"),
  function(e1, e2) {
    return(new("magpie", callGeneric(e1, e2@.Data)))
  }
)
