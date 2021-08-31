#' Get sets
#'
#' Extracts sets of a MAgPIE-object if available
#'
#'
#' @aliases getSets getSets<-
#' @param x MAgPIE object
#' @param sep A character separating joined dimension names
#' @param fulldim bool: Consider dimension 3 as a possible aggregate of more
#' dimensions (TRUE) or stick to it as one dimension (FALSE)
#' @param value A vector with set names you want to replace the current set
#' names of the object with.
#' @return Sets of the MAgPIE-object. If no information about contained sets is
#' available NULL
#' @author Markus Bonsch, Jan Philipp Dietrich
#' @seealso \code{\link{getRegions}},
#' \code{\link{getNames}},\code{\link{getYears}}, \code{\link{getCPR}},
#' \code{\link{read.magpie}}, \code{\link{write.magpie}},
#' \code{"\linkS4class{magpie}"}
#' @examples
#'
#' a <- new.magpie("GLO.1", 2000, c("a.o1", "b.o1", "a.o2"))
#'  getSets(a) <- c("reg", "cell", "t", "bla", "blub")
#'  getSets(a)
#'
#'  getSets(a)["d3.1"] <- "BLA"
#'  getSets(a, fulldim = FALSE)
#'  getSets(a)
#' @export
getSets <- function(x, fulldim = TRUE, sep = ".") {
  out <- names(dimnames(x))[drop = FALSE]
  if (is.null(out)) return(NULL)

  if (fulldim) { #nolint
    tmp <- strsplit(out, split = sep, fixed = TRUE)
    tmp <- lapply(tmp, FUN = function(x) {
                            if (length(x) == 0) x <- NA
                            return(x)
                          }
    )
    addDimCode <- function(x) {
      names(x) <- seq_along(x)
      return(x)
    }
    tmp <- lapply(tmp, addDimCode)
    names(tmp) <- paste0("d", seq_along(tmp))

    out <- unlist(tmp)
  }
  return(out)
}



#' @describeIn getSets replace set names
#' @export
`getSets<-` <- function(x, fulldim = TRUE, sep = ".", value) { #nolint
   # clean x
   uncleanSets <- getSets(x)
   x <- clean_magpie(x, what = "sets")
   if (is.null(value)) return(x)
   if (is.null(names(dimnames(x))) || length(value) %in% c(0, 3)) fulldim <- FALSE # nolint
   if (!fulldim) { # nolint
     names(dimnames(x)) <- value
     return(x)
   } else {
     sNow <- getSets(x, fulldim = TRUE)
     if (length(value) != length(sNow)) {
       if (length(value) != length(uncleanSets)) {
         stop("Input length does not agree with the number of sets in x!")
       }
       # clean value
       oldValue <- value
       names(oldValue) <- names(uncleanSets)
       value <- sNow
       value[names(oldValue)] <- oldValue
     }

     mainDim <- as.integer(substring(names(sNow), 2, 2))
     sNew <- NULL
     for (i in 1:3) {
      sNew <- c(sNew, paste(value[mainDim == i], collapse = sep))
     }
     getSets(x, fulldim = FALSE, sep = sep) <- sNew
     return(x)
   }
}
