.mselectSupport <- function(search, where, ndim, names) {
  search <- escapeRegex(search)
  matches <- NULL
  while (length(search) > 0) {
    end <- min(length(search), 1000)
    subsearch <- search[1:end]
    search <- search[-(1:end)]
    subsearch <- paste0("(", paste(subsearch, collapse = "|"), ")")
    subsearch <- paste0("^", paste(rep("[^\\.]*\\.", where - 1), collapse = ""), subsearch,
                     paste(rep("\\.[^\\.]*", ndim - where), collapse = ""), "$")
    matches <- union(matches, grep(subsearch, names))
  }
  return(names[sort(matches)])
}

.mselectCoords <- function(x, ...) {
  if (is.null(names(dimnames(x)))) stop("Dimnames must have names in order to use mselect!")
  args <- list(...)
  if (length(args) == 1) if (is.list(args[[1]])) args <- args[[1]]
  sep <- "\\."
  sets <- strsplit(names(dimnames(x)), sep)

  .getItemsX <- function(x, dim) {
    out <- getItems(x, dim = dim)
    if (is.null(out)) out <- 1
    return(out)
  }

  i <- .getItemsX(x, dim = 1)
  j <- .getItemsX(x, dim = 2)
  k <- .getItemsX(x, dim = 3)

  for (n in names(args)) {
    where <- grep(paste0("^", n, "$"), unlist(sets))
    if (length(where) > 1) stop(paste0("set name \"", n, "\" found more than once!"))
    if (length(where) == 0) stop(paste0("set name \"", n, "\" not found!"))

    if (where <= length(sets[[1]])) {
      # spatial
      ndim <- nchar(gsub("[^\\.]", "", getCells(x)[1])) + 1
      i <- .mselectSupport(args[[n]], where, ndim, i)
    } else if (where <= length(unlist(sets[1:2]))) {
      # temporal
      j <- .mselectSupport(args[[n]], where - length(sets[[1]]), length(sets[[2]]), j)
    } else {
      # data
      k <- .mselectSupport(args[[n]], where - length(unlist(sets[1:2])), length(sets[[3]]), k)
    }
  }
  m <- list(i = i, j = j, k = k)
  m[lapply(m, length) == 0] <- NULL
  if (is.null(m$j) && dim(x)[2] > 0) m$j <- -seq_len(dim(x)[2])
  return(m)
}





#' MSelect
#'
#' Select values from a MAgPIE-object
#'
#' This functions only work for MAgPIE objects with named dimensions as the
#' dimension name (set_name) has to be used to indicate in which dimension the
#' entries should be searched for!
#'
#' @aliases mselect mselect<-
#' @param x MAgPIE object
#' @param ... entry selections of the form
#' \code{set_name=c(set_elem1,set_elem2)}. Alternatively a single list element
#' containing these selections can be provided.
#' @param collapseNames Boolean which decides whether names should be collapsed
#' or not.
#' @param value values on which the selected magpie entries should be set.
#' @return The reduced MAgPIE object containing only the selected entries or
#' the full MAgPIE object in which a selection of entries was manipulated.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{collapseNames}}, \code{"\linkS4class{magpie}"}
#' @examples
#'
#' pop <- maxample("pop")
#' mselect(pop, i = c("AFR", "EUR"), scenario = "A2", t = "y2035")
#' @export
mselect <- function(x, ..., collapseNames = FALSE) {
  m <- .mselectCoords(x, ...)
  if (collapseNames) return(collapseNames(x[m$i, m$j, m$k]))
  return(x[m$i, m$j, m$k])
}

#' @describeIn mselect replace values in magpie object
#' @export
"mselect<-" <- function(x, ..., value) { # nolint
  m <- .mselectCoords(x, ...)
  x[m$i, m$j, m$k] <- value
  return(x)
}
