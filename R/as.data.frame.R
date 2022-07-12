#' ~~ Methods for Function as.data.frame ~~
#'
#' ~~ Methods for function \code{as.data.frame} ~~
#'
#'
#' @name as.data.frame-methods
#' @aliases as.data.frame as.data.frame-methods as.data.frame,ANY-method
#' as.data.frame,magpie-method
#' @docType methods
#' @param x A MAgPIE-object
#' @param rev The revision of the algorithm that should be used for conversion.
#' rev=1 creates columns with the predefined names Cell, Region, Year, Data1,
#' Data2,... and Value, rev=2 uses the set names of the MAgPIE object for
#' naming and adds an attribute "dimtype" to the data.frame which contains
#' information about the types of the different columns (spatial, temporal,
#' data or value), rev=3 is identical to rev=2 except that characters are
#' not being converted to factors (stringsAsFactors = FALSE).
#' @param raw Logical to control whether years beginning with "y" should be
#' converted to integers (without "y") and coordinates should be converted to
#' numerics. If set to raw columns are returned as they are in the initial
#' object.
#' @section Methods: \describe{
#'
#' \item{list("signature(x = \"magpie\")")}{ Conversion creates columns for
#' Cell, Region, Year, Data1, Data2,... and Value } }
#' @keywords methods
#' @examples
#'
#' pop <- maxample("pop")
#' head(as.data.frame(pop))
#' head(as.data.frame(pop, rev = 2))
#'
#' a <- maxample("animal")
#' head(as.data.frame(a, rev = 3))
#' head(as.data.frame(a, rev = 3, raw = TRUE))
#' attr(as.data.frame(a, rev = 3), "dimtype")
#'
#' @importFrom utils type.convert
#' @exportMethod as.data.frame

setMethod("as.data.frame",
  signature(x = "magpie"),
  function(x, rev = 1, raw = FALSE) {
    if (rev == 1) {
      yearsAsIntegers <- suppressWarnings(getYears(x, as.integer = TRUE))
      if (raw || any(is.na(getYears(x)) != is.na(yearsAsIntegers))) {
        dimnames(x)[[2]] <- getYears(x)
      } else {
        dimnames(x)[[2]] <- yearsAsIntegers
      }
      if (is.null(dimnames(x)[[2]])) dimnames(x)[[2]] <- 0
      if (is.null(dimnames(x)[[3]])) dimnames(x)[[3]] <- "NA"
      if (any(dim(x) == 0)) {
        return(data.frame())
      } else {
        x <- as.data.frame(as.table(x))
      }
      if (all(grepl(".", x[[3]], fixed = TRUE))) {
        levels(x[[3]]) <- gsub("\\.$", "\\.STRINGTORESETAG", levels(x[[3]]))
        tmp <- data.frame(t(matrix(unlist(strsplit(as.character(x[[3]]), split = "\\.")), ncol = nrow(x))),
                          stringsAsFactors = FALSE)
        for (i in seq_len(ncol(tmp))) {
          tmp[[i]] <- factor(tmp[[i]], unique(tmp[[i]]))
          levels(tmp[[i]]) <- gsub("STRINGTORESETAG", "", levels(tmp[[i]]))
        }
        x <- cbind(x[, 1:2], tmp, x[4])
      }
      colnames(x) <- c("Region", "Year", paste("Data", 1:(dim(x)[2] - 3), sep = ""), "Value")
      x <- cbind(Cell = suppressWarnings(as.integer(gsub("^[^\\.]*\\.", "", x$Region))), x)
      x$Region <- gsub("\\..*$", "", x$Region)
      return(x)
    } else if (rev == 2) {
      return(asDataFrameX(x, raw = raw, stringsAsFactors = TRUE))
    } else if (rev == 3) {
      return(asDataFrameX(x, raw = raw, stringsAsFactors = FALSE))
    } else {
      stop('Unknown revision "', rev, '"!')
    }
  }
)

asDataFrameX <- function(x, raw, stringsAsFactors = FALSE) { # nolint
  x <- clean_magpie(x, what = "sets")
  if (any(dim(x) == 0)) {
    return(data.frame())
  }
  dimnames <- dimnames(x)
  x <- as.data.frame(as.table(x), stringsAsFactors = FALSE)
  names(x)[4] <- ".value"
  what <- ".value"
  types <- c(".spat", ".temp", ".data")
  for (i in 3:1) {
    if (grepl(".", names(x)[i], fixed = TRUE)) {
      tmp <- data.frame(t(matrix(unlist(strsplit(as.character(x[[i]]), split = "\\.")), ncol = nrow(x))),
                        stringsAsFactors = FALSE)
      names(tmp) <-  strsplit(names(x)[i], split = "\\.")[[1]]
      if (i == 1) {
        x <- cbind(tmp, x[2:length(x)])
      } else {
        x <- cbind(x[1:(i - 1)], tmp, x[(i + 1):length(x)])
      }
      what <- c(paste0(types[i], seq_len(dim(tmp)[2])), what)
    } else if (is.null(dimnames[[i]])) {
      x <- x[-i]
    } else {
      what <- c(paste0(types[i], 1), what)
    }
  }
  # use other types than character if possible
  for (i in seq_len(ncol(x))) {
    if (is.character(x[[i]])) {
      if (!raw) {
        # convert years
        if (grepl("^y[0-9]{1,4}$", x[[i]][1]) && all(grepl("^y[0-9]{1,4}$", x[[i]]))) {
          x[[i]] <- substring(x[[i]], 2)
        }
        # convert coordinates
        if (grepl("^-?[0-9]+p-?[0-9]+", x[[i]][1]) && all(grepl("^-?[0-9]+p-?[0-9]+", x[[i]]))) {
          x[[i]] <- sub("p", ".", x[[i]])
        }
      }
      x[[i]] <- type.convert(x[[i]], as.is = TRUE)
      if (stringsAsFactors && is.character(x[[i]])) x[[i]] <- factor(x[[i]], unique(x[[i]]))
    }
  }
  attr(x, "dimtype") <- what
  return(x)
}
