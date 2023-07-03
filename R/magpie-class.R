
#' Class "magpie" ~~~
#'
#' The MAgPIE class is a data format for cellular MAgPIE data with a close
#' relationship to the array data format. \code{is.magpie} tests if \code{x} is
#' an MAgPIE-object, \code{as.magpie} transforms \code{x} to an MAgPIE-object
#' (if possible).
#'
#'
#' @name magpie-class
#' @aliases magpie-class as.magpie as.magpie-methods as.magpie,magpie-method
#' as.magpie,array-method as.magpie,lpj-method as.magpie,data.frame-method
#' as.magpie,numeric-method as.magpie,NULL-method as.magpie,quitte-method
#' as.magpie,tbl_df-method as.magpie,RasterBrick-method as.magpie,logical-method
#' as.magpie,RasterLayer-method as.magpie,RasterStack-method as.magpie,SpatRaster-method
#' as.magpie,LPJmLData-method as.magpie,SpatVector-method
#' is.magpie [,magpie-method [,magpie,ANY,ANY-method [<-,magpie,ANY,ANY-method
#' [<-,magpie-method Ops,magpie,magpie-method Ops,magpie,numeric-method Ops,numeric,magpie-method
#' ifelse,magpie-method is.na,magpie-method is.nan,magpie-method
#' is.infinite,magpie-method is.finite,magpie-method
#' @docType class
#' @param x An object that should be either tested or transformed as/to an
#' MAgPIE-object.
#' @param ... additional arguments supplied for the conversion to a MAgPIE
#' object. Allowed arguments for arrays and dataframes are \code{spatial} and
#' \code{temporal} both expecting a vector of dimension or column numbers which
#' contain the spatial or temporal information. By default both arguments are
#' set to NULL which means that the \code{as.magpie} will try to detect
#' automatically the temporal and spatial dimensions. The arguments will just
#' overwrite the automatic detection. If you want to specify that the data does
#' not contain a spatial or temporal dimension you can set the corresponding
#' argument to 0. In addition \code{as.magpie} for data.frames is also
#' expecting an argument called \code{datacol} which expects a number stating
#' which is the first column containing data. This argument should be used if
#' the dimensions are not detected corretly, e.g. if the last dimension column
#' contains years which are then detected as values and therefore interpreted
#' as first data column. In addition an argument \code{tidy=TRUE} can be used
#' to indicate that the data.frame structure is following the rules of tidy
#' data (last column is the data column all other columns contain dimension
#' information). This information will help the conversion. \code{sep} defines
#' the dimension separator (default is ".") and \code{replacement} defines how
#' the separator as a reserved character should be converted in order to not
#' mess up with the object (default "_").
#' Another available argument for conversions of data.frames and quitte
#' objects to magpie is \code{filter} if set to TRUE (default)
#' "." (separator) will be replaced withe the \code{replacement} character and
#' empty entries will be replaced with a single space. If set to FALSE no filter
#' will be applied to the data.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("magpie", data, dim, dimnames, ...)}. MAgPIE objects have three
#' dimensions (cells,years,datatype) and the dimensionnames of the first
#' dimension have the structure "REGION.cellnumber". MAgPIE-objects behave the
#' same like array-objects with 2 exceptions: \cr 1.Dimensions of the object
#' will not collapse (e.g. \code{x[1,1,1]} will remain 3D instead of becoming
#' 1D)\cr 2.It is possible to extract full regions just by typing
#' \code{x["REGIONNAME",,]}. \cr\cr
#'
#' Please mind following standards: \cr Header must not contain any purely
#' numeric entries, but combinations of characters and numbers are allowed
#' (e.g. "bla","12" is forbidden, wheras "bla","b12" is allowed)\cr Years
#' always have the structure "y" + 4-digit number, e.g. "y1995"\cr Regions
#' always have the structure 3 capital letters, e.g. "AFR" or "GLO"\cr\cr This
#' standards are necessary to allow the scripts to detect headers, years and
#' regions properly and to have a distinction to other data.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{read.magpie}}, \code{\link{write.magpie}},
#' \code{\link{getRegions}}, \code{\link{getYears}}, \code{\link{getNames}},
#' \code{\link{getCPR}}, \code{\link{ncells}}, \code{\link{nyears}},
#' \code{\link{ndata}}
#' @keywords classes
#' @examples
#'
#' showClass("magpie")
#'
#' pop <- maxample("pop")
#'
#' # returning PAO and PAS for 2025
#' pop["PA", 2025, , pmatch = "left"]
#'
#' # returning CPA for 2025
#' pop["PA", 2025, , pmatch = "right"]
#'
#' # returning CPA PAO and PAS for 2025
#' pop["PA", 2025, , pmatch = TRUE]
#'
#' # returning PAS and 2025
#' pop["PAS", 2025, ]
#'
#' # return all entries for year 2025
#' pop[2025, dim = 2]
#'
#' # returning everything but values for PAS or values for 2025
#' pop["PAS", 2025, , invert = TRUE]
#'
#' # accessing subdimension via set name
#'
#' a <- maxample("animal")
#' a[list(country = "NLD", y = "53p25"), , list(species = c("rabbit", "dog"))]
#'
#' # please note that the list elements act as filter. For instance, the
#' # following example will not contain any dogs as the data set does
#' # not contain any dogs which are black.
#' a[list(country = "NLD", y = "53p25"), , list(species = c("rabbit", "dog"), color = "black")]
#'
#' # it is also possible to extract given combinations of subdimensions
#' # via a data-frame
#' df <- data.frame(getItems(a, 3, split = TRUE, full = TRUE))[c(1, 3, 4), ][3:2]
#' getItems(a[df], 3)
#'
#' # Unknown dimensions to be added in output!
#' df$blub <- paste0("bl", 1:dim(df)[1])
#' getItems(a[df], 3)
#' @exportClass magpie
#' @importFrom data.table as.data.table
#' @importFrom methods setClass


setClass("magpie", contains = "array", prototype = array(0, c(0, 0, 0)))

.mselectDF <- function(x, df) { # nolint
  if (is.null(names(dimnames(x)))) stop("Dimnames must have names in order to use mselect!")

  dims <- dimCode(names(df), x)
  dims[dims %in% 1:3] <- dims[dims %in% 1:3] + 0.1

  if (all(dims == 0)) stop("None of the dimensions in the mapping could be found in the magpie object!")
  dfmissing <- NULL
  if (any(dims == 0)) {
    dfmissing <- df[dims == 0]
    df <- df[dims != 0]
    dims <- dims[dims > 0]
  }

  if (anyDuplicated(dims)) stop('Dimension(s) "', paste(names(dims)[duplicated(dims)], collapse = '", "'),
    '" appear(s) more than once in the given mapping!')

  maindim <- unname(round(dims[1]))
  if (any(round(dims) != maindim)) stop("Data.frame must only contain subdimensions with a shared main dimension.",
    " Mixtures across main dimensions are not allowed!")

  sdims <- as.integer(substring(dims, 3))
  maxdim <- nchar(gsub("[^\\.]", "", names(dimnames(x))[maindim])) + 1 # nolint
  if (nrow(df) == 1) df[1, ] <- escapeRegex(df[1, ])
  if (nrow(df) > 1) df <- data.frame(sapply(df, escapeRegex)) # nolint
  dmissing <- which(!(1:maxdim %in% sdims))
  sdims <- c(sdims, dmissing)
  for (d in dmissing) df <- cbind(df, "[^\\.]*")
  df <- df[order(sdims)]
  search <-  paste0("^", apply(df, 1, paste, collapse = "\\."), "$")
  .subset <- function(x, f, dim) {
    if (all(f == 1) && dim(x)[dim] == 0) {
      d <- getItems(x, full = TRUE)
      d[[dim]] <- "dummy"
      x <- new.magpie(d[[1]], d[[2]], d[[3]], sets = getSets(x, fulldim = FALSE))
    }
    if (dim == 1) return(x[f, , ])
    if (dim == 2) return(x[, f, ])
    return(x[, , f])
  }
  found <- lapply(search, grep, getItems(x, dim = maindim, full = TRUE), perl = TRUE)
  x <- .subset(x, unlist(found), maindim)
  length <- unlist(lapply(found, length))

  if (!is.null(dfmissing)) {
    if (length(dfmissing) > 1) {
      nameExtRaw <- do.call("paste", c(dfmissing, sep = "."))
    } else {
      nameExtRaw <- dfmissing[[1]]
    }
    nameExtensions <- nameExtRaw[rep(seq_along(nameExtRaw), length)]
    getItems(x, dim = maindim, raw = TRUE) <- paste(getItems(x, dim = maindim, full = TRUE), nameExtensions,
      sep = ".")
    names(dimnames(x))[maindim] <- paste(getSets(x, fulldim = FALSE)[maindim], paste(names(dfmissing), collapse = "."),
      sep = ".")
  }
  if (any(length == 0) && nrow(df) > 0) {
    rowExtensions <- gsub("\\.", ".", sub("[^\\.]*", "NA", sub("^\\^", "", sub("\\$$", "", search[length == 0])),
      fixed = TRUE), fixed = TRUE)
    if (!is.null(dfmissing)) rowExtensions <- paste(rowExtensions, nameExtRaw[length == 0], sep = ".")
    elems <- rep(1, length(rowExtensions))
    ext <- .subset(x, elems, maindim)
    getItems(ext, dim = maindim, raw = TRUE) <- rowExtensions
    ext[, , ] <- NA
    x <- suppressWarnings(mbind(x, ext))
    if (!is.null(getOption("magclass.verbosity")) && getOption("magclass.verbosity") > 1) {
      message("NOTE (.mselectDF): The following elements were added to x as they appeared",
        " in the mapping but not in x: ", paste0(rowExtensions, collapse = ", "),
        " (values set to NA)\n")
    }
  }
  return(x)
}

.dimextract <- function(x, i, dim, pmatch = FALSE, invert = FALSE) { # nolint

  if (is.magpie(i) && is.logical(i)) {
    # check whether input is a 1D magpie object
    if (dim(i)[dim] == length(i)) {
      elemsX <- getItems(x, dim = dim)
      elemsI <- getItems(i, dim = dim)
      i <- as.vector(i)
      if (dim(x)[dim] > 1 && setequal(elemsX, elemsI)) {
        # reorder, if necessary
        i <- i[match(elemsX, elemsI)]
      }
    } else if (dim == 1) {
      i <- magpie_expand(i, x)
    }
  }
  if (is.factor(i)) i <- as.character(i)
  if (invert && is.numeric(i)) i <- -i
  if (!is.character(i) && !is.list(i)) return(i)

  if (length(i) == 0) return(NULL)
  dimnames <- dimnames(x)[[dim]]
  if (is.null(dimnames)) stop("Missing element names in dimensions ", dim, "!")
  .countdots <- function(i) {
    return(nchar(gsub("[^\\.]", "", i)))
  }
  if (!is.list(i) && .countdots(i[1]) == .countdots(dimnames[1]) && pmatch == FALSE) {
    # i vector seems to specify the full dimname
    if (!anyDuplicated(as.data.table(dimnames))) {
      if (invert) {
        return(which(!(dimnames %in% i)))
      } else {
        match <- match(i, dimnames)
        if (any(is.na(match))) {
          stop("subscript out of bounds (\"", paste0(i[is.na(match)], collapse = "\", \""), "\")")
        }
        return(match)
      }
    } else {
      warning("Your dimnames in dim=", dim, " contain duplicates! This might lead to erronous results",
        " and bad code performance. Please try to avoid duplicates in dimnames under all circumstances!")
    }
  }

  pmatch1 <- ifelse(pmatch == TRUE | pmatch == "right", "[^.]*", "")
  pmatch2 <- ifelse(pmatch == TRUE | pmatch == "left", "[^.]*", "")

  if (!is.list(i)) i <- list(i)
  elems <- seq_len(dim(x)[dim])
  if (!is.null(names(i))) {
    if (is.null(names(dimnames(x))[dim])) stop("subdimension does not exist (missing set names)!")
    nameOrder <- strsplit(names(dimnames(x))[dim], ".", fixed = TRUE)[[1]]
    if (!all(names(i) %in% nameOrder)) {
      stop("subdimension does not exist (\"", paste(names(i)[!(names(i) %in% nameOrder)], collapse = "\", \""), "\")")
    }
  }
  k <- 1
  for (j in i) {
    if (is.factor(j)) j <- as.character(j)
    if (!is.null(names(i))) {
      subdim <- which(nameOrder == names(i)[k])
      startpattern <- paste0("^", strrep("[^.]*\\.", subdim - 1))
    } else {
      startpattern <- "(^|\\.)"
    }
    tmp <- lapply(paste0(startpattern, pmatch1, escapeRegex(j), pmatch2, "(\\.|$)"),
      grep, dimnames[elems], perl = TRUE)
    if (any(vapply(tmp, length, length(tmp)) == 0)) {
      stop("subscript out of bounds (\"", paste(j[vapply(tmp, length, length(tmp)) == 0], collapse = "\", \""), "\")")
    }
    tmp <- unlist(tmp)
    if (invert) tmp <- setdiff(seq_along(elems), tmp)
    elems <- elems[tmp]
    k <- k + 1
  }
  return(elems)
}

#' @exportMethod [
setMethod("[", # nolint
  signature(x = "magpie"),
  function(x, i, j, k, ..., pmatch = FALSE, invert = FALSE, dim = NULL, drop = FALSE) {
    if (...length() > 0) {
      stop("unknown argument(s) supplied!")
    }
    if (!is.null(dim)) {
      if (!is.element(dim, 1:3)) stop("Invalid dim selection (allowed: 1, 2 or 3)")
      if (!missing(j) || (!missing(k) && !is.null(k) && k != FALSE)) {
        stop("Only single dimension selection allowed when dim is set!")
      }
      if (dim == 1) {
        return(x[i, , , drop = drop, pmatch = pmatch, invert = invert])
      } else if (dim == 2) {
        return(x[, i, , drop = drop, pmatch = pmatch, invert = invert])
      } else {
        return(x[, , i, drop = drop, pmatch = pmatch, invert = invert])
      }
    }
    if (is.null(dim(x))) return(x@.Data[i])
    if (!missing(i)) {
      if (is.data.frame(i)) return(.mselectDF(x, i))
      i <- .dimextract(x, i, 1, pmatch = pmatch, invert = invert)
    }
    if (!missing(j)) {
      .addY <- function(j, n) {
        if (is.list(j)) return(lapply(j, .addY, n))
        if (is.numeric(j) && any(j > n)) return(paste("y", j, sep = ""))
        return(j)
      }
      j <- .addY(j, dim(x)[2])
      if (is.null(j)) j <- seq_len(dim(x)[2])
      j <- .dimextract(x, j, 2, pmatch = pmatch, invert = invert)
    }
    if (!missing(k)) k <- .dimextract(x, k, 3, pmatch = pmatch, invert = invert)
    # if [] contains only 1 argument, the number of elements in sys.call(-1)
    # is 3. This indicates that it is a vectorised call of the form x[i]
    if (length(sys.call(-1)) == 3) {
      return(x@.Data[i])
    }
    x@.Data <- x@.Data[i, j, k, drop = FALSE]
    if (drop) x <- collapseNames(x)
    return(x)
  }
)

#' @exportMethod [<-
setMethod("[<-", # nolint
  signature(x = "magpie"),
  function(x, i, j, k, ..., pmatch = FALSE, dim = NULL, value) {
    if (...length() > 0) {
      stop("unknown argument(s) supplied!")
    }
    if (!is.null(dim)) {
      if (!is.element(dim, 1:3)) stop("Invalid dim selection (allowed: 1, 2 or 3)")
      if (!missing(j) || (!missing(k) && !missing(value))) {
        stop("Only single dimension selection allowed when dim is set!")
      }
      if (dim == 1) {
        x[i, , , pmatch = pmatch] <- value
      } else if (dim == 2) {
        x[, i, , pmatch = pmatch] <- value
      } else {
        x[, , i, pmatch = pmatch] <- value
      }
      return(x)
    }
    if (is.null(dim(x))) {
      tmp <- x@.Data
      tmp[i] <- k
      return(tmp)
    }
    if (!missing(i)) {
      if (is.factor(i)) i <- as.character(i)
      if (is.character(i) || is.list(i)) i <- .dimextract(x, i, 1, pmatch = pmatch)
    }
    if (!missing(j)) {
      if (is.factor(j)) j <- as.character(j)
      if (is.numeric(j) & any(j > dim(x)[2])) j <- paste("y", j, sep = "")
      else if (is.null(j)) j <- seq_len(dim(x)[2])
      else if ((is.character(j) || is.list(j)) && grepl(".", dimnames(x)[[2]][1], fixed = TRUE)) {
        j <- .dimextract(x, j, 2, pmatch = pmatch)
      }
    }
    if (!missing(k)) {
      if (is.factor(k)) k <- as.character(k)
      if (is.character(k) || is.list(k)) k <- .dimextract(x, k, 3, pmatch = pmatch)
    }
    # if [] contains only 1 argument, the number of elements in sys.call(-1)
    # is 4. This indicates that it is a vectorised call of the form x[i] <- value
    if (length(sys.call(which = -1)) == 4) {
      x@.Data[i] <- value
      return(x)
    }
    if (is.magpie(value)) {
      if (missing(i)) ii <- seq_len(dim(x)[1]) else ii <- i
      if (missing(j)) jj <- seq_len(dim(x)[2]) else jj <- j
      if (missing(k)) kk <- seq_len(dim(x)[3]) else kk <- k
      value <- magpie_expand(value, x[ii, jj, kk])
    } else if (length(value) != length(x@.Data[i, j, k]) && length(value) != 1) {
      # dangerous writing of value as order might be wrong!
      stop("Replacement does not work! Different replacement length!")
    } else if (length(value) != 1 && getOption("magclass.verbosity", default = 0) > 1) {
      message("NOTE ([<-): Dangerous replacement! As replacement value is not",
        " an MAgPIE object name checking is deactivated!\n")
    }
    x@.Data[i, j, k] <- value
    return(x)
  }
)

.isFALSE <- function(x) return(is.logical(x) && length(x) == 1 && !is.na(x) && !x)
