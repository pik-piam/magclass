#' dimCode
#'
#' Function converts a dimension name or number to a dimension Code used for
#' MAgPIE objects
#'
#'
#' @param dim A vector of dimension numbers or dimension names which should be
#' translated
#' @param x MAgPIE object in which the dimensions should be searched for.
#' @param missing Either a value to which a dimension should be set in case
#' that it is not found (default is 0), or "stop" indicating that the function
#' should throw an error in these cases.
#' @param strict if set to TRUE also properly set dimension names which refer
#' to non-existing subdimensions will be treated as missing, otherwise these
#' dimension codes will be returned, even if the subdimension does not exist
#' @param sep  A character separating joined dimension names
#' @return A dimension code identifying the dimension. Either a integer which
#' represents the main dimensions (1=spatial, 2=temporal, 3=data) or a numeric,
#' representing the subdimensions of a dimension (e.g. 3.2 for the second data
#' dimension).
#' @author Jan Philipp Dietrich, Kristine Karstens
#' @seealso \code{\link{mselect}}, \code{\link{getDim}}
#' @examples
#'
#' pop <- maxample("pop")
#' dimCode(c("t", "scenario", "blablub"), pop)
#' @export dimCode
dimCode <- function(dim, x, missing = 0, strict = FALSE, sep = ".") {

  if (all(is.character(dim)) && any(grepl(sep, dim, fixed = TRUE))) {
    stop("Dimension separator must not be used in dimension name!")
  }

  # function to translate dim to dim code
  if (is.character(dim)) {

    # get super dims and initialize dim number array
    set     <- getSets(x, fulldim = FALSE, sep = sep)
    dnames  <- dim
    dim     <- numeric()

    # loop over all entries for dim
    for (i in seq_along(dnames)) {

      # get superdim and check for appearance and uniqueness
      superdim    <- grep(paste0("(\\.|^)", dnames[i], "(\\.|$)"), set)

      if (length(superdim) > 1) {
        stop("One or more elements were found more than once in x!")
      } else if (length(superdim) == 0) {
        dim[i] <- 0
      } else {

        # split in subdims and calculate dimension Code
        tmp      <- unlist(strsplit(set[superdim], split = sep, fixed = TRUE))

        if (length(tmp) > 1) {
          subdim   <- grep(paste0("^", dnames[i], "($)"), tmp)
          if (length(subdim) > 1) stop("One or more elements were found more than once or not at all in x!")
        } else {
          subdim <- 0
        }

        dim[i]   <- as.numeric(superdim + subdim / 10)
      }
    }

    names(dim) <- dnames
  }

  if (strict) dim[!dimExists(dim, x)] <- 0

  # check for errors and set "missing"
  if (any(dim >= 4) | any(dim < 1)) {
    if (missing == "stop") {
      stop("illegal dimension. Use either dimension 1, 2, or 3, or address subdimensions via 3.1, 3.2, ...")
    }
    dim[dim >= 4 | dim < 1] <- missing
  }

  return(dim)
}
