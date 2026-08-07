#' parallelExtremum
#'
#' (for internal use) Applies base::pmin/base::pmax to the underlying arrays of magpie
#' objects whose dimensions have already been aligned by \code{\link{withAlignedDims}}.
#' @details base::pmin/base::pmax compare and subassign their arguments directly. Handing
#' them magpie objects makes every one of those steps dispatch to the magclass Ops and
#' `[<-` methods, which realign the operands by name again for each comparison. Reducing
#' the plain arrays instead avoids that, and is roughly 80 times faster on large objects.
#' @param func base::pmin or base::pmax
#' @param na.rm Passed on to func
#' @param ... Magpie objects with identical dimensions, as returned by withAlignedDims
#' @author Patrick Rein
#' @keywords internal
parallelExtremum <- function(func, na.rm = FALSE, ...) { # nolint: object_name_linter.
  magpies <- list(...)
  out <- magpies[[1]]
  # base::pmin/pmax restore dim and dimnames from their first argument
  out@.Data <- do.call(func, c(lapply(magpies, slot, ".Data"), list(na.rm = na.rm)))
  return(out)
}
