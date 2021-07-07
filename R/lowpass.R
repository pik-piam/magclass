#' Lowpass Filter
#'
#' Filters high frequencies out of a time series. The filter
#' has the structure x'(n) = (x(n-1)+2*x(n)+x(n+1))/4
#'
#'
#' @param x Vector of data points, that should be filtered or MAgPIE object
#' @param i number of iterations the filter should be applied to the data
#' @param fix Fixes the starting and/or ending data point. Default value is
#' \code{NULL} which doesn't fix any point. Available options are:
#' \code{"start"} for fixing the starting point, \code{"end"} for fixing the
#' ending point and \code{"both"} for fixing both ends of the data.
#' @param altFilter set special filter rule to indexes defined in this
#' parameter. The special filter has the structure x'(n) = (2*x(n)+x(n+1))/3
#' @param warn boolean deciding whether lowpass issues a warning for
#' critical parameter choices or not
#' @return The filtered data vector or MAgPIE object
#' @author Jan Philipp Dietrich, Misko Stevanovic
#' @examples
#'
#' lowpass(c(1, 2, 11, 3, 4))
#' # to fix the starting point
#' lowpass(c(0, 9, 1, 5, 14, 20, 6, 11, 0), i = 2, fix = "start")
#' @export
lowpass <- function(x, i = 1, fix = NULL, altFilter = NULL, warn = TRUE) { #nolint

  if (!is.null(fix) && !(fix %in% c("start", "end", "both"))) {
    stop("Option \"", fix, "\" is not available for the \"fix\" argunemt!")
  }
  if (warn && !is.null(fix)) {
    warning("Fixing start or end does modify the total sum of values! Use fix=NULL to let the total sum unchanged!")
  }

  if (i == 0) return(x)

  if (is.magpie(x)) {
    years <- getYears(x, as.integer = TRUE)
    if (is.unsorted(years)) {
      x <- x[, sort(years), ]
    }
    l <- nyears(x)
    x2 <- x
    x <- as.array(x)
    for (j in 1:i) {
      y <- x
      x[, 2:(l - 1), ] <- (y[, 1:(l - 2), ] + 2 * y[, 2:(l - 1), ] + y[, 3:l, ]) / 4
      if (!is.null(altFilter)) {
        x[, altFilter, ] <- (2 * y[, altFilter, ] + y[, altFilter + 1, ]) / 3
      }
      if (is.null(fix) || !(fix %in% c("start", "both"))) {
        x[, 1, ] <- (3 * y[, 1, ] + y[, 2, ]) / 4
      }
      if (is.null(fix) || !(fix %in% c("end", "both"))) {
        x[, l, ] <- (3 * y[, l, ] + y[, l - 1, ]) / 4
      }
    }
    x <- as.magpie(x)
    copy.attributes(x) <- x2
  } else {
    l <- length(x)
    for (j in 1:i) {
      y <- x
      x[2:(l - 1)] <- (y[1:(l - 2)] + 2 * y[2:(l - 1)] + y[3:l]) / 4
      if (!is.null(altFilter)) {
        x[altFilter] <- (2 * y[altFilter] + y[altFilter + 1]) / 3
      }
      if (is.null(fix) || !(fix %in% c("start", "both"))) {
        x[1] <- (3 * y[1] + y[2]) / 4
      }
      if (is.null(fix) || !(fix %in% c("end", "both"))) {
        x[l] <- (3 * y[l] + y[l - 1]) / 4
      }
    }
  }
  return(x)
}
