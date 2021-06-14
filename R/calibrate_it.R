#' calibrate_it
#'
#' Standardized functions to calibrate values to a certain baseyear.
#'
#'
#' @param origin Original Values (MAgPIE object)
#' @param cal_to Values to calibrate to (MAgPIE object).
#' @param cal_type "none" leaves the values as they are, "convergence" starts
#' from the aim values and then linearily converges towards the values of
#' origin, "growth_rate" uses the growth-rates of origin and applies them on
#' aim.
#' @param cal_year year on which the dataset should be calibrated.
#' @param end_year only for cal_type="convergence". Year in which the
#' calibration shall be faded out.
#' @param report_calibration_factors prints out the multipliers which are used
#' for calibration.
#' @return Calibrated dataset.
#' @author Benjamin Bodirsky
#' @seealso \code{\link{convergence}},\code{\link{lin.convergence}}
#' @examples
#'
#' pop <- maxample("pop")
#' test <- as.magpie(array(1000, dim(pop[, , "A2"]), dimnames(pop[, , "A2"])))
#' calibrate_it(origin = pop, cal_to = test[, "y1995", ], cal_type = "growth_rate")
#' calibrate_it(origin = pop, cal_to = test[, "y1995", ], cal_type = "convergence",
#'   cal_year = "y1995", end_year = "y2055")
#' calibrate_it(origin = pop, cal_to = test[, "y1995", ], cal_type = "none")
#' @export calibrate_it
calibrate_it <- function(origin, cal_to, cal_type = "convergence", cal_year = NULL, # nolint
                         end_year = NULL, report_calibration_factors = FALSE) {     # nolint

  if (cal_type == "convergence" & (is.null(cal_year) | is.null(end_year))) {
    stop("for convergence, cal_year and end_year is required")
  }
  if (!is.magpie(origin)) {
    stop("origin is no magpie object")
  }
  if (!is.magpie(cal_to)) {
    stop("cal_to is no magpie object")
  }
  if (!is.null(cal_year)) {
    if (cal_year %in% getYears(cal_to)) {
      cal_to <- cal_to[, cal_year, ] #nolint
    }
  }
  if (dim(cal_to)[[2]] != 1) {
    stop("cal_to has more timesteps than one.")
  }
  if ((is.null(cal_year)) & (dim(cal_to)[2] == 1)) {
    cal_year <- getYears(cal_to)     #nolint
    cal_to <- setNames(cal_to, NULL) #nolint
  }
  if ((!is.null(cal_year)) & (cal_year == getYears(cal_to))) {
    cal_to <- setNames(cal_to, NULL) #nolint
  }
  if (!is.null(getYears(cal_to)) & (cal_year != getYears(cal_to))) {
    stop("cal_year has to be in cal_to, or cal_to has to be NULL")
  }
  if (!is.null(getNames(cal_to)) & (!identical(getNames(origin), getNames(cal_to)))) {
    stop("names of cal_to has to be identical with origin or NULL")
  }
  calibrationFactor <- as.magpie(array(NA, dim(origin), dimnames(origin)))
  calibrationFactor[, , ] <- 1
  calibrated <- as.magpie(array(NA, dim(origin), dimnames(origin)))

  cal_to <- setYears(cal_to[, cal_year, ], NULL) #nolint

  if (cal_type == "none") {
    calibrated <- origin
    if (report_calibration_factors == TRUE) {
      print(1)
    }
  } else if (cal_type == "convergence") {
    calibrationFactor[, , ] <- cal_to[, , ] / setYears(origin[, cal_year, ], NULL)
    calibrationFactor <- convergence(origin = calibrationFactor, aim = 1, start_year = cal_year,
                                     end_year = end_year, direction = NULL, type = "linear")
    calibrated <- origin * calibrationFactor
    if (report_calibration_factors == TRUE) {
      print(calibrationFactor)
    }
  } else if (cal_type == "growth_rate") {
    calOrigin <- setYears(origin[, cal_year, ], NULL)
    calibrated[, , ]  <- origin[, , ] / calOrigin[, , ] * cal_to[, , ]
    if (report_calibration_factors == TRUE) {
      print(calOrigin[, , ] * cal_to[, , ])
    }
  } else {
    stop("unknown cal_type")
  }
  return(calibrated)
}
