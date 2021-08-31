#' convergence
#'
#' Cross-Fades the values of one MAGPIE object into the values of another over
#' a certain time
#'
#'
#' @param origin an object with one name-column
#' @param aim Can be twofold: An magpie object or a numeric value.
#' @param start_year year in which the convergence from origin to aim starts.
#' If set to NULL the the first year of aim is used as start_year
#' @param end_year year in which the convergence from origin to aim shall be
#' (nearly) reached. If set to NULL the the last year of aim is used as
#' end_year.
#' @param direction NULL, "up" or "down". NULL means normal convergence in both
#' directions, "up" is only a convergence if origin<aim, "down" means only a
#' convergence if origin>aim
#' @param type "smooth", "s", "linear" or "decay". Describes the type of
#' convergence: linear means a linear conversion , s is an s-curve which starts
#' from origin in start_year and reaches aim precisely in end_year. After 50
#' percent of the convergence time, it reaches about the middle of the two
#' values. Its based on the function min(1, pos^4/(0.07+pos^4)*1.07) smooth is
#' a conversion based on the function x^3/(0.1+x^3).  In the latter case only
#' 90\% of convergence will be reached in the end year, because full
#' convergence is reached in infinity.  decay is a conversion based on the
#' function x/(1.5 + x)*2.5.
#' @param par parameter value for convergence function; currently only used for
#' type="decay"
#' @return returns a time-series with the same timesteps as origin, which
#' lineary fades into the values of the aim object
#' @author Benjamin Bodirsky, Jan Philipp Dietrich
#' @examples
#'
#' pop <- maxample("pop")
#' population <- add_columns(pop, "MIX")
#' population[, , "MIX"] <- convergence(population[, , "A2"], population[, , "B1"])
#' @export convergence
convergence <- function(origin, aim, start_year = NULL, end_year = NULL, # nolint
                        direction = NULL, type = "smooth", par = 1.5) {

  ### Basic checks ###
  if (!is.magpie(origin)) stop("origin is no magpie object")

  if (is.null(dim(aim))) aim <- as.magpie(array(aim, dim(origin), dimnames(origin)),
                                          spatial = 1, temporal = 2)
  if (!is.magpie(aim)) stop("aim is no magpie object")

  if (!identical(dimnames(aim)[[1]], dimnames(origin)[[1]])) stop("regions have to be the same")

  if (ndata(origin) != 1 & !identical(getNames(origin), getNames(aim))) {
    stop("If there ist more than one name-column, dimnames have to be the same")
  }

  .yearFill <- function(x, ref) {
    if (nyears(x) == 1) {
      tmp <- setYears(x, NULL)
      x <- ref
      x[, , ] <- tmp
    }
    return(x)
  }
  aim    <- .yearFill(aim, origin)
  origin <- .yearFill(origin, aim)

  if (!identical(getYears(aim), getYears(origin))) {
    stop("Objects need the same timesteps, or aim has to have only one timestep")
  }

  .prepYear <- function(year, refyear) {
    if (is.null(year)) year <- refyear
    if (isYear(year, with_y = TRUE)) year <- substr(year, 2, 5)
    year <- as.numeric(year)
    if (!isYear(year, with_y = FALSE)) stop("wrong year format for convergence aim")
    return(year)
  }

  startYear <- .prepYear(start_year, getYears(aim)[1])
  endYear   <- .prepYear(end_year,   getYears(aim)[nyears(aim)])

  # In the case of direction up or down data should only be manipulated in one
  # direction. Therefor, the aim object is manipulated accordingly
  if (!is.null(direction)) {
    if (direction == "up") aim[aim < origin] <- origin[aim < origin]
    else if (direction == "down") aim[aim > origin] <- origin[aim > origin]
    else stop("Illegal direction setting, only up and down are allowed arguments!")
  }

  years <- new.magpie("GLO", getYears(origin), NULL, getYears(origin, as.integer = TRUE))
  pos <- (years - startYear) / (endYear - startYear)
  pos[pos < 0] <- 0
  pos[pos > 1] <- 1
  if (type == "linear") {
    mix <- pos
  } else if (type == "s") {
    mix <- pos^4 / (0.07 + pos^4) * 1.07
  } else if (type == "smooth") {
    mix <- pos^3 / (0.1 + pos^3)
  } else if (type == "decay") {
    mix <- pos / (par + pos) * (par + 1) # nolint
  } else {
    stop("type does not exist")
  }
  converged <- aim * mix + origin * (1 - mix)

  return(converged)
}
