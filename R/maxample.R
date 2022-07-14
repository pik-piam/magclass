#' maxample
#'
#' A collection of magclass example data sets
#'
#'
#' @param data name of the example data set. Currently available are "pop" (regional population data,
#' previously named "population_magpie"), "animal" (fictional, high-dimensional animal sighting data set)
#' and "bilateral" (fictional, bilateral trade cost data set).
#' @return the chosen example data set
#' @author Jan Philipp Dietrich
#' @examples
#'
#' p <- maxample("pop")
#' str(p)
#'
#' a <- maxample("animal")
#' str(a)
#' getItems(a, split = TRUE)
#' @export
maxample <- function(data) {
  if (!(data %in% c("pop", "animal", "bilateral"))) stop("Unknown data set \"", data, "\"!")
  .read <- function(data) return(readRDS(system.file(paste0("extdata/examples/", data, ".rds"), package = "magclass")))
  return(.read(data))
}
