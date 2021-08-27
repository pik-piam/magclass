#' Get cells per region
#'
#' Counts how often each element of the provided subdimension exists in the given
#' data set. Originally created to count the number of cells in a region (this
#' is also where its name originates from) it can now be used to count elements
#' of any subdimension via the dim argument.
#'
#' @param x MAgPIE object or a resolution written as numeric (currently only
#' data for 0.5 degree resolution is available).
#' @param dim Dimension for which the items should be returned. Either number or
#' name of dimension or a vector of these (in case of a vector all subimensions
#' must belong to the same main dimension!). See \code{\link{dimCode}} for more details.
#' @return cells per region
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getRegions}}, \code{\link{read.magpie}},
#' \code{\link{write.magpie}}
#' @examples
#'
#' getCPR(0.5)
#' a <- maxample("animal")
#' getCPR(a, dim = "color")
#' getCPR(a, dim = 3.2)
#' getCPR(a, dim = "country")
#' getCPR(a, dim = c("color", "species"))
#' @export

getCPR <- function(x, dim = 1.1) {
   if (is.magpie(x)) {
      dim <- dimCode(dim, x)
      if (length(dim) != 1 && length(unique(floor(dim))) > 1) {
         stop("Selected subdimensions must belong to the same main dimension!")
      }
      items <- getItems(x, dim = dim, full = TRUE)
   } else if (x == 0.5) {
      items <- magclassdata$half_deg$region
   } else {
      stop("Cannot extract cpr information for x!")
   }
   table <- table(items)
   names(dimnames(table)) <- NULL
   return(table)
}
