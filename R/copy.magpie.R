#' Copy MAgPIE-files
#'
#' This function copies MAgPIE-files from one location to another. During the
#' copying it is also possible to change the file type (e.g. from 'mz' to
#' 'csv')
#'
#'
#' @aliases copy.magpie copy.magpie
#' @param input_file file, that should be copied
#' @param output_file copy destination
#' @param round number of digits the values should be rounded. NULL means no rounding
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{read.magpie}},\code{\link{write.magpie}}
#' @examples
#'
#' # copy.magpie("bla.csv","blub.mz")
#' @export copy.magpie
copy.magpie <- function(input_file, output_file, round = NULL) { #nolint
  inType  <- tail(strsplit(input_file, "\\.")[[1]], 1)
  outType <- tail(strsplit(output_file, "\\.")[[1]], 1)
  if (inType == outType && is.null(round)) {
    file.copy(input_file, output_file, overwrite = TRUE)
  } else if (is.null(round)) {
    write.magpie(read.magpie(input_file), output_file)
  } else {
    write.magpie(round(read.magpie(input_file), round), output_file)
  }
}
