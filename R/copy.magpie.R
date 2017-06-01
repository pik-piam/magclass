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
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{read.magpie}},\code{\link{write.magpie}}
#' @examples
#' 
#' # copy.magpie("bla.csv","blub.mz")
#' 
#' @export copy.magpie
copy.magpie <- function(input_file,output_file) {
  in_type  <- tail(strsplit(input_file,'\\.')[[1]],1)
  out_type <- tail(strsplit(output_file,'\\.')[[1]],1)
  if(in_type==out_type) {
    tmp <- file.copy(input_file, output_file, overwrite=TRUE) 
  } else {
    write.magpie(read.magpie(input_file), output_file)
  }
}
