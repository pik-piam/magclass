.readComment <- function(fileName, commentChar = "*", metaChar = "#") {
  comment <- NULL
  if (!is.null(commentChar)) {
    if (commentChar != "") {
      zz <- file(fileName)
      open(zz)
      readRepeat <- TRUE
      while (readRepeat) {
        tmp <- readLines(zz, 1)
        if (length(grep(paste("^", escapeRegex(commentChar), sep = ""), tmp)) &
            !grepl(metaChar, substr(tmp, 3, 3), fixed = TRUE)) {
          comment <- c(comment, tmp)
        } else {
          readRepeat <- FALSE
        }
      }
      close(zz)
    }
  }
  return(substring(comment, 2))
}
