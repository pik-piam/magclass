#' escapeRegex
#' 
#' Escapes all symbols in a string which have a special meaning in regular
#' expressions.
#' 
#' 
#' @param x String or vector of strings that should be escaped.
#' @return The escaped strings.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link[base]{grep}}
escapeRegex <- function(x) 
{
    return(gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x))
}
