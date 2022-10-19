#' splits a data.frame or vector of strings with form 'variable (unit)' into
#' a data.frame with variable and unit separated
#'
#' @param x data.frame or vector of strings
#' @param col column name. Default: variable
#' @author Jan Philipp Dietrich, Mika Pflüger, Oliver Richters
#'
#' @export
unitsplit <- function(x, col = "variable") {
  # structure of this regex (flavor: PCRE2):
  # '^' start of the string
  # '(?P<varname>…)' a named capture group for the varname. It contains:
  #   '.*' greedy everything
  # ' ' separator: a space
  # '(?P<recurse>…)' a named capture group for recursion to match balanced parentheses in the unit. It contains:
  #   '\\(' a literal opening parenthesis
  #   '(?P<unit>…)' a named capture group for the unit. It contains:
  #      '(?>…)*' an atomic non-capturing group which is repeated zero or more times. It contains:
  #          `[^()|]` any character which is neither a parenthesis or the pipe symbol
  #          `|` or
  #          `(?P>recurse)` a match for the named capture group "recurse".
  #          As the named capture group "recurse" contains exactly one literal opening and closing parenthesis, this
  #          recursion makes sure that parentheses in the unit are always balanced - if a parenthesis needs to be
  #          matched, we have to repeat the whole "recurse" capture group, and therefore, we need exactly one literal
  #          opening and closing parenthesis (or another recursion).
  #   '\\)' a literal closing parenthesis
  # '$' end of the string
  # If you have to understand the regex, I can warmly recommend putting it into https://regex101.com/ or a similar
  # service. Just replace all '\\' by '\', double backslashes are an R thing.
  if (is.null(ncol(x))) x <- data.frame(variable = x)
  pattern <- "^(?P<varname>.*) (?P<recurse>\\((?P<unit>(?>[^()|]|(?P>recurse))*)\\))$"
  # Even though we use named capture groups in the pattern, we can't use names in the replacement, because sub() only
  # supports numbered replacements. \\1 is the named capture group "varname", \\3 is the named capture group "unit".
  # Note that if the pattern does not match (e.g. because the value in question does not contain a unit), sub will not
  # replace anything, effectively passing everything through unchanged to varName and unit. That's correct for varName,
  # but for the unit, we have to overwrite non-matches with "N/A".
  varName <- sub(pattern, "\\1", x[[col]], perl = TRUE)
  unit <- sub(pattern, "\\3", x[[col]], perl = TRUE)
  unit[grep(pattern, x[[col]], invert = TRUE, perl = TRUE)] <- "N/A"
  tmp <- data.frame(varName, unit, stringsAsFactors = FALSE)
  names(tmp) <- c(names(x[col]), "unit")
  x <- cbind(tmp, x[setdiff(names(x), names(x[col]))])
  return(x)
}
