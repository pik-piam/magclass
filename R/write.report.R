#' Write file in report format
#'
#' This function writes the content of a MAgPIE object into a file or returns
#' it directly using the reporting format as it is used for many model
#' inter-comparisons.
#'
#' @param x MAgPIE object or a list of lists with MAgPIE objects as created by
#' read.report. In the latter case settings for model and scenario are
#' overwritten by the information given in the list.
#' @param file file name the object should be written to. If NULL the formatted
#' content is returned
#' @param model Name of the model which calculated the results
#' @param scenario The scenario which was used to get that results.
#' @param unit Unit of the data. Only relevant if unit is not already supplied
#' in Dimnames (format "name (unit)"). Can be either a single string or a
#' vector of strings with a length equal to the number of different data
#' elements in the MAgPIE object
#' @param ndigit Number of digits the output should have
#' @param append Logical which decides whether data should be added to an
#' existing file or an existing file should be overwritten
#' @param skipempty Determines whether empty entries (all data NA) should be
#' written to file or not.
#' @param extracols names of dimensions which should appear in the output as additional columns
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{read.report}}
#' @examples
#'
#' write.report(maxample("pop"))
#' @importFrom utils write.table
#' @export
write.report <- function(x, file = NULL, model = NULL, scenario = NULL, unit = NULL, ndigit = 4,
                         append = FALSE, skipempty = TRUE, extracols = NULL) {

  scenarioCall <- scenario
  modelCall <- model
  if (is.list(x)) {
    if (!is.list(x[[1]])) stop("Wrong format. x must be either a list of lists or a MAgPIE object!")
    if (is.null(file)) stop("file = NULL not supported for lists!")
    for (.scenario in names(x)) {
      for (.model in names(x[[.scenario]])) {
        scenario <- ifelse(is.null(scenarioCall), .scenario, scenario)
        model <- ifelse(is.null(modelCall), .model, model)
        write.report(x[[.scenario]][[.model]], file = file, model = model, scenario = scenario, unit = unit,
                     ndigit = ndigit, append = append, skipempty = skipempty, extracols = extracols)
        append <- TRUE
      }
    }
  } else {
    if (!is.magpie(x)) stop("Input is not a MAgPIE object!")
    x <- prepareData(x, model = model, scenario = scenario, unit = unit, skipempty = skipempty,
                     ndigit = ndigit, extracols = extracols)
    if (is.null(file)) return(x)
    if (!file.exists(file)) append <- FALSE
    if (append) {
      # check header for consistency
      header <- read.table(file, nrows = 1, sep = ";", stringsAsFactors = FALSE)
      years1 <- as.numeric(header[sapply(header, is.numeric)]) # nolint
      years2 <- as.numeric(colnames(x)[!is.na(suppressWarnings(as.numeric(colnames(x))))])
      union <- sort(union(years1, years2))
      addycols <- function(data, years) {
        ycols <- !is.na(suppressWarnings(as.numeric(colnames(data))))
        tmp <- data[ycols]
        data <- data[!ycols]
        data[as.character(sort(years))] <- "N/A"
        data[names(tmp)] <- tmp
        return(data)
      }
      if (length(union) > length(years1)) {
        data <- read.table(file, sep = ";", stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
        data <- data[-length(data)]
        write.table(addycols(data, union), file, quote = FALSE, sep = ";", row.names = FALSE,
                    col.names = TRUE, append = FALSE, eol = ";\n")
      }
      if (length(union) > length(years2)) {
        x <- addycols(as.data.frame(x), union)
      }
    }
    write.table(x, file, quote = FALSE, sep = ";", row.names = FALSE, col.names = !append,
                append = append, eol = ";\n")
  }
}

prepareData <- function(x, model = NULL, scenario = NULL, unit = NULL, skipempty = FALSE,
                        ndigit = 4, extracols = NULL) {
  sep <- "."
  # clean data
  x <- round(clean_magpie(x, what = "sets"), digits = ndigit)
  names(dimnames(x))[1] <- "Region"
  dimnames(x)[[1]] <- sub("^GLO(\\.{0,1}[0-9]*)$", "World\\1", dimnames(x)[[1]])
  dimnames(x)[[2]] <- substring(dimnames(x)[[2]], 2)

  # check for duplicates and possibly remove duplicates
  d <- duplicated(as.data.table(getNames(x)))
  if (any(d)) {
    warning("Data contains duplicate entries (", paste(getNames(x)[d], collapse = ", "),
            "), only first found entries will be written to file!")
    x <- x[, , which(!d)]
  }

  # convert to data.table, reshape and convert to data.frame
  x <- data.table::setDF(data.table::dcast(data.table::as.data.table(x, na.rm = skipempty),
                                           eval(parse(text = paste0("...~", names(dimnames(x))[2])))))

  # split data and dimension information
  data <- x[3:length(x)]
  x <- x[1:2]

  # split subdimensions
  colsplit <- function(x, col, sep = ".") {
    if (all(grepl(sep, x[[col]], fixed = TRUE, useBytes = TRUE))) {
      tmp <- data.frame(t(matrix(unlist(strsplit(as.character(x[[col]]), split = sep, fixed = TRUE, useBytes = TRUE)),
                                 ncol = length(x[[col]]))), stringsAsFactors = FALSE)
      names(tmp) <- strsplit(names(x)[col], split = sep, fixed = TRUE, useBytes = TRUE)[[1]]
      x <- cbind(tmp, x[setdiff(seq_len(ncol(x)), col)])
    }
    return(x)
  }
  for (i in grep(sep, names(x), fixed = TRUE, useBytes = TRUE)) x <- colsplit(x, i, sep = sep)

  for (i in seq_along(x)) {
    if (tolower(names(x)[i]) %in% c("scenario", "model", "region")) next
    if (class(x[[i]]) != "character") next
    if (!any(grepl(" \\(.*\\)$", x[[i]]))) next
    x <- unitsplit(x, i)
  }

  correctNames <- function(x, name = "Scenario", replacement = NULL) {
    if (is.null(replacement)) replacement <- "N/A"
    w <- which(tolower(names(x)) == tolower(name))
    if (length(w) == 0) {
      x <- cbind(replacement, x, stringsAsFactors = FALSE)
    } else if (length(w) == 1) {
      x <- cbind(x[w], x[-w], stringsAsFactors = FALSE)
    } else {
      warning("Found ", name, " more than once! First occurrence will be used")
      w <- w[1]
      x <- cbind(x[w], x[-w], stringsAsFactors = FALSE)
    }
    if (is.factor(x[[1]])) x[[1]] <- as.character(x[[1]])
    x[1][x[1] == "NA"] <- "N/A"
    names(x)[1] <- name
    return(x)
  }

  x <- correctNames(x, name = "Unit", replacement = unit)
  if (!is.null(extracols)) {
    for (i in extracols) {
      x <- correctNames(x, name = i)
    }
  }
  x <- correctNames(x, name = "Region", replacement = NULL)
  x <- correctNames(x, name = "Scenario", replacement = scenario)
  x <- correctNames(x, name = "Model", replacement = model)

  nxcol <- length(extracols)

  if (length(x) == (4 + nxcol)) {
    tmp <- "N/A"
  } else {
    tmp <- do.call(paste, c(x[(5 + nxcol):length(x)], sep = "."))
  }
  x <- cbind(x[1:(3 + nxcol)], Variable = tmp, x[4 + nxcol], stringsAsFactors = FALSE)

  data[is.na(data)] <- "N/A"
  x <- cbind(x, data)
  x <- x[do.call("order", x[c("Scenario", "Model", "Variable", "Region")]), ]
  return(x)
}

unitsplit <- function(x, col) {
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
  pattern <- "^(?P<varname>.*) (?P<recurse>\\((?P<unit>(?>[^()|]|(?P>recurse))*)\\))$"
  # Even though we use named capture groups in the pattern, we can't use names in the replacement, because sub() only
  # supports numbered replacements. \\1 is the named capture group "varname", \\3 is the named capture group "unit".
  # Note that if the pattern does not match (e.g. because the value in question does not contain a unit), sub will not
  # replace anything, effectively passing everything through unchanged to varName and unit. That's correct for varName,
  # but for the unit, we have to overwrite non-matches with "N/A".
  varName <- sub(pattern, "\\1", x[[col]], perl = TRUE)
  unit <- sub(pattern, "\\3", x[[col]], perl = TRUE)
  unit[grep(pattern, x[[col]], invert = TRUE, perl = TRUE)] <- "N/A"
  tmp <- data.frame(varName, unit)
  names(tmp) <- c(names(x)[col], "unit")
  x <- cbind(tmp, x[setdiff(seq_len(ncol(x)), col)])
  return(x)
}
