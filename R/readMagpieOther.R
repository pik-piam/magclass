readMagpieOther <- function(fileName, fileType, comment.char = "*", check.names = FALSE) {  # nolint
  sep <- ifelse(fileType == "put", "\t", ",")
  # check for header
  temp <- utils::read.csv(fileName, nrow = 1, header = FALSE, sep = sep, comment.char = comment.char,
                          check.names = check.names, stringsAsFactors = TRUE)

  # check for numeric elements in first row, which means a missing header
  header <- !any(sapply(temp, is.numeric)) #nolint

  temp <- utils::read.csv(fileName, header = header, sep = sep, comment.char = comment.char,
                          check.names = check.names, stringsAsFactors = TRUE)

  # analyse column content
  coltypes <- rep("data", dim(temp)[2])
  for (column in seq_len(dim(temp)[2])) {
    if (sum(coltypes == "year") == 0 &&
          length(grep("^(y[0-9]{4}|[0-2][0-9]{3})$", temp[, column])) == dim(temp)[1]) {
      coltypes[column] <- "year"
    } else if (sum(coltypes == "region") == 0 && sum(coltypes == "regiospatial") == 0 &&
                 length(grep("^[A-Z]{3}$", temp[, column])) == dim(temp)[1]) {
      coltypes[column] <- "region"
    } else if (sum(coltypes == "regiospatial") == 0 && sum(coltypes == "region") == 0 &&
                 length(grep("^[A-Z]{3}_[0-9]+$", temp[, column])) == dim(temp)[1]) {
      coltypes[column] <- "regiospatial"
    } else if (!is.numeric(temp[1, column])) {
      coltypes[column] <- "other"
    } else if (sum(coltypes == "cell") == 0 && all(!is.na(temp[, column])) && all(temp[, column] != 0) &&
                 length(temp[, column]) %% max(temp[, column]) == 0 &&
                 suppressWarnings(try(all(unique(temp[, column]) == 1:max(temp[, column])), silent = TRUE) == TRUE)) {
      coltypes[column] <- "cell"
    }
  }

  if (any(coltypes == "year")) {
    temp <- temp[order(temp[, which(coltypes == "year")]), ]
    if (length(grep("y", temp[, which(coltypes == "year")])) == 0) {
      temp[, which(coltypes == "year")] <- as.factor(paste("y", temp[, which(coltypes == "year")], sep = ""))
    }
  }

  # backup check if cell column is really a cell column
  if (any(coltypes == "cell")) {
    if (dimnames(temp)[[2]][which(coltypes == "cell")] == "iteration") {
      temp[, which(coltypes == "cell")] <- paste("iter", format(temp[, which(coltypes == "cell")]), sep = "")
      coltypes[which(coltypes == "cell")] <- "other"
    } else if (header && !(dimnames(temp)[[2]][which(coltypes == "cell")] %in%
                             c("dummy", "dummy.1", "dummy.2", "dummy.3", "", " ",
                               "cell", "cells", "Cell", "Cells"))) {
      coltypes[which(coltypes == "cell")] <- "data"
    }
  }

  if (any(coltypes == "cell")) {
    ncells <- dim(temp)[1]
    if (any(coltypes == "year")) ncells <- ncells / length(unique(temp[, which(coltypes == "year")]))
    if (any(coltypes == "other")) ncells <- ncells / length(unique(temp[, which(coltypes == "other")]))
    if (!all(temp[1:ncells, which(coltypes == "cell")] == 1:ncells)) coltypes[which(coltypes == "cell")] <- "data"
  }

  if (all(coltypes != "data")) {
    if (coltypes[length(coltypes)] == "cell") coltypes[length(coltypes)] <- "data"
    else stop("No data column found! (coltypes: ", paste(coltypes, collapse = ", "))
  }

  if (sum(coltypes == "other") > 1) {
    stop("Invalid format. More than one \"other\" column is not allowed! (coltypes: ", paste(coltypes, collapse = ", "))
  }

  # set all coltypes after the first occurrence of "data" to "data"
  coltypes[min(which(coltypes == "data")):length(coltypes)] <- "data"

  # set first columntype from "cell" to "data" if it seems that the data set is just a vector of numbers
  if (all(coltypes == c("cell", rep("data", length(coltypes) - 1))) && dim(temp)[1] == 1) coltypes[1] <- "data"

  headertype <- "none"
  if (header) {
    if (length(grep("^y+[0-9]{4}$", dimnames(temp)[[2]][which(coltypes == "data")[1]])) == 1) {
      headertype <- "year"
    } else if (length(grep("^[A-Z]{3}$", dimnames(temp)[[2]][which(coltypes == "data")[1]])) == 1) {
      headertype <- "region"
    } else {
      headertype <- "other"
    }
  }

  if (any(coltypes == "other")) {
    othernames <- levels(as.factor(temp[, which(coltypes == "other")]))
    nother <- length(othernames)
    if (header) {
      if (headertype == "other") {
        elemnames <- dimnames(temp)[[2]][which(coltypes == "data")]
        elemnames <- paste(rep(othernames, each = length(elemnames)), elemnames, sep = ".")
      } else {
        elemnames <- othernames
      }
    } else {
      if (sum(coltypes == "data") == 1) {
        elemnames <- othernames
      } else {
        elemnames <- 1:sum(coltypes == "data")
        elemnames <- paste(rep(othernames, each = length(elemnames)), elemnames, sep = ".")
      }
    }
    ncols <- length(elemnames)
  } else {
    nother <- 1
    if (header) {
      if (headertype == "other") {
        elemnames <- dimnames(temp)[[2]][which(coltypes == "data")]
        ncols <- length(elemnames)
      } else {
        elemnames <- NULL
        ncols <- 1
      }
    } else {
      elemnames <- NULL
      ncols <- sum(coltypes == "data")
    }
  }


  if (any(coltypes == "year")) {
    yearnames <- levels(temp[, which(coltypes == "year")])
  } else if (headertype == "year") {
    yearnames <- dimnames(temp)[[2]][which(coltypes == "data")]
  } else {
    yearnames <- NULL
  }
  nyears <- max(1, length(yearnames))

  if (any(coltypes == "cell")) {
    ncells <- max(temp[, which(coltypes == "cell")])
  } else {
    if (headertype != "year") {
      ncells <- dim(temp)[1] / (nyears * nother)
    } else {
      ncells <- dim(temp)[1] / nother
    }
  }

  if (any(coltypes == "regiospatial")) {
    cellnames <- gsub("_", ".", temp[1:ncells, which(coltypes == "regiospatial")], fixed = TRUE)
  } else {
    if (any(coltypes == "region")) {
      tmpRegionnames <- levels(temp[, which(coltypes == "region")])
      regionnames <- tmpRegionnames[temp[, which(coltypes == "region")]]
      if (ncells == length(tmpRegionnames)) regionnames <- unique(regionnames)
    } else if (headertype == "region") {
      regionnames <- dimnames(temp)[[2]][which(coltypes == "data")]
      ncells <- ncells * length(regionnames)
    } else {
      regionnames <- "GLO"
    }
    if (length(unique(regionnames)) < length(regionnames)) {
      cellnames <- paste(regionnames, 1:ncells, sep = ".")
    } else {
      cellnames <- regionnames
    }
  }
  if (length(cellnames) == 1) cellnames <- list(cellnames)
  output <- array(NA, c(ncells, nyears, ncols))
  dimnames(output)[[1]] <- cellnames
  dimnames(output)[[2]] <- yearnames
  dimnames(output)[[3]] <- elemnames

  if (any(coltypes == "other") && (headertype == "other" || headertype == "none")) {
    counter <- 0
    for (other.elem in othernames) {
      output[, , (1:sum(coltypes == "data")) + counter] <- array(as.vector(
        as.matrix(temp[which(temp[, which(coltypes == "other")] == other.elem), which(coltypes == "data")])
      ), c(ncells, nyears, sum(coltypes == "data")))
      counter <- counter + sum(coltypes == "data")
    }
  } else if (!any(coltypes == "other") && headertype == "region") {
    for (i in seq_along(cellnames)) {
      output[i, , 1] <- temp[, which(coltypes == "data")[i]]
    }
  } else if (!any(coltypes == "other") && headertype == "year") {
    for (year in yearnames) {
      output[, year, 1] <- temp[, year]
    }
  } else if (any(coltypes == "other") && headertype == "region") {
    for (i in seq_along(cellnames)) {
      for (elem in elemnames) {
        output[i, , elem] <- temp[which(temp[, which(coltypes == "other")] == elem), which(coltypes == "data")[i]]
      }
    }
  } else if (any(coltypes == "other") && headertype == "year") {
    for (year in yearnames) {
      for (elem in elemnames) {
        output[, year, elem] <- temp[which(temp[, which(coltypes == "other")] == elem), year]
      }
    }
  } else {
    output[, , ] <- as.vector(as.matrix(temp[, which(coltypes == "data")]))
  }
  output <- as.magpie(output)
  attr(output, "comment") <- .readComment(fileName, commentChar = comment.char)
  return(output)
}
