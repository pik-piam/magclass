#' print
#'
#' print method for MAgPIE objects for conventient display of magpie data.
#'
#' @param x MAgPIE object
#' @param drop argument which controls whether empty dimensions should be
#' skipped or not.
#' @param reshape argument that controls tabular representation of nested data
#' dimension cross tables, FALSE will reproduce standard print behavior
#' any pair of two dimension numbers will create a table for these two dims,
#' and loop over the other dimensions
#'
#' @param ... arguments to be passed to or from other methods.
#' @return Invisibly, the MAgPIE object x.
#' @author Jan Philipp Dietrich, Kristine Karstens, Felicitas Beier
#' @seealso \code{\link[base]{print}}
#' @examples
#'
#' pop <- maxample("pop")
#' print(pop)
#' print(pop[, 1, ], drop = FALSE)
#' print(pop[, 1, ])
#' @export
print.magpie <- function(x, drop = TRUE, reshape = FALSE, ...) {
  if (length(reshape) != 2 || !is.vector(reshape, mode = "numeric")) {
    reshape <- FALSE
  }
  if (any(!reshape)) {
    print(as.array(x)[, , , drop = drop], ...)
  } else if (drop) {
    dims <- as.numeric(substring(names(getSets(x)), 2))
    names(dims) <- getSets(x)
    reshape[reshape %% 1 == 0] <- reshape[reshape %% 1 == 0] + 0.1

    # check if specified subdims exists in x
    if (!all(reshape %in% dims)) {
      warning("Non-existing subdims specified in 'reshape'. Call print again with reshape=FALSE.")
      return(print.magpie(x, reshape = FALSE, drop = drop))
    }

    names(reshape) <- c(names(dims[dims == reshape[1]]), names(dims[dims == reshape[2]]))
    loop           <- setdiff(dims, reshape)
    names(loop)    <- setdiff(names(dims), names(reshape))

    header         <- array(dim = 3)
    names(header)  <- sapply(1:3, #nolint
                            function(dim, i) paste(names(dim[round(dim) == i]), collapse = "."), dim = loop)


    nestedLoop <- function(toPrint, missing, header) {
      i <- missing[1]
      missing <- tail(missing, length(missing) - 1)

      for (j in getItems(toPrint, dim = i)) {
        toPrintReduced <- toPrint[j, dim = floor(i)]
        if (is.na(header[trunc(i)])) {
          header[trunc(i)] <- j
        } else {
          header[trunc(i)] <- paste(c(header[trunc(i)], j), collapse = ".")
        }

        if (length(missing) != 0) {
          nestedLoop(toPrintReduced, missing, header)
        } else {
          header[is.na(header)] <- " "
          writeLines(paste(paste(names(header), collapse = ", "), " = ",
            paste(header,        collapse = ", ")), sep = "\n\n")
          writeLines(paste("\t", names(reshape[1])))
          print(reshape(subset(as.data.frame(toPrintReduced, rev = 2), select = c(names(reshape), ".value")),
                        timevar = names(reshape[1]), idvar = names(reshape[2]),
                        direction = "wide",
                        new.row.names = getItems(toPrintReduced, dim = reshape[2]),
                        varying = list(getItems(toPrintReduced, dim = reshape[1]))), row.names = FALSE)
          writeLines("")
        }

        if (grepl("\\.", header[trunc(i)])) {
          header[trunc(i)] <- gsub(".([^\\.]+$)", "", header[trunc(i)])
        } else {
          header[trunc(i)] <- NA
        }
      }
    }
    nestedLoop(x, loop, header)
  } else {
    warning("reshape option can just be used with drop=TRUE. Execute with reshape=FALSE:")
    print.magpie(x = x, drop = drop, reshape = FALSE, ...)
  }
  return(invisible(x))
}
