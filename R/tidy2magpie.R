tidy2magpie <- function(x, spatial = NULL, temporal = NULL) { #nolint
  # assumption: dataframe format in which only the very last
  #             column contains values!
  if ("data.frame" %in% class(x)) {
    class(x) <- "data.frame"
  } else {
    stop("Data does not seem to be a data.frame!")
  }
  sep <- "."

  for (i in seq_len(ncol(x))) {
    if (is.factor(x[[i]])) x[[i]] <- as.character(x[[i]])
  }

  if (is.null(colnames(x))) colnames(x) <- paste0("col", 1:dim(x)[2])
  if (anyNA(colnames(x))) colnames(x)[is.na(colnames(x))] <- "NA"
  colnames(x) <- make.unique(colnames(x), sep = "")

  if (dim(x)[1] == 0) return(copy.attributes(x, new.magpie(NULL)))

  if (is.null(spatial)) spatial  <- colnames(x[-length(x)])[apply(x[-length(x)], 2, is.spatial)]
  if (is.null(temporal)) temporal <- colnames(x[-length(x)])[apply(x[-length(x)], 2, is.temporal)]
  if (is.numeric(spatial)) spatial  <- colnames(x)[spatial]
  if (is.numeric(temporal)) temporal <- colnames(x)[temporal]

  .collapsecol <- function(x, which, sep = ".") {
    xname <- paste(colnames(x)[which], collapse = sep)
    args <- list()
    for (i in seq_along(which)) {
      args[[i]] <- x[, which[i]]
    }
    args["sep"] <- sep
    out <- as.data.frame(do.call(paste, args))
    colnames(out) <- xname
    return(out)
  }

  if (sum(colnames(x) %in% temporal) > 1) {
    t <- .collapsecol(x, which(colnames(x) %in% temporal), sep)
  } else if (sum(colnames(x) %in% temporal) == 1) {
    t <- x[, which(colnames(x) %in% temporal), drop = FALSE]
  } else {
    t <- data.frame(year = rep("NOTIME", dim(x)[1]))
  }
  t[[1]] <- as.character(t[[1]])

  if (sum(colnames(x) %in% spatial) > 1) {
    s <- .collapsecol(x, which(colnames(x) %in% spatial), sep)
  } else if (sum(colnames(x) %in% spatial) == 1) {
    s <- x[, which(colnames(x) %in% spatial), drop = FALSE]
  } else {
    s <- data.frame(region = rep("GLO", dim(x)[1]))
  }
  s[[1]] <- as.character(s[[1]])

  if (sum(!(colnames(x)[-dim(x)[2]] %in% c(temporal, spatial))) > 1) {
    d <- .collapsecol(x, which(!(colnames(x)[-dim(x)[2]] %in% c(temporal, spatial))), sep)
  } else if (sum(!(colnames(x)[-dim(x)[2]] %in% c(temporal, spatial))) == 1) {
    d <- x[, which(!(colnames(x)[-dim(x)[2]] %in% c(temporal, spatial))), drop = FALSE]
  } else {
    d <- data.frame(data = rep(tail(colnames(x), 1), dim(x)[1]))
  }
  d[[1]] <- as.character(d[[1]])

  uSpat <- as.character(unique(s[, 1]))
  uTemp <- as.character(unique(t[, 1]))
  uData <- as.character(unique(d[, 1]))
  dimnames <- list(uSpat, uTemp, uData)
  m <- array(dim = c(length(uSpat), length(uTemp), length(uData)), dimnames = dimnames)
  coord <- as.matrix(cbind(s, t, d))
  .duplicates_check(coord)
  m[coord] <- x[, dim(x)[2]]
  if (dim(m)[2] == 1) if (dimnames(m)[[2]] == "NOTIME") dimnames(m) <- list(dimnames(m)[[1]], NULL, dimnames(m)[[3]])
  if (dim(m)[3] == 1) if (dimnames(m)[[3]] == "NODATA") dimnames(m) <- list(dimnames(m)[[1]], dimnames(m)[[2]], NULL)

  names(dimnames(m)) <- c(names(s), names(t), names(d))
  m <- as.magpie(m, spatial = 1, temporal = 2)
  return(copy.attributes(x, m))
}
