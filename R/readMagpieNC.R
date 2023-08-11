readMagpieNC <- function(filename) {
  x <- terra::sds(filename)
  x <- do.call(rbind, lapply(names(x), function(name) {
    years <- terra::time(x[name])
    a <- as.data.frame(x[name], na.rm = TRUE, xy = TRUE)
    a <- do.call(rbind, lapply(seq_along(years), function(j) {
      b <- a[, c(1:2, j + 2)] # always select x and y, plus a single-year column
      names(b)[3] <- "value"
      b["year"] <- years[j]
      return(b)
    }))
    a["data"] <- name
    return(a)
  }))
  x <- x[, c("x", "y", "year", "data", "value")]
  x$x <- sub("\\.", "p", x$x)
  x$y <- sub("\\.", "p", x$y)
  x <- tidy2magpie(x, spatial = c("x", "y"), temporal = "year")
  return(x)
}
