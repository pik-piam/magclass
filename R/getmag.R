getmag <- function(x, e, dim) {
  if (dim == 1) {
    return(x[e, , ])
  } else if (dim == 2) {
    return(x[, e, ])
  } else if (dim == 3) {
    return(x[, , e])
  }
}

setmag <- function(x, e, dim, newData) {
  if (dim == 1) {
    x[e, , ] <- newData
  } else if (dim == 2) {
    x[, e, ] <- newData
  } else if (dim == 3) {
    x[, , e] <- newData
  }
  return(x)
}
