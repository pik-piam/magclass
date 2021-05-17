
noC <- function(x) {
  getComment(x) <- NULL
  return(x)
}

test_that("ifelse method works correctly", {
  p <- maxample("pop")
  p2 <- p[10:1,,]
  expect_identical(noC(ifelse(p2 > 1000, p, 0)), noC(ifelse(p2 > 1000, p2, 0))) 
  expect_identical(noC(ifelse(p2 > 1000, p, 0)), noC(ifelse(p2 > 1000, p2, 0))) 
  expect_error(ifelse(p,1,9), "must only contain booleans")
  expect_error(ifelse(p > 0, Inf, 0), "Infinite values not supported")
  expect_error(ifelse(p > 0, NA, 0), "NA values not supported")
})
