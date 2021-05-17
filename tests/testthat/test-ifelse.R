
noC <- function(x) {
  getComment(x) <- NULL
  return(x)
}

test_that("ifelse method works correctly", {
  p <- maxample("pop")
  p[1,1,1] <- Inf
  p[2,1,1] <- -Inf
  p2 <- p[10:1,,]
  expect_identical(noC(ifelse(p2 > 1000, p, 0)), noC(ifelse(p2 > 1000, p2, 0))) 
  expect_error(ifelse(p,1,9), "must only contain booleans")
})
