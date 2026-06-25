p <- maxample("pop")
a <- maxample("animal")

test_that("aperm.magpie returns a plain array, not a magpie", {
  expect_identical(class(aperm(p, c(2, 1, 3))), "array")
  expect_identical(class(aperm(p, c(1, 3, 2))), "array")
  expect_identical(class(aperm(p)), "array")
})

test_that("aperm.magpie result matches aperm on the underlying array", {
  expect_identical(aperm(p, c(2, 1, 3)), aperm(as.array(p), c(2, 1, 3)))
  expect_identical(aperm(p, c(3, 2, 1)), aperm(as.array(p), c(3, 2, 1)))
  expect_identical(aperm(p), aperm(as.array(p)))
  expect_identical(aperm(a, c(2, 1, 3)), aperm(as.array(a), c(2, 1, 3)))
})

test_that("apply() on magpie with NAs works without dispatching magpie's [ method", {
  x <- p
  x[1, 1, 1] <- NA
  # apply() internally calls aperm(); without aperm.magpie this would error with
  # "incorrect number of dimensions" because magpie's [,magpie-method expects 3D
  expect_no_error(apply(x, c(2, 3), sum, na.rm = TRUE))
  expect_no_error(apply(x, c(1, 3), sum, na.rm = TRUE))
  expect_no_error(apply(x, c(1, 2), sum, na.rm = TRUE))
})

test_that("apply() on magpie with Infs works without dispatching magpie's [ method", {
  x <- p
  x[1, 1, 1] <- Inf
  expect_no_error(apply(x, c(2, 3), sum))
  result <- apply(x, c(2, 3), sum)
  expect_true(is.matrix(result))
})
