a <- maxample("animal")

test_that("getComment works", {
  x <- as.magpie(1)
  expect_silent(getComment(x) <- "this is a comment")
  expect_identical(getComment(x), "this is a comment")
})

test_that("is-methods work", {
  tf <- as.magpie(c(TRUE, FALSE))
  expect_identical(is.nan(as.magpie(c(NaN, 2))), tf)
  expect_identical(is.na(as.magpie(c(NA, 2))), tf)
  expect_identical(is.infinite(as.magpie(c(-Inf, 2))), tf)
  expect_identical(is.finite(as.magpie(c(2, Inf))), tf)
})

test_that("Incorrect inputs are properly detected", {
  expect_error(maxample("nothere"), "Unknown data set")
  expect_warning(magclass:::.duplicates_check(getCoords(a)[c(1, 1, 2:5), ]), "Duplicate entries")
})
