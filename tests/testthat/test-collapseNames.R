context("Collapse Test")

test_that("arguments (collapsedim and preservedim) work in preserving and collapsing as specified dims", {
  x <- maxample("animal")
  expect_identical(collapseNames(x, "type"), collapseDim(x, "type"))
  expect_identical(x, collapseNames(x, preservedim = 1))
  expect_identical(x, collapseNames(x, preservedim = "type"))
  expect_identical(collapseNames(x), collapseNames(x, collapsedim = 1))
  expect_identical(collapseNames(x, 3), collapseNames(x, 3.3))
  x <- x[, , 1]
  getNames(x) <- NULL
  expect_identical(x, collapseNames(x))
  expect_null(collapseNames(NULL))
})
