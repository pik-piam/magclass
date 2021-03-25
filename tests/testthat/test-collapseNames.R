context("Collapse Test")

test_that("arguments (collapsedim and preservedim) work in preserving and collapsing as specified dims", {
  x <- maxample("animal")
  expect_identical(x, collapseNames(x, preservedim = 1))
  expect_identical(collapseNames(x), collapseNames(x, collapsedim = 1))
})
