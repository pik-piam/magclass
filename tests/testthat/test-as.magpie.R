context("Conversion Test")

data("population_magpie")

test_that("conversions do not affect content", {
  mag <- population_magpie
  expect_identical(as.magpie(mag),mag)
  expect_equivalent(as.magpie(as.array(mag)),mag)
  expect_equivalent(as.magpie(as.data.frame(mag)),mag)
})

test_that("underscores are preserved", {
  mag2 <- population_magpie
  getCells(mag2) <- paste(getCells(mag2),1:ncells(mag2),sep="_")
  arr <- as.array(mag2)
  mag3 <- as.magpie(arr,spatial=1)
  expect_identical(getCells(mag3),getCells(mag2))
})