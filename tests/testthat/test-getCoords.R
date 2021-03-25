context("Coordinate handling test")


test_that("getCoords can read and write coordinates", {
  x <- maxample("pop")
  coords <- data.frame(x = 1:ncells(x) + 0.1, y = ncells(x):1 + 0.1)
  expect_error(getCoords(x), "No coordinates found")
  expect_silent(getCoords(x) <- coords)
  expect_identical(getCoords(x), coords)
  expect_identical(getItems(x,"x"), sub(".","p",coords$x, fixed = TRUE))
  coords2 <- data.frame(x = coords$y, y = coords$x)
  expect_silent(getCoords(x) <- coords2)
  expect_identical(getItems(x,"x"), sub(".","p",coords2$x, fixed = TRUE))
  expect_identical(getCoords(x), coords2)
  expect_silent(getCoords(x, xlab = "x2", ylab = "y2") <- coords)
  expect_identical(getItems(x,"y2"), sub(".","p",coords$y, fixed = TRUE))
  expect_identical(getCoords(x, xlab = "x2", ylab = "y2"), coords)
})


