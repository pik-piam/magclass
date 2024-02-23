test_that("extend works", {
  animal <- maxample("animal")
  # shuffle first dim to ensure order does not matter
  animal <- animal[sample(seq_along(getItems(animal, 1))), , ]
  expect_identical(guessResolution(getCoords(animal)), 0.5)

  x <- extend(animal)
  expectedCoords <- expand.grid(x = seq(-179.75, 179.75, 0.5),
                                y = seq(89.75, -89.75, -0.5),
                                KEEP.OUT.ATTRS = FALSE)
  expect_identical(getCoords(x), expectedCoords)
  expect_identical(getSets(x), getSets(animal))
  expect_identical(getItems(x[getItems(animal, 1), , ], 1), getItems(animal, 1))

  expect_error(extend(animal, res = 1),
               "The coordinates of the input object are not a subset of the extended coordinates")

  x <- extend(animal, xRange = c(-3.25, 16.75), yRange = c(63.25, -59.75), res = 0.25)

  expectedCoords <- expand.grid(x = seq(-3.25, 16.75, 0.25),
                                y = seq(63.25, -59.75, -0.25),
                                KEEP.OUT.ATTRS = FALSE)
  expect_identical(getCoords(x), expectedCoords)

  # ensure there are no assumptions about position of subdims x and y in first dim
  animal2 <- dimOrder(animal, c(3, 4, 2, 1), dim = 1)
  expect_identical(names(dimnames(animal2))[1], "country.cell.y.x")
  x <- extend(animal2)
  expectedCoords <- expand.grid(x = seq(-179.75, 179.75, 0.5),
                                y = seq(89.75, -89.75, -0.5),
                                KEEP.OUT.ATTRS = FALSE)
  expect_identical(getCoords(x), expectedCoords)
})
