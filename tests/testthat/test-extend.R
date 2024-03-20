test_that("extend works", {
  animal <- maxample("animal")
  # shuffle first dim to ensure order does not matter
  animal <- animal[sample(seq_along(getItems(animal, 1))), , ]
  animalCoords <- getCoords(animal)
  expect_identical(guessResolution(animal), 0.5)

  x <- extend(animal)
  expectedCoords <- expand.grid(x = seq(min(animalCoords$x), max(animalCoords$x), 0.5),
                                y = seq(max(animalCoords$y), min(animalCoords$y), -0.5),
                                KEEP.OUT.ATTRS = FALSE)
  expect_identical(getCoords(x), expectedCoords)
  expect_identical(getSets(x), getSets(animal))
  expect_identical(getItems(x[getItems(animal, 1), , ], 1), getItems(animal, 1))

  expect_error(extend(animal, gridDefinition = c(3.25, 6.75, 53.25, 49.75, 1)),
               "The coordinates of the input object are not a subset of the extended coordinates")

  x <- extend(animal, gridDefinition = c(3.25, 6.75, 53.25, 49.75, 1), crop = TRUE)
  expectedCoords <- expand.grid(x = seq(3.25, 6.75, 1),
                                y = seq(53.25, 49.75, -1),
                                KEEP.OUT.ATTRS = FALSE)
  expect_identical(getCoords(x), expectedCoords)

  x <- extend(animal, gridDefinition = c(-3.25, 16.75, 63.25, -59.75, 0.25))

  expectedCoords <- expand.grid(x = seq(-3.25, 16.75, 0.25),
                                y = seq(63.25, -59.75, -0.25),
                                KEEP.OUT.ATTRS = FALSE)
  expect_identical(getCoords(x), expectedCoords)

  # ensure there are no assumptions about position of subdims x and y in first dim
  animal2 <- dimOrder(animal, c(3, 4, 2, 1), dim = 1)
  expect_identical(names(dimnames(animal2))[1], "country.cell.y.x")
  x <- extend(animal2)
  expectedCoords <- expand.grid(x = seq(min(animalCoords$x), max(animalCoords$x), 0.5),
                                y = seq(max(animalCoords$y), min(animalCoords$y), -0.5),
                                KEEP.OUT.ATTRS = FALSE)
  expect_identical(getCoords(x), expectedCoords)
})
