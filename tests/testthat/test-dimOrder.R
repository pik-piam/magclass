
a  <- maxample("animal")

test_that("subdimension permutation works", {
  expect_identical(dimOrder(a, perm = 3:1, dim = 1), dimOrder(a, perm = c(3:1, 4), dim = 1))
  expect_identical(dimOrder(a, perm = 1:4, dim = 1), a)
  expect_identical(dimOrder(a, perm = 1, dim = 1), a)
  expect_identical(dimOrder(a, perm = 1, dim = 3), a)
  expect_identical(getItems(dimOrder(a, perm = c(2, 1, 3), dim = 3), dim = 3)[1], "rabbit.animal.black")
})

test_that("edge cases in dimOrder work", {
  expect_error(dimOrder(a, perm = c(3, 100)), "Values of perm must be integers in the range from 1 to 3")
  expect_identical(getSets(dimOrder(a[NULL, , ], perm = 3:1, dim = 1), fulldim = FALSE)[1], "country.y.x.cell")
})
