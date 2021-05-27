
a  <- maxample("animal")
p  <- maxample("pop")

test_that("subdimension counting works", {
  expect_identical(ndim(a, 1), 4)
  expect_identical(ndim(a, 2), 3)
  expect_identical(ndim(a, 3), 3)
  expect_identical(ndim(a), 10)
  expect_identical(ndim(p, 1), 1)
  expect_identical(ndim(p), 3)

  expect_identical(ndim(a[NULL, , ], 1), 4)
  names(dimnames(a)) <- NULL
  expect_identical(ndim(a[NULL, , ], 1), 0)

  expect_error(ndim(a, "bla"), "Invalid dim selection")
  expect_error(ndim(a, 1.5), "Invalid dim selection")
  expect_error(ndim(a, 0), "Invalid dim selection")
  expect_error(ndim(a, 10), "Invalid dim selection")
})
