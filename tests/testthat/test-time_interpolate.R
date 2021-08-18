p <- maxample("pop")
attr(p, "Metadata") <- NULL


test_that("time interpolate works", {
  expect_silent(p2 <- time_interpolate(p, "y2000", integrate = TRUE))
  expect_identical(p2[, 2000, , invert = TRUE], p)
  expect_true(all(p2[, 2000, ] - magpply(p[, c(1995, 2005), ], mean, DIM = 2) == 0))
  expect_silent(p3 <- time_interpolate(p, c(1980, 2000), integrate = TRUE, extrapolation_type = "constant"))
  expect_identical(p3[, c(1980, 2000), , invert = TRUE], p)
  expect_true(all(p3[, 1980, ] - setYears(p3[, 1995, ], NULL) == 0))
  expect_error(time_interpolate(1), "Invalid data format")
  expect_error(time_interpolate(as.magpie(1), interpolated_year = "blablub"), "not in the right format")
  expect_silent(p4 <- time_interpolate(p[, 1, ], c(1900, 2100)))
  expect_true(all(dimSums(p4, dim = 2) - 2 * p[, 1, ] == 0))
  expect_silent(p5 <- time_interpolate(p[, 1, ], c(1900, 1995, 2100)))
  expect_true(all(dimSums(p5, dim = 2) - 3 * p[, 1, ] == 0))
  expect_silent(p6 <- time_interpolate(p[, 1, ], c(1900, 1995, 2100), integrate = TRUE))
  expect_identical(p5, p6)
})

test_that("time interpolate can handle faulty data", {
  a <- maxample("animal")
  a <- dimSums(a, dim = c(2.2, 2.3))
  names(dimnames(a))[3] <- "bla.blub"
  expect_silent(out <- time_interpolate(a, interpolated_year = c(2012, 2013)))
})
