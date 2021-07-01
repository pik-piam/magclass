context("dimCode Test")

x   <- maxample("animal")
pop <- maxample("pop")

test_that("dimension codes are correctly extracted", {
  expect_equivalent(dimCode(1, x), 1)
  expect_equivalent(dimCode(1.5, x), 1.5)
  expect_equivalent(dimCode(4.2, x), 0)
  expect_equivalent(dimCode(3.5, x, strict = FALSE), 3.5)
  expect_equivalent(dimCode(3.5, x, strict = TRUE), 0)
  expect_equivalent(dimCode("month", x), 2.2)
  expect_equivalent(dimCode("x", x), 1.1)
  expect_equivalent(dimCode("country", x), 1.3)
  expect_equivalent(dimCode(c("y", "year", "color"), x), c(1.2, 2.1, 3.3))
  expect_equivalent(dimCode(NULL, x), NULL)
  expect_equivalent(dimCode("t", pop), 2)
  expect_equivalent(dimCode("notavail", pop), 0)
})

test_that("illegal dim values are properly detected", {
  expect_error(dimCode("a.b", x), "separator must not be used in dimension name")
  getSets(pop) <- rep("same", 3)
  expect_error(dimCode("same", pop), "found more than once")
  getSets(x, fulldim = FALSE)[3] <- "species.species.color"
  expect_error(dimCode("species", x), "more than once")
  expect_error(dimCode(5, x, missing = "stop"), "illegal dimension")
})
