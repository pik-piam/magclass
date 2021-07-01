
a <- maxample("animal")

test_that("getNames works", {
  expect_identical(getNames(a, dim = 2), c("rabbit", "bird", "dog"))
  expect_silent(getNames(a, dim = 2) <- c("hase", "vogel", "hund"))
  expect_identical(getNames(a), c("animal.hase.black", "animal.hase.white", "animal.vogel.black", 
                                  "animal.vogel.red", "animal.hund.brown"))
  expect_error(getNames(a) <- NULL, "not possible")
  expect_error(getNames(a) <- "a", "Wrong number of data names")
  expect_silent(getNames(a[,,NULL]) <- character())
  expect_error(getNames(a) <- c("one","two","thr.ee","four","five"), "Inconsistent names")
  expect_error(getNames(a, dim = 2:3) <- 1, "Only a single dimension can be chosen")
  expect_error(getNames(a, dim = 5) <- 1, "does not exist")
  expect_error(getNames(a, dim = "blub") <- 1, "does not exist")
  expect_silent(getNames(a, dim = "type") <- "tier")
  expect_identical(getNames(a, dim = "type"), "tier")

})
