
a <- maxample("animal")

test_that("getNames works", {
  expect_identical(getNames(a, dim = 2), c("rabbit", "bird", "dog"))
  expect_silent(getNames(a, dim = 2) <- c("hase", "vogel", "hund"))
  expect_identical(getNames(a), c("animal.hase.black", "animal.hase.white", "animal.vogel.black",
                                  "animal.vogel.red", "animal.hund.brown"))
  expect_identical(getNames(setNames(a, paste0("a.", 1:5))),  paste0("a.", 1:5))
  expect_error(getNames(a) <- NULL, "Cannot unset")
  expect_error(getNames(a) <- "a", "Wrong number of items")
  expect_silent(getNames(a[, , NULL]) <- character())
  expect_error(getNames(a, dim = 2:3) <- 1, "Unsupported dim selection")
  expect_error(getNames(a, dim = 5) <- 1, "Wrong number of items")
  expect_error(getNames(a, dim = "blub") <- 1, "does not exist")
  expect_silent(getNames(a, dim = "type") <- "tier")
  expect_identical(getNames(a, dim = "type"), "tier")

})
