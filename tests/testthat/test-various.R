a <- maxample("animal")
attr(a, "Metadata") <- NULL

test_that("getComment works", {
  x <- as.magpie(1)
  expect_silent(getComment(x) <- "this is a comment")
  expect_identical(getComment(x), "this is a comment")
})

test_that("is-methods work", {
  tf <- as.magpie(c(TRUE, FALSE))
  expect_identical(is.nan(as.magpie(c(NaN, 2))), tf)
  expect_identical(is.na(as.magpie(c(NA, 2))), tf)
  expect_identical(is.infinite(as.magpie(c(-Inf, 2))), tf)
  expect_identical(is.finite(as.magpie(c(2, Inf))), tf)
})

test_that("Incorrect inputs are properly detected", {
  expect_error(maxample("nothere"), "Unknown data set")
  expect_warning(magclass:::.duplicates_check(getCoords(a)[c(1, 1, 2:5), ]), "Duplicate entries")
})

test_that("magpiesort works", {
  expect_error(magpiesort(1), "not a MAgPIE object")
  expect_identical(magpiesort(a[, dim(a)[2]:1, ]), magpiesort(a))
  expect_identical(getItems(magpiesort(a), dim = 1),
                   getItems(magpiesort(a[, , NULL]), dim = 1))
  expect_identical(getItems(magpiesort(a[1, , ]), dim = 2),
                   getItems(magpiesort(a[NULL, , ]), dim = 2))
})

test_that("unwrap works", {
  x <- as.magpie(array(1:6, c(3, 2), list(c("bla", "blub", "ble"), c("up", "down"))))
  ref <- structure(1:6, .Dim = c(1L, 1L, 3L, 2L),
                   .Dimnames = list("GLO", NULL, c("bla", "blub", "ble"), c("up", "down")))
  expect_identical(unwrap(x), ref)
  expect_warning(unwrap(x, sep = ":"), "not supported anymore")
  expect_error(unwrap(1), "not a MAgPIE object")
  expect_error(unwrap(x[, , c(1, 1)]), "Duplicated names detected")
  expect_error(unwrap(a), "needs to be complete")
})

