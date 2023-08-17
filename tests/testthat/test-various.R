a <- maxample("animal")
attr(a, "Metadata") <- NULL  # nolint: object_name_linter.
p <- maxample("pop")
attr(p, "Metadata") <- NULL  # nolint: object_name_linter.


test_that("getComment works", {
  x <- as.magpie(1)
  expect_silent(getComment(x) <- "this is a comment")
  expect_identical(getComment(x), "this is a comment")
  expect_identical(getComment(setComment(x, "new comment")), "new comment")
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
  expect_identical(magpiesort(a[, dim(a)[2]:1, ]), magpiesort(a)) # nolint: seq_linter.
  expect_identical(getItems(magpiesort(a), dim = 1),
                   getItems(magpiesort(a[, , NULL]), dim = 1))
  expect_identical(getItems(magpiesort(a[1, , ]), dim = 2),
                   getItems(magpiesort(a[NULL, , ]), dim = 2))
  expect_identical(magpiesort(p), magpiesort(p[dim(p)[1]:1, , ])) # nolint: seq_linter.
})

test_that("(un)wrap works", {
  x <- as.magpie(array(1:6, c(3, 2), list(c("bla", "blub", "ble"), c("up", "down"))))
  ref <- structure(1:6, .Dim = c(1L, 1L, 3L, 2L),
                   .Dimnames = list("GLO", NULL, c("bla", "blub", "ble"), c("up", "down")))
  expect_identical(unwrap(x), ref)
  expect_warning(unwrap(x, sep = ":"), "not supported anymore")
  expect_error(unwrap(1), "not a MAgPIE object")
  expect_error(unwrap(x[, , c(1, 1)]), "Duplicated names detected")
  expect_error(unwrap(a), "needs to be complete")
  p0 <- p[, , 1]
  getItems(p0, dim = 3) <- NULL
  a0 <- as.array(p0)
  names(dimnames(a0)) <- NULL
  expect_identical(unwrap(p0), a0)
  expect_error(wrap(1), "not an array")
  expect_error(wrap(p0, map = 1), "not a list")
  expect_error(wrap(p0, map = list(1, 1)), "duplicated dimension indices")
  expect_error(wrap(p0, map = list(1:4)), "non-existing dimension")
  expect_error(wrap(p0, map = list(1)), "miss some dimensions")
  ref <- new("magpie", .Data = structure(c(AFR.y1995. = 553, CPA.y1995. = 1281),
                                         .Dim = 2L, .Dimnames = list(i.t.d3 = c("AFR.y1995.", "CPA.y1995."))))
  expect_identical(wrap(round(p0[1:2, 1, 1]), map = list(1:3)), ref)
  expect_identical(wrap(p0[1:2, 1, NULL]), new("magpie", .Data = structure(numeric(0), .Dim = 0L)))
})

test_that("head and tail work", {
  expect_identical(head(a, n2 = 2), a[1:3, 1:2, 1:2])
  expect_identical(tail(a, n2 = 2), a[dim(a)[1] - 2:0, dim(a)[2] - 1:0, dim(a)[3] - 1:0])
  expect_identical(tail(a[1, 1, 1]), a[1, 1, 1])
  expect_identical(head(a[1, 1, 1]), a[1, 1, 1])
})

test_that("getCells works", {
  expect_error(getCells(p) <- 12, "Wrong number of cell names supplied")
  p0a <- p0b <- p[NULL, , ]
  expect_silent(getCells(p0a) <- NULL)
  expect_identical(p0a, p0b)
})

test_that("getRegions works", {
  a <- collapseDim(a, dim = c("x", "y"))
  expect_identical(getRegions(a), c("NLD", "BEL", "LUX")) # nolint: undesirable_function_linter.
  expect_warning(getRegions(a) <- rep("BLA", ncells(a)), "deprecated") # nolint: undesirable_function_linter.
  expect_identical(getRegions(a), "BLA") # nolint: undesirable_function_linter.
  a0 <- dimSums(a, dim = 1)
  expect_null(getRegions(a0)) # nolint: undesirable_function_linter.
  expect_equal(nregions(a0), 1) # nolint: undesirable_function_linter.
})

test_that("getRegionList works", {
  a <- collapseDim(a, dim = c("x", "y"))
  expect_warning(rl <- getRegionList(a), "deprecated") # nolint: undesirable_function_linter.
  expect_identical(rl, as.factor(getItems(a, dim = "country", full = TRUE)))
  expect_error(suppressWarnings(getRegionList(a) <- "GLO"), # nolint: undesirable_function_linter.
               "Lengths of RegionLists do not agree")
  expect_warning(getRegionList(a) <- rep("GLO", dim(a)[1]), "deprecated") # nolint: undesirable_function_linter.
  expect_identical(suppressWarnings(getRegionList(a)), # nolint: undesirable_function_linter.
                   as.factor(rep("GLO", dim(a)[1])))
})

test_that("rounding works", {
  ref <- new("magpie", .Data = structure(c(552.67, 1280.64), .Dim = c(2L, 1L, 1L),
                                         .Dimnames = list(i = c("AFR", "CPA"), t = "y1995", scenario = "A2")))
  expect_identical(round(p[1:2, 1, 1], 2), ref)
})

test_that("isYear works", {
  expect_true(all(isYear(getYears(p, as.integer = TRUE), with_y = FALSE)))
  expect_true(all(isYear(getYears(p), with_y = TRUE)))
  expect_false(isYear("yabcd"))
  expect_false(isYear("y12345"))
  expect_false(isYear("abcd", with_y = FALSE))
  expect_false(isYear("12345", with_y = FALSE))
  expect_error(isYear(p), "is no Vector")
})

test_that("getYear works", {
  expect_error(getYears(p) <- 1999, "Wrong number of years")
  expect_silent(getYears(p[, -(1:nyears(p)), ]) <- NULL) # nolint
  p1 <- p[, 1, ]
  expect_silent(getYears(p1) <- NULL)
  expect_null(getYears(p1))
  expect_error(getYears(p) <- NULL, "Setting years to NULL is not possible")
  expect_error(getYears(p1) <- "x1000", "Wrong year format")
})

test_that("sizeCheck works", {
  limit <- getOption("magclass_sizeLimit")
  withr::defer(options(magclass_sizeLimit = limit))

  # passing magpie objects instead of magpie object dimensions
  expect_error(magclass:::sizeCheck(p),
               "Numeric vector of dimension sizes expected.")

  # passing magpie object dimensions
  expect_null(magclass:::sizeCheck(dim(p)))

  # passing too large magpie object dimensions
  options(magclass_sizeLimit = 1)
  expect_error(magclass:::sizeCheck(dim(p)),
               "magclass object size .* exceeds limit")
})

test_that("log methods work", {
  expect_identical(as.vector(logb(as.magpie(10), 10)), 1)
  expect_identical(as.vector(log(as.magpie(10), 10)), 1)
})
