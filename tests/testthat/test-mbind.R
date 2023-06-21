a  <- maxample("animal")
p  <- maxample("pop")
attr(p, "Metadata") <- NULL # nolint: object_name_linter # nolint: linter_name_linter

test_that("mbind works", {
  expect_identical(mbind(p, p), p[, , c(1:2, 1:2)])
  expect_identical(mbind(p[, 1:3, ], p[, 4:16, ]), p)
  expect_identical(mbind(p[1:4, , ], p[5:10, , ]), p)
  expect_identical(mbind(p, NULL, p), mbind(list(p, p)))
  expect_identical(mbind(p, NULL, p), mbind(p, p))
  expect_null(mbind(NULL, NULL))

  expect_error(mbind(p, 12), "must all be MAgPIE-objects")
  expect_error(mbind(p[2:3, , ], p[c(1, 1), , ]), "occur more than once")
  expect_error(mbind(p[, 2:3, ], p[, c(1, 1), ]), "occur more than once")

  expect_error(mbind(a, p), "Cannot handle")
  expect_error(mbind(p[1:3, 2:4, ], p), "Cannot handle")
  expect_error(mbind(p[, 1:3, 1], p), "Cannot handle")
  expect_error(mbind(p[1:3, , 1], p), "Cannot handle")

  for (i in 1:3) {
    p0 <- p[1, dim = i]
    getItems(p0, dim = i) <- NULL
    expect_null(getItems(mbind(p0, p0), dim = i))
  }
})

test_that("mbind checks duplicated dimensions", {
  skip_if_not_installed("withr")

  namesA <- c("foo", "bar")
  namesB <- c("foo|bar", "foo|baz", "foo|+|baz", "foo|++|bar", "foobar")
  magpieA <- new.magpie(names = namesA)
  magpieB <- new.magpie(names = namesB)

  withr::with_options(
    # no checking of duplicates
    new = list(MAGPIE_MBIND_DUPLICATES = NULL),
    code = {
      expect_no_condition(
        object = mbind(magpieA, magpieA[, , namesA[-1]]))

      expect_no_condition(
        object = mbind(magpieB[, , namesB[-1]], magpieB[, , namesB[1]]))
    })

  withr::with_options(
    # duplicates generate a warning
    new = list(MAGPIE_MBIND_DUPLICATES = "warning"),
    code = {
      expect_warning(
        object = mbind(magpieA, magpieA[, , namesA[-1]]),
        regexp = "mbind introduced duplicated dimnames")

      expect_no_warning(
        object = mbind(magpieB[, , namesB[-1]], magpieB[, , namesB[1]]))
    })

  withr::with_options(
    # duplicates including pluses generate a warning
    new = list(MAGPIE_MBIND_DUPLICATES = "warning_remove-plus"),
    code = {
      expect_warning(
        object = mbind(magpieA, magpieA[, , namesA[-1]]),
        regexp = "mbind introduced duplicated dimnames")

      expect_warning(
        object = mbind(magpieB[, , namesB[-1]], magpieB[, , namesB[1]]),
        regexp = "mbind introduced duplicated dimnames")
    })

  withr::with_options(
    # duplicates generate an error
    new = list(MAGPIE_MBIND_DUPLICATES = "stop"),
    code = {
      expect_error(
        object = mbind(magpieA, magpieA[, , namesA[-1]]),
        regexp = "mbind would introduce duplicated dimnames")

      expect_no_error(
        object = mbind(magpieB[, , namesB[-1]], magpieB[, , namesB[1]]))
    })

  withr::with_options(
    # duplicates including pluses generate an error
    new = list(MAGPIE_MBIND_DUPLICATES = "stop_remove-plus"),
    code = {
      expect_error(
        object = mbind(magpieA, magpieA[, , namesA[-1]]),
        regexp = "mbind would introduce duplicated dimnames")

      expect_error(
        object = mbind(magpieB[, , namesB[-1]], magpieB[, , namesB[1]]),
        regexp = "mbind would introduce duplicated dimnames")
    })
})
