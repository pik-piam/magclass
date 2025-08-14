testMagpie <- function(regions = "AFR.1", years = "y2000", names = "test", values = c(123), ...) {
  return(new.magpie(regions, years, names, values, ...))
}

expect_pmin_result <- function(a, b, result) { # nolint: object_name_linter. Mimics testthat style.
  pminResult <- pmin(a, b)
  expect_equal(!!result, !!pminResult)
  return(pminResult)
}

test_that("pmin on simple magpie objects", {
  # Equal
  expect_pmin_result(testMagpie(values = 123),
                     testMagpie(values = 123),
                     testMagpie(values = 123))

  # One is smaller
  expect_pmin_result(testMagpie(values = 121),
                     testMagpie(values = 123),
                     testMagpie(values = 121))
})

test_that("variadic pmin", {
  # Single argument is fine, but simply returns the same object
  expect_equal(pmin(testMagpie()), testMagpie())

  # More than two arguments are also fine
  expect_equal(pmin(testMagpie(values = 123),
                    testMagpie(values = 1),
                    testMagpie(values = 123)),
               testMagpie(values = 1))
})

test_that("pmin on magpie objects with different orderings in one dimension", {
  expect_pmin_result(testMagpie(regions = c("AFR.1", "AFR.2"), values = c(120, 345)),
                     testMagpie(regions = c("AFR.2", "AFR.1"), values = c(340, 123)),
                     testMagpie(regions = c("AFR.1", "AFR.2"),
                                values = c(120, 340)))

  expect_pmin_result(testMagpie(years = c("2000", "2001"), values = c(120, 345)),
                     testMagpie(years = c("2001", "2000"), values = c(340, 123)),
                     testMagpie(years = c("2000", "2001"), values = c(120, 340)))

  expect_pmin_result(testMagpie(names = c("test1", "test2"), values = c(120, 345)),
                     testMagpie(names = c("test2", "test1"), values = c(340, 123)),
                     testMagpie(names = c("test1", "test2"), values = c(120, 340)))
})

test_that("pmin on magpie objects with different orderings in all dimensions", {
  expect_pmin_result(testMagpie(regions = c("AFR.1", "AFR.2"),
                                years = c("2000", "2001"),
                                names = c("test1", "test2"),
                                values = c(1, 2, 3, 4, 5, 6, 7, 8)),
                     testMagpie(regions = c("AFR.2", "AFR.1"),
                                years = c("2001", "2000"),
                                names = c("test2", "test1"),
                                values = c(1, 2, 3, 4, 5, 6, 7, 8)),
                     testMagpie(regions = c("AFR.1", "AFR.2"),
                                years = c("2000", "2001"),
                                names = c("test1", "test2"),
                                values = c(1, 2, 3, 4, 4, 3, 2, 1)))

  # Sanity check: base::pmin fails in the same scenario as above
  expect_failure(expect_equal(base::pmin(testMagpie(regions = c("AFR.1", "AFR.2"),
                                                    years = c("2000", "2001"),
                                                    names = c("test1", "test2"),
                                                    values = c(1, 2, 3, 4, 5, 6, 7, 8)),
                                         testMagpie(regions = c("AFR.2", "AFR.1"),
                                                    years = c("2001", "2000"),
                                                    names = c("test2", "test1"),
                                                    values = c(1, 2, 3, 4, 5, 6, 7, 8))),
                              testMagpie(regions = c("AFR.1", "AFR.2"),
                                         years = c("2000", "2001"),
                                         names = c("test1", "test2"),
                                         values = c(1, 2, 3, 4, 4, 3, 2, 1))))
})

test_that("pmin special cases", {
  expect_pmin_result(testMagpie(years = 2001:2002, values = c(1, 3)), 2,
                     testMagpie(years = 2001:2002, values = c(1, 2)))
  expect_pmin_result(2, testMagpie(years = 2001:2002, values = c(1, 3)),
                     c(1, 2))

  expect_error(pmin(testMagpie(regions = "A"),
                    testMagpie(regions = c("A", "B"))),
               "pmin expects magpie objects with equal dimensions")
  expect_error(pmin(testMagpie(regions = "A"),
                    testMagpie(regions = c("A", "B"))),
               "pmin expects magpie objects with equal dimensions")
  expect_error(pmin(testMagpie(years = "2000"),
                    testMagpie(years = c("2000", "2001"))),
               "pmin expects magpie objects with equal dimensions")

  expect_error(pmin(testMagpie(names = "A"),
                    testMagpie(names = c("A", "B"))),
               "pmin expects magpie objects with equal dimensions")

  expect_error(pmin(testMagpie(names = c("A", "B")),
                    testMagpie(names = c("C", "D"))),
               "pmin expects magpie objects with the same items in all dimensions")
})

test_that("pmax simple test", {
  # As pmin and pmax use the same underlying function to rearrange data,
  # a basic test whether pmax works is sufficient.
  expect_equal(pmax(testMagpie(values = 5), testMagpie(values = 10), testMagpie(values = 5)),
               testMagpie(values = 10))
})
