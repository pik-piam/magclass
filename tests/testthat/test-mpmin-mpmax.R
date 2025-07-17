testMagpie <- function(regions = "AFR.1", years = "2000", names = "test", values = c(123), ...) {
  return(new.magpie(regions, years, names, values, ...))
}

expect_mpmin_result <- function(a, b, result) {
  mpminResult <- mpmin(a, b)
  expect_equal(!!result, !!mpminResult)
  return(mpminResult)
}

test_that("mpmin on simple magpie objects", {
  # Equal
  expect_mpmin_result(testMagpie(values = 123),
                      testMagpie(values = 123),
                      testMagpie(values = 123))

  # One is smaller
  expect_mpmin_result(testMagpie(values = 121),
                      testMagpie(values = 123),
                      testMagpie(values = 121))
})

test_that("variadic mpmin", {
  # Single argument is fine, but simply returns the same object
  expect_equal(mpmin(testMagpie()), testMagpie())

  # More than two arguments are also fine
  expect_equal(mpmin(testMagpie(values = 123),
                     testMagpie(values = 1),
                     testMagpie(values = 123)),
               testMagpie(values = 1))
})

test_that("mpmin on magpie objects with different orderings in one dimension", {
  expect_mpmin_result(testMagpie(regions = c("AFR.1", "AFR.2"), values = c(120, 345)),
                      testMagpie(regions = c("AFR.2", "AFR.1"), values = c(340, 123)),
                      testMagpie(regions = c("AFR.1", "AFR.2"),
                                 values = c(120, 340)))

  expect_mpmin_result(testMagpie(years = c("2000", "2001"), values = c(120, 345)),
                      testMagpie(years = c("2001", "2000"), values = c(340, 123)),
                      testMagpie(years = c("2000", "2001"), values = c(120, 340)))

  expect_mpmin_result(testMagpie(names = c("test1", "test2"), values = c(120, 345)),
                      testMagpie(names = c("test2", "test1"), values = c(340, 123)),
                      testMagpie(names = c("test1", "test2"), values = c(120, 340)))
})

test_that("mpmin on magpie objects with different orderings in all dimensions", {
  expect_mpmin_result(testMagpie(regions = c("AFR.1", "AFR.2"),
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
})

test_that("mpin special cases", {
  expect_error(mpmin(new.magpie(), 2), "mpmin expects two magpie objects")
  expect_error(mpmin(2, new.magpie()), "mpmin expects two magpie objects")

  expect_error(mpmin(testMagpie(regions = "A"),
                     testMagpie(regions = c("A", "B"))),
               "mpmin expects magpie objects with equal dimensions")
  expect_error(mpmin(testMagpie(regions = "A"),
                     testMagpie(regions = c("A", "B"))),
               "mpmin expects magpie objects with equal dimensions")
  expect_error(mpmin(testMagpie(years = "2000"),
                     testMagpie(years = c("2000", "2001"))),
               "mpmin expects magpie objects with equal dimensions")

  expect_error(mpmin(testMagpie(names = "A"),
                     testMagpie(names = c("A", "B"))),
               "mpmin expects magpie objects with equal dimensions")

  expect_error(mpmin(testMagpie(names = c("A", "B")),
                     testMagpie(names = c("C", "D"))),
               "mpmin expects magpie objects with equal items in dimensions")
})

test_that("mpmax simple test", {
  # As mpmin and mpmax use the same underlying function to rearrange data,
  # a basic test whether pmax works is sufficient.
  expect_equal(mpmax(testMagpie(values = 5), testMagpie(values = 10), testMagpie(values = 5)),
               testMagpie(values = 10))
})
