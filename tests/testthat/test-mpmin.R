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

test_that("mpin error handling", {
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
  expect_error(mpmin(testMagpie(regions = c("A.1", "B.1"), sets = c("region.cell", "year", "scenario")),
                     testMagpie(regions = c("1.A", "1.B"), sets = c("cell.region", "year", "scenario"))),
               "mpmin expects magpie objects with equal items in dimensions")

  # TODO: Unclear yet whether this should throw an error or not
  # This is especially important for data subdimensions
  #
  # expect_error(mpmin(testMagpie(regions = "AFR.1", sets = c("region.cell", "year", "scenario")),
  #                    testMagpie(regions = "AFR.1", sets = c("region.countryIndex", "year", "scenario"))),
  #              "mpmin expects magpie objects with equal items in dimensions")
  # expect_error(mpmin(testMagpie(names = c("test.1", "test.2"), sets = c("region", "year", "scenario.element"),
  #                               values = c(1, 2)),
  #                    testMagpie(names = c("test.1", "test.2"), sets = c("region", "year", "scenario.sub"),
  #                               values = c(1, 2))),
  #              "mpmin expects magpie objects with equal items in dimensions")
})
