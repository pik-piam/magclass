test_that("matchDim works", {

  mp <- new.magpie(
    cells_and_regions = c("AAA", "BBB", "CCC"),
    years = paste0("y", c(2000, 2005, 2010)),
    names = c("foo", "bar", "baz"),
    sets = c("region", "t", "name")
  )

  mp2 <- new.magpie(
    cells_and_regions = c("BBB", "CCC", "DDD"),
    years = paste0("y", c(2000, 2010, 2020)),
    names = c("foo", "bar", "qux"),
    sets = c("region", "t", "name")
  )

  # match all dimensions
  x <- matchDim(mp, mp2)
  expect_equal(getItems(x, dim = 1), c("BBB", "CCC", "DDD"))
  expect_equal(getItems(x, dim = 2), c("y2000", "y2010", "y2020"))
  expect_equal(getItems(x, dim = 3), c("foo", "bar", "qux"))

  # match only spatial
  x <- matchDim(mp, mp2, dim = 1)
  expect_equal(getItems(x, dim = 1), c("BBB", "CCC", "DDD"))
  expect_equal(getItems(x, dim = 2), c("y2000", "y2005", "y2010"))
  expect_equal(getItems(x, dim = 3), c("foo", "bar", "baz"))

  # match only temporal
  x <- matchDim(mp, mp2, dim = 2)
  expect_equal(getItems(x, dim = 1), c("AAA", "BBB", "CCC"))
  expect_equal(getItems(x, dim = 2), c("y2000", "y2010", "y2020"))
  expect_equal(getItems(x, dim = 3), c("foo", "bar", "baz"))

  # match only third
  x <- matchDim(mp, mp2, dim = 3)
  expect_equal(getItems(x, dim = 1), c("AAA", "BBB", "CCC"))
  expect_equal(getItems(x, dim = 2), c("y2000", "y2005", "y2010"))
  expect_equal(getItems(x, dim = 3), c("foo", "bar", "qux"))

  # reject objects with varying subdimensions
  mp3 <- add_dimension(mp2, dim = 1.2, add = "subregion", "yyy")
  expect_error(matchDim(mp, mp3, dim = 1),
               regexp = "magclass objects x and ref have different number of subdimensions in dimension 1")
  expect_no_error(matchDim(mp, mp3, dim = 2))

})
