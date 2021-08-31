
p <- maxample("pop")

test_that("clean_magpie works", {
  expect_identical(clean_magpie(p), p)
  expect_error(clean_magpie(p, what = "themess"), "Unknown setting")
  names(dimnames(p)) <- NULL
  expect_identical(getSets(clean_magpie(p)), c(d1.1 = "region", d2.1 = "year", d3.1 = "data"))
  p0 <- dimSums(maxample("pop"), dim = 1)
  expect_silent(clean_magpie(p0))
  p <- maxample("pop")
  names(dimnames(p)[[1]]) <- paste0("n", 1:10)
  expect_silent(clean_magpie(p))
})
