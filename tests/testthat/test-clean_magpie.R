
p <- maxample("pop")

test_that("clean_magpie works", {
  expect_identical(clean_magpie(p), p)
  expect_error(clean_magpie(p, what = "themess"), "Unknown setting")
  names(dimnames(p)) <- NULL
  expect_identical(getSets(clean_magpie(p)), c(d1.1 = "region", d2.1 = "year", d3.1 = "data"))
})
