context("Dimension detection Test")

data("population_magpie")

test_that("getDim matches whole strings", {
  expect_identical(getDim(c("AFR","CPA"), population_magpie,dimCode=FALSE),"i")
  expect_identical(getDim(c("AFR","CPA"), population_magpie,dimCode=TRUE),1.1)
  expect_identical(getDim(c("AF","CP"),population_magpie),numeric(0))
})


