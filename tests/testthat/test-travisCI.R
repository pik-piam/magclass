context("Travis CI integration")
skip_if_not(identical(Sys.getenv("TRAVIS"), "true"))
library(testthat)
library(lucode)

test_that("Validation Key is correct (for Travis CI)", {
  expect_true(lucode:::validkey()$valid)
})