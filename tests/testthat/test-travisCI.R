library(testthat)

context("Version validation on Travis-CI")

test_that("Validation Key", {
  if(!identical(Sys.getenv("TRAVIS"), "true")) skip("Is only tested on Travis CI")
  expect_true(lucode:::validkey()$valid)
})