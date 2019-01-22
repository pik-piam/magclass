library(testthat)
library(lucode)

test_that("Validation Key is correct (for Travis CI)", {
  expect_true(lucode:::validkey()$valid)
})