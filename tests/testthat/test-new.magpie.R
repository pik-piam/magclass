context("MAgPIE creation test")

test_that("magpie objects are created correctly", {
  reg <- c("AFR", "EUR")
  expect_silent(x <- new.magpie(reg, 1, 1))
  expect_identical(getItems(x, 1), reg)
  reg2 <- reg
  names(reg2) <- reg
  expect_silent(x <- new.magpie(reg2, 1, 1))
  expect_identical(getItems(x, 1), reg)

  expect_silent(o <- new.magpie(10:3, sort = TRUE))
  expect_identical(getItems(o, dim = 1), paste0("GLO.", 3:10))

})
