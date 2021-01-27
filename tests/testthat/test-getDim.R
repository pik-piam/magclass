context("Dimension detection Test")

pop <- maxample("pop")

test_that("getDim matches whole strings", {
  expect_identical(getDim(c("AFR","CPA"), pop,  dimCode = FALSE),"i")
  expect_identical(getDim(c("AFR","CPA"), pop,  dimCode = TRUE),1.1)
  expect_identical(getDim(c("AF","CP"),   pop), numeric(0))
})


