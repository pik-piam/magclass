context("Dimension detection Test")

pop <- maxample("pop")
animal <- maxample("animal")

test_that("getDim matches whole strings", {
  expect_identical(getDim(c("AFR","CPA"), pop,  dimCode = FALSE), "i")
  expect_identical(getDim(c("AFR","CPA"), pop,  dimCode = TRUE), 1.1)
  expect_identical(getDim(c("AF","CP"),   pop), numeric(0))
  expect_identical(getDim("bird", animal,  dimCode = FALSE), "species")
  expect_identical(getDim("bird", animal,  dimCode = TRUE), 3.2)
  expect_identical(getDim("april", animal,  dimCode = FALSE), "month")
  expect_identical(getDim("april", animal,  dimCode = TRUE), 2.2)
  expect_identical(getDim(c("AFR","CPA"), pop,  fullmatch = TRUE), numeric(0))
  expect_identical(getDim(getRegions(pop), pop,  fullmatch = TRUE), 1.1)
  
})


