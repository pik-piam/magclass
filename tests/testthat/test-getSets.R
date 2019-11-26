context("Set Manipulation Test")

test_that("sets names can be overwritten", {
  x <- new.magpie("GLO.REG","y1995","BLA.BLUB",sets=c("glo.reg","year","glo.data"))
  expect_identical(getSets(x),c("glo","reg","year","glo","data"))
  getSets(x)[4] <- "bla"
  expect_identical(getSets(x),c("glo","reg","year","bla","data"))
})
