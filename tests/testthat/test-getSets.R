context("Set Manipulation Test")

test_that("sets names can be overwritten", {
  x <- new.magpie("GLO.REG","y1995","BLA.BLUB",sets=c("glo.reg","year","glo.data"))
  expect_identical(getSets(x),c("d1.1"="glo","d1.2"="reg","d2.1"="year","d3.1"="glo","d3.2"="data"))
  getSets(x)[4] <- "bla"
  expect_identical(getSets(x),c("d1.1"="glo","d1.2"="reg","d2.1"="year","d3.1"="bla","d3.2"="data"))
  getSets(x)["d1.2"] <- "region"
  expect_identical(getSets(x),c("d1.1"="glo","d1.2"="region","d2.1"="year","d3.1"="bla","d3.2"="data"))
})
