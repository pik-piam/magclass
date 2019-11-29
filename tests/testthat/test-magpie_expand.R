library(testthat)

context("Expansion Test")

test_that("partial expansion", {
  skip("partial expansion not yet active by default")
  a <- new.magpie(c("AFR.1","AFR.2","EUR.1"),fill = 1)
  b <- new.magpie(c("AFR","EUR"),fill = 1)
  expect_identical(magpie_expand(b,a),a)
  d <- new.magpie(c("AFR.BLUB.1","AFR.BLUB.2","EUR.BLUB.1",
                    "AFR.BLA.1","AFR.BLA.2","EUR.BLA.1"),fill = 1)
  e <- new.magpie(c("BLA.AFR.A","BLA.EUR.A","BLUB.AFR.A","BLUB.EUR.A",
                    "BLA.AFR.B","BLA.EUR.B","BLUB.AFR.B","BLUB.EUR.B"),fill = 1)
  ee <- magpie_expand(e,d)
  expect_identical(ee,magpie_expand(d,ee))
})

