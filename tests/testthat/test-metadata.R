library(testthat)

context("Testing metadata handling")

cmd <- withMetadata()
on.exit(withMetadata(cmd))
withMetadata(TRUE)


test_that("Read/write metadata works", {
  tfile <- paste0(tempdir(),"/test.mz")
  write.magpie(population_magpie,tfile)
  a <- read.magpie(tfile)
  expect_identical(getMetadata(population_magpie),getMetadata(a))
})

withMetadata(cmd)