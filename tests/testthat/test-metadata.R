library(testthat)

context("Testing metadata handling")

cmd <- withMetadata()
on.exit(withMetadata(cmd))
withMetadata(TRUE)


test_that("Read/write metadata works", {
  pop <- maxample("pop")
  tfile <- paste0(tempdir(),"/test.mz")
  write.magpie(pop,tfile)
  a <- read.magpie(tfile)
  expect_identical(getMetadata(pop),getMetadata(a))
})

withMetadata(cmd)