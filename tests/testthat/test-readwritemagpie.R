context("Read/Write Test")

test_that("read/write does not affect content", {
  mag <- maxample("pop")
  names(dimnames(mag)) <- NULL
  getNames(mag) <- c("A2-A","B1-A")
  for (ext in c(".csv",".cs3",".cs4")) {
    tmpfile <- tempfile(fileext = ext)
    write.magpie(mag,tmpfile)
    mag2 <- read.magpie(tmpfile)
    names(dimnames(mag2)) <- NULL
    expect_equivalent(mag,mag2)
  }
  mag <- maxample("pop")
  tmpfile <- tempfile(fileext = ".mz")
  write.magpie(mag,tmpfile)
  mag2 <- read.magpie(tmpfile)
  expect_equivalent(mag,mag2)
})


test_that("read/write conserves cell naming", {
  p <- new.magpie(c("AFR.2","CPA.3","AFR.1","CPA.4"), fill=0)
  tmpfile <- tempfile(fileext = ".mz")
  write.magpie(p,tmpfile)
  p2 <- read.magpie(tmpfile)
  expect_identical(getCells(p),getCells(p2))
})
