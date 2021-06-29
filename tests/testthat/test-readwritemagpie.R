context("Read/Write Test")

test_that("read/write does not affect content", {
  mag <- maxample("pop")
  names(dimnames(mag)) <- NULL
  getNames(mag) <- c("A2-A", "B1-A")
  for (ext in c(".csv", ".cs3", ".cs4")) {
    tmpfile <- tempfile(fileext = ext)
    write.magpie(mag, tmpfile)
    mag2 <- read.magpie(tmpfile)
    names(dimnames(mag2)) <- NULL
    expect_equivalent(mag, mag2)
    unlink(tmpfile)
  }
  getComment(mag) <- "This is a comment"
  for (ext in c(".mz", ".rds")) {
    tmpfile <- tempfile(fileext = ext)
    write.magpie(mag, tmpfile)
    mag2 <- read.magpie(tmpfile)
    expect_equivalent(mag, mag2)
    expect_identical(read.magpie(tmpfile, as.array = TRUE), as.array(read.magpie(tmpfile))[, , ])
    unlink(tmpfile)
  }

  m2 <- mag
  getCells(m2) <- paste0("AFR.", 1:10)
  tmpfile <- tempfile(fileext = ".cs3")
  expect_silent(write.magpie(m2, tmpfile))
  expect_silent(in2 <- read.magpie(tmpfile))
  names(dimnames(in2)) <- NULL
  attr(m2, "Metadata") <- NULL
  expect_equal(in2, m2)
  unlink(tmpfile)
})

test_that("read supports older formats", {
  f1 <- system.file("extdata", "testdata", "oldformat1.m", package = "magclass")
  expect_silent(a <- read.magpie(f1))
  ref1 <- new("magpie", .Data = structure(552.666381835938, .Dim = c(1L, 1L, 1L),
                                          .Dimnames = list(i = "AFR", t = "y1995", scenario = "A2")))
  attr(ref1, "FileFormatVersion") <- 4
  expect_equal(a, ref1)

  f2 <- system.file("extdata", "testdata", "oldformat2.mz", package = "magclass")
  expect_silent(a2 <- read.magpie(f2))
  ref2 <- new("magpie", .Data = structure(c(552.666381835938, 1280.63500976562), .Dim = c(2L, 1L, 1L),
                                          .Dimnames = list(i.j = c("AFR.1", "AFR.2"),
                                                           t = "y1995", scenario = "A2")))
  attr(ref2, "FileFormatVersion") <- 4
  expect_equal(a2, ref2)
})


test_that("read/write conserves cell naming", {
  p <- new.magpie(c("AFR.2", "CPA.3", "AFR.1", "CPA.4"), fill = 0)
  tmpfile <- tempfile(fileext = ".mz")
  write.magpie(p, tmpfile)
  p2 <- read.magpie(tmpfile)
  expect_identical(getCells(p), getCells(p2))
})

.fakeFile <- function(filePath) {
  zz <- gzfile(filePath, "wb")
  fformatVersion <- 100
  writeBin(as.integer(fformatVersion), zz, size = 2)
  close(zz)
}

test_that("read/write triggers errors and warnings correctly", {
  a <- maxample("animal")
  expect_error(write.magpie(a, file.path(tempdir(), "tmp.cs3")), "cs3 does not support sparse data")
  expect_error(read.magpie("doesnotexist.csv"), "does not exist")
  path <- file.path(tempdir(), "exists.blub")
  writeLines("exists", path)
  expect_error(read.magpie(path), "Unknown file type")
  expect_error(read.magpie(file.path(tempdir(), "*.blub")), "Unknown file type")

  path <- file.path(tempdir(), "fake.mz")
  .fakeFile(path)
  expect_error(read.magpie(path), "File format is newer")
  unlink(path)

  path1 <- file.path(tempdir(), "animal1.rds")
  write.magpie(a, path1)
  path2 <- file.path(tempdir(), "animal2.rds")
  write.magpie(a, path2)
  expect_warning(read.magpie(file.path(tempdir(), "animal*.rds")), "ambiguous")
  unlink(path1)
  unlink(path2)

  tmpfile <- tempfile(fileext = ".rds")
  saveRDS(1, tmpfile)
  expect_error(read.magpie(tmpfile), "does not contain a magpie object")
  unlink(tmpfile)

})
