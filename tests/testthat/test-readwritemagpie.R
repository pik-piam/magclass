test_that("read/write does not affect content", {
  mag <- maxample("pop")
  names(dimnames(mag)) <- NULL
  getNames(mag) <- c("A2-A", "B1-A")
  getComment(mag) <- "This is a comment"
  for (ext in c(".csv", ".cs3", ".cs3r", ".cs4", ".cs4r", ".cs5")) {
    tmpfile <- withr::local_tempfile(fileext = ext)
    write.magpie(mag, tmpfile)
    mag2 <- read.magpie(tmpfile)
    names(dimnames(mag2)) <- NULL
    expect_identical(names(dimnames(mag)), names(dimnames(mag2)))
    expect_equivalent(mag, mag2)
    unlink(tmpfile)
  }

  for (ext in c(".m", ".mz", ".rds")) {
    tmpfile <- withr::local_tempfile(fileext = ext)
    write.magpie(mag, tmpfile)
    mag2 <- read.magpie(tmpfile)
    expect_equivalent(mag, mag2)
    expect_identical(read.magpie(tmpfile, as.array = TRUE), as.array(read.magpie(tmpfile))[, , ])
    unlink(tmpfile)
  }

  bi <- maxample("bilateral")
  for (ext in c(".m", ".mz", ".rds", ".cs5")) {
    tmpfile <- withr::local_tempfile(fileext = ext)
    write.magpie(bi, tmpfile)
    bi2 <- round(read.magpie(tmpfile), 4)
    expect_equivalent(bi, bi2)
    unlink(tmpfile)
  }


  m2 <- mag
  getCells(m2) <- paste0("AFR.", 1:10)
  tmpfile <- withr::local_tempfile(fileext = ".cs3")
  expect_silent(write.magpie(m2, tmpfile))
  expect_silent(in2 <- read.magpie(tmpfile))
  names(dimnames(in2)) <- NULL
  attr(m2, "Metadata") <- NULL # nolint
  expect_equal(in2, m2)
  unlink(tmpfile)

  m3 <- setCells(mag[1, , ], "GLO")
  tmpfile <- withr::local_tempfile(fileext = ".cs2r")
  write.magpie(m3, tmpfile)
  mag3 <- read.magpie(tmpfile)
  names(dimnames(mag3)) <- NULL
  expect_equivalent(m3, mag3)
  unlink(tmpfile)

  expect_error(write.magpie(1), "Input is not in MAgPIE-format")

  mag <- maxample("pop")
  tmpfile <- withr::local_tempfile(fileext = ".cs5")
  write.magpie(mag, tmpfile)
  mag2 <- read.magpie(tmpfile)
  expect_identical(names(dimnames(mag)), names(dimnames(mag2)))
  unlink(tmpfile)
})

test_that("read supports older formats", {
  f1 <- system.file("extdata", "testdata", "oldformat1.m", package = "magclass")
  expect_silent(a <- read.magpie(f1))
  ref1 <- new("magpie", .Data = structure(552.666381835938, .Dim = c(1L, 1L, 1L),
                                          .Dimnames = list(i = "AFR", t = "y1995", scenario = "A2")))
  attr(ref1, "FileFormatVersion") <- 4 # nolint
  expect_equal(a, ref1)

  f2 <- system.file("extdata", "testdata", "oldformat2.mz", package = "magclass")
  expect_silent(a2 <- read.magpie(f2))
  ref2 <- new("magpie", .Data = structure(c(552.666381835938, 1280.63500976562), .Dim = c(2L, 1L, 1L),
                                          .Dimnames = list(i.j = c("AFR.1", "AFR.2"),
                                                           t = "y1995", scenario = "A2")))
  attr(ref2, "FileFormatVersion") <- 4 # nolint
  expect_equal(a2, ref2)
})

test_that("handling of spatial data works", {
  td <- tempdir()
  md <- magclass:::magclassdata$half_deg
  m05 <- new.magpie(paste0(md$region, ".", seq_len(dim(md)[1])), years = c(2000, 2001), fill = c(md$lon, md$lat))
  m10 <- mbind(m05, m05)
  getNames(m10) <- c("bla", "blub")
  expect_error(write.magpie(m10, file.path(td, "test.grd")), "no support for multiple variables")
  expect_no_warning(write.magpie(m10, file.path(td, "test.nc")))
  expect_silent(write.magpie(m05, file.path(td, "test.nc")))
  expect_silent(m05in <- read.magpie(file.path(td, "test.nc")))
  getCoords(m05) <- magclass:::magclassdata$half_deg[c("lon", "lat")]
  m05 <- collapseDim(m05, dim = c(1.1, 1.2))
  m05 <- m05[getItems(m05in, dim = 1), , ]
  getNames(m05in) <- NULL
  getSets(m05in, fulldim = FALSE)[3] <- "data"
  attr(m05in, ".internal.selfref")  <- NULL # nolint
  expect_identical(m05, m05in)

  a <- maxample("animal")
  a <- dimSums(a, dim = c(1.3, 1.4, 2, 3))
  write.magpie(a, file.path(td, "animal.asc"))
  copy.magpie(file.path(td, "animal.asc"), file.path(td, "animal.nc"))
  asc <- read.magpie(file.path(td, "animal.asc"))
  expect_silent(anc <- read.magpie(file.path(td, "animal.nc")))

  .clean <- function(x) {
    attr(x, ".internal.selfref")  <- NULL # nolint
    getItems(x, dim = 2) <- NULL
    getItems(x, dim = 3) <- NULL
    return(x)
  }
  expect_identical(.clean(asc), a)
  expect_identical(.clean(anc), a)
})


test_that("read/write conserves cell naming", {
  p <- new.magpie(c("AFR.2", "CPA.3", "AFR.1", "CPA.4"), fill = 0)
  tmpfile <- withr::local_tempfile(fileext = ".mz")
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

  tmpfile <- withr::local_tempfile(fileext = ".rds")
  saveRDS(1, tmpfile)
  expect_error(read.magpie(tmpfile), "does not contain a magpie object")
  unlink(tmpfile)

  expect_error(write.magpie(as.magpie(1), "bla.nc"), "No coordinates")
  expect_error(write.magpie(a, "blub.grd"), "no support for multiple variables")
  expect_error(write.magpie(a[, , 1], "blub.asc"), "choose just one")
})

test_that("copy.magpie works", {
  p <- maxample("pop")
  td <- tempdir()
  write.magpie(p, file.path(td, "test.mz"))
  expect_silent(copy.magpie(file.path(td, "test.mz"), file.path(td, "test2.mz")))
  expect_silent(copy.magpie(file.path(td, "test.mz"), file.path(td, "test3.cs3")))
  expect_silent(copy.magpie(file.path(td, "test.mz"), file.path(td, "test4.mz"), round = 0))
  expect_identical(read.magpie(file.path(td, "test.mz")), read.magpie(file.path(td, "test2.mz")))
  expect_equivalent(read.magpie(file.path(td, "test.mz")), read.magpie(file.path(td, "test3.cs3")))
  expect_identical(round(read.magpie(file.path(td, "test.mz"))), read.magpie(file.path(td, "test4.mz")))
})

test_that("edge cases work", {
  td <- tempdir()
  a <- new.magpie("GLO", 1:10, fill = 1)
  expect_silent(write.magpie(a, file.path(td, "test.csv")))
  expect_silent(b <- read.magpie(file.path(td, "test.csv")))
  expect_equivalent(a, b)
})
