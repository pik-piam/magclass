p <- maxample("pop")
attr(p, "Metadata") <- NULL

test_that("read/write report works", {
  expect_error(read.report("bla"), "could not be found")
  f <- tempfile()
  getSets(p) <- c("region", "year", "scenario")
  expect_silent(write.report(p, f))
  expect_silent(p2 <- read.report(f, as.list = FALSE))
  p2 <- collapseDim(p2)
  expect_identical(round(p2, 4), round(p, 4))
  expect_silent(write.report(p, f, append = TRUE))
  expect_warning(p3 <- read.report(f, as.list = TRUE), "Duplicate entries")
  expect_identical(names(p3), c("A2", "B1"))
  expect_identical(names(p3[[1]]), "NA")

  d <- tempdir()
  getSets(p)[3] <- "variable"
  f1 <- file.path(d, "bla1.mif")
  f2 <- file.path(d, "bla2.mif")
  expect_silent(write.report(p, f1, scenario = "A"))
  expect_warning(write.report2(p, f2, scenario = "B"), "Deprecated")
  # capitalize header of f1
  tmp <- readLines(f1)
  tmp[1] <- toupper(tmp[1])
  writeLines(tmp, f1)
  expect_silent(p4 <- read.report(file.path(d, "bla*.mif")))
  expect_identical(names(p4), c("A", "B"))
  expect_identical(p4$A, p4$B)

  write.report(p[, 1:3, ], f)
  # remove header of f
  tmp <- readLines(f)
  writeLines(tmp[2:length(tmp)], f)
  expect_error(read.report(f), "No header given")

  write.report(p[, 1:11, ], f)
  # remove header of f
  tmp <- readLines(f)
  writeLines(tmp[2:length(tmp)], f)
  expect_warning(pguess <- read.report(f, as.list = FALSE), "Years are being guessed")
  years <- c(2005, seq(2010, 2100, 10))
  expect_equal(getYears(pguess, as.integer = TRUE), years)


  getNames(p)[1] <- c("A.2")
  write.report(p, f)
  expect_warning(p4 <- read.report(f, as.list = FALSE), "Replaced")
  expect_identical(getItems(p4, dim = 3), c("NA.NA.Ap2 (NA)", "NA.NA.B1 (NA)"))

  
  # test list format
  p <- maxample("pop")
  write.report(p, f1)
  pl <- read.report(f1, as.list = TRUE)
  write.report(pl, f2)
  pl2 <- read.report(f2, as.list = TRUE)
  expect_identical(pl, pl2)
  
  expect_error(write.report(as.list(1)), "Wrong format")
  expect_error(write.report(1), "not a MAgPIE object")
  expect_warning(write.report(p[,,rep(1,2)], f), "duplicate entries")
  
  unlink(c(f, f1, f2))
})
