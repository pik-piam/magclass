p <- maxample("pop")
attr(p, "Metadata") <- NULL # nolint

test_that("read/write report works", {
  ref <- structure(list(Model = "N/A", Scenario = "N/A", Region = "World", Variable = "1",
                        Unit = "N/A", `1` = 1), row.names = 1L, class = "data.frame")
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
  f1 <- file.path(d, "bla1.mif")
  f2 <- file.path(d, "bla2.mif")
  getSets(p)[3] <- "variable"
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
  expect_error(write.report(list(list(as.magpie(1)))), "not supported for lists")
  expect_error(write.report(1), "not a MAgPIE object")
  expect_warning(write.report(p[, , rep(1, 2)], f), "duplicate entries")

  unlink(c(f, f1, f2))
})

test_that("append works", {
  f <- tempfile()
  expect_silent(write.report(p[, 1:3, ], f, model = "A", append = TRUE))
  expect_silent(write.report(p[, 1:2, ], f, model = "B", append = TRUE))
  expect_silent(write.report(p[, , ], f, model = "C", append = TRUE))
  expect_silent(r <- read.report(f, as.list = FALSE))
  expect_equal(dim(r), c(10, 16, 6))
  expect_true(all(is.na(r[, -1:-3, "A"])))
  expect_true(!anyNA(r[, 1:3, "A"]))
  expect_true(all(is.na(r[, -1:-2, "B"])))
  expect_true(!anyNA(r[, 1:2, "B"]))
  expect_true(!anyNA(r[, , "C"]))
  unlink(f)
})

test_that("multidim handling works", {
  p <- add_dimension(p, 3.2, "Scenario", "blub")
  expect_warning(r <- write.report(p[, 1:2, ]), "Found Scenario more than once")
  expect_identical(names(r), c("Model", "Scenario", "Region", "Variable", "Unit", "1995",
                               "2005"))
  getSets(p)[4] <- "Xtra"
  ref <- structure(list(Model = c("N/A", "N/A", "N/A", "N/A"),
                        Scenario = c("A2", "A2", "B1", "B1"),
                        Region = c("AFR", "CPA", "AFR", "CPA"),
                        Xtra = c("blub", "blub", "blub", "blub"),
                        Variable = c("N/A", "N/A", "N/A", "N/A"),
                        Unit = c("N/A", "N/A", "N/A", "N/A"),
                        `1995` = c(552.6664, 1280.635, 552.6664, 1280.635),
                        `2005` = c(696.44, 1429.53, 721.85, 1429.26)),
                   row.names = c(1L, 3L, 2L, 4L), class = "data.frame")
  expect_identical(write.report(p[1:2, 1:2, ], extracols = "Xtra"), ref)
  ref2 <- ref[-4]
  ref2$Variable <- "blub" # nolint
  expect_identical(write.report(p[1:2, 1:2, ]), ref2)
})

test_that("read/write report works with braces", {
  f <- tempfile()
  foo <- new.magpie("DEU", c(2015, 2020),
                    "Emissions|CO2|Energy|Demand|Transportation (w/ bunkers) (Mt CO2/yr)",
                    fill = 0)
  foo["DEU", 2020, "Emissions|CO2|Energy|Demand|Transportation (w/ bunkers) (Mt CO2/yr)"] <- 10
  expect_silent(write.report(foo, f))
  df <- utils::read.csv(f, sep = ";", stringsAsFactors = FALSE)
  expect_identical(df$Unit, "Mt CO2/yr")
})

test_that("write report does not crash with only NAs", {
  f <- tempfile()
  foo <- new.magpie("DEU", c(2015, 2020),
                    "Emissions|CO2|Energy|Demand|Transportation (w/ bunkers) (Mt CO2/yr)",
                    fill = NA)
  expect_identical(write.report(foo, f), data.table::data.table())
  expect_message(write.report(foo, f),
                 "magclass object contains only NAs, returning empty data table. No file was written.")
})
