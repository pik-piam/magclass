context("Set Manipulation Test")

test_that("sets names can be overwritten", {
  x <- new.magpie("GLO.REG", "y1995", "BLA.BLUB", sets = c("glo.reg", "year", "glo.data"))
  expect_identical(getSets(x), c("d1.1" = "glo", "d1.2" = "reg", "d2.1" = "year", "d3.1" = "glo", "d3.2" = "data"))
  getSets(x)[4] <- "bla"
  expect_identical(getSets(x), c("d1.1" = "glo", "d1.2" = "reg", "d2.1" = "year", "d3.1" = "bla", "d3.2" = "data"))
  getSets(x)["d1.2"] <- "region"
  expect_identical(getSets(x), c("d1.1" = "glo", "d1.2" = "region", "d2.1" = "year", "d3.1" = "bla", "d3.2" = "data"))
  x2 <- x
  expect_silent(getSets(x2) <- NULL)
  expect_identical(x, x2)
  expect_error(getSets(x)[6:7] <- 1:2, "length does not agree")
  names(dimnames(x)) <- "bla"
  expect_identical(getSets(x), c(d1.1 = "bla", d2.1 = NA, d3.1 = NA))
  getSets(x) <- c("", "bla", "blub")
  expect_identical(getSets(x), c(d1.1 = NA, d2.1 = "bla", d3.1 = "blub"))

  a <- maxample("animal")
  getSets(a, fulldim = FALSE)[3] <- "type.species"
  expect_silent(getSets(a)[3] <- "bla")
})

test_that("handling of faulty set names work", {
  a <- maxample("animal")
  b <- a
  getSets(b, fulldim = FALSE)[3] <- "x.y.z"
  getSets(b) <- getSets(a)
  expect_identical(b, a)

  getSets(b, fulldim = FALSE)[3] <- "x.y"
  uniqueSets <- make.unique(getSets(b), sep = "")
  getSets(b) <- uniqueSets
  expect_identical(unname(getSets(b)), c(uniqueSets, "data"))

  a <- maxample("animal")
  b <- a
  sets <- getSets(a)
  names(dimnames(b)) <- NULL
  expect_silent(getSets(b) <- sets)
  expect_identical(b, a)
})
