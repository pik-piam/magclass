library(testthat)

context("Expansion Test")

mev <- getOption("magclass_expand_version")
msm <- getOption("magclass_setMatching")

on.exit({
  options(magclass_expand_version = mev)
  options(magclass_setMatching = msm)
})

test_that("partial expansion", {
  a <- new.magpie(c("AFR.1", "AFR.2", "EUR.1"), fill = 1)
  b <- new.magpie(c("AFR", "EUR"), fill = 1)

  options(magclass_expand_version = NULL)
  options(magclass_setMatching = TRUE)
  expect_identical(magpie_expand(b, a), a)
  options(magclass_setMatching = msm)
  expect_identical(magpie_expand(b, a), a)

  d <- new.magpie(c("AFR.BLUB.1", "AFR.BLUB.2", "EUR.BLUB.1",
    "AFR.BLA.1", "AFR.BLA.2", "EUR.BLA.1"), fill = 1)
  e <- new.magpie(c("BLA.AFR.A", "BLA.EUR.A", "BLUB.AFR.A", "BLUB.EUR.A",
    "BLA.AFR.B", "BLA.EUR.B", "BLUB.AFR.B", "BLUB.EUR.B"), fill = 1)
  ee <- magpie_expand(e, d)
  expect_identical(ee, magpie_expand(d, ee))
})

test_that("expansion with unusual set names", {
  d <- new.magpie(c("AFR.BLUB.1", "AFR.BLUB.2", "EUR.BLUB.1", "EUR.BLUB.2",
    "AFR.BLA.1", "AFR.BLA.2", "EUR.BLA.1", "EUR.BLA.2"), fill = 1)
  e <- new.magpie(c("BLA.AFR.A", "BLA.EUR.A", "BLUB.AFR.A", "BLUB.EUR.A",
    "BLA.AFR.B", "BLA.EUR.B", "BLUB.AFR.B", "BLUB.EUR.B"), fill = 1)

  # test for cases with missing set names
  names(dimnames(e)) <- NULL
  ee <- magpie_expand(e, d)
  expect_identical(ee, magpie_expand(d, ee))

  # test case with NA set name
  f <- new.magpie("GLO", 1900, "value", fill = 1)
  g <- new.magpie("GLO", 1900, "blub", fill = 1)
  names(dimnames(g))[3] <- NA
  ff <- magpie_expand(f, g)
  expect_identical(ff, magpie_expand(g, ff))

})

test_that("subdimension order", {
  # subdimension order should remain unchanged if possible
  names <- c("bla.value1", "bla.value2")
  h <- new.magpie("GLO", 1900, names, fill = 1)
  i <- new.magpie("GLO", 1900, c("value1", "value2"), fill = 1)
  expect_identical(getItems(magpie_expand(i, h), 3), names)

  x <- new.magpie("GLO", 2000, c("a", "b", "c"))
  y <- new.magpie("GLO", 2000, c("1", "2", "3"))
  xx <- magpie_expand(x, y)
  expect_identical(getItems(xx, 3), getItems(magpie_expand(y, xx), 3))
})

test_that("magpie_expand_dim works", {
  a <- maxample("animal")
  expect_error(magclass:::magpie_expand_dim(a, a, dim = 4), "Unsupported dim setting")
  expect_error(magclass:::magpie_expand_dim(a[, , 2:5], a, dim = 3), "Identical set names but different content")
  expect_identical(magclass:::magpie_expand_dim(collapseDim(a, "day"), a, dim = 2), a)
})
