library(testthat)

context("Testing metadata handling")

cmd <- withMetadata()
on.exit(withMetadata(cmd))
withMetadata(TRUE)

animal <- maxample("animal")
pop    <- maxample("pop")

test_that("medata basics work", {
  expect_true(withMetadata())
  expect_true(is.list(getMetadata(pop)))
  expect_identical(getMetadata(pop,"user"),"jpd")
})

test_that("Read/write metadata works", {
  tfile <- paste0(tempdir(),"/test.mz")
  write.magpie(pop,tfile)
  a <- read.magpie(tfile)
  expect_identical(getMetadata(pop),getMetadata(a))
})

test_that("Unit calculations work", {
  skip_if_not_installed("units",minimum_version = "0.7.0")
  expect_identical(units(pop), install_magpie_units("1e+06 people"))
  expect_identical(units(pop + pop), install_magpie_units("1e+06 people"))
  expect_identical(units(pop * pop), install_magpie_units("1e+12 people^2"))
  pop2 <- pop
  units(pop2) <- "people"
  expect_identical(units(pop2), install_magpie_units("people"))
  expect_identical(as.vector(pop2[1,1,1]),as.vector(10^6*pop[1,1,1]))
  expect_equivalent(pop2 - pop, 0 * pop)
  expect_identical(units(animal), install_magpie_units("km^-2"))
  expect_identical(units(pop/animal), install_magpie_units("1e06 km^2*people"))
  expect_identical(units(pop/animal), install_magpie_units("1e06 people*km^2"))
})


withMetadata(cmd)