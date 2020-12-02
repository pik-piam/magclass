library(testthat)

context("Subsetting test")

p <- population_magpie
a <- as.array(p)

test_that("single element subsetting works", {
  expect_identical(p[11],a[11])
  expect_identical(p[3], a[3])
})

test_that("multi element subsetting works", {
  expect_equivalent(as.array(p[3,,]),                a[3,,,drop=FALSE])
  expect_equivalent(as.array(p["FSU",,]),            a[4,,,drop=FALSE])
  expect_equivalent(as.array(p[,2005,]),             a[,2,,drop=FALSE])
  expect_equivalent(as.array(p["PAS",,"B1"]),        a[9,,2,drop=FALSE])
  expect_equivalent(as.array(p["PAS","y2005","B1"]), a[9,2,2,drop=FALSE])
})

test_that("invert argument works", {
  expect_identical(p[-1,,],   p["AFR",invert=TRUE])
  expect_identical(p[-1,,],   p["AFR",,invert=TRUE])
  expect_identical(p[-1,,],   p["AFR",,,invert=TRUE])
  expect_identical(p[-9,-4,], p["PAS",2025,invert=TRUE])
  expect_identical(p[-9,-4,], p["PAS",2025,,invert=TRUE])
  expect_identical(p[,-4,],   p[,2025,,invert=TRUE])
})


