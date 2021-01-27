library(testthat)

context("Subsetting test")

p <- maxample("pop")
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

test_that("multiple subdimensions work", {
  getItems(p,"j",maindim=1) <- 1:10
  expect_identical(p["AFR",,],p[1,,])
  expect_identical(p[list(i="CPA"),,],p[2,,])
  expect_equal(ncells(p[list(i="SAS",j=3),,]),0)
  expect_silent(p[list(i="AFR"),,] <- 99)
  expect_equal(as.vector(p["AFR",1,1]),99)
  expect_silent(p[list(i="AFR"),,list(scenario="A2")] <- 100)
  expect_equal(as.vector(p["AFR",1,"A2"]), 100)
  expect_silent(p[list("AFR"),2145,list("A2")] <- 101)
  expect_equal(as.vector(p["AFR",16,"A2"]), 101)
})


