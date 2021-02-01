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
  expect_error(ncells(p[list(i="SAS",j=3),,]),"not existent")
  expect_silent(p[list(i="AFR"),,] <- 99)
  expect_equal(as.vector(p["AFR",1,1]),99)
  expect_silent(p[list(i="AFR"),,list(scenario="A2")] <- 100)
  expect_equal(as.vector(p["AFR",1,"A2"]), 100)
  expect_silent(p[list("AFR"),2145,list("A2")] <- 101)
  expect_equal(as.vector(p["AFR",16,"A2"]), 101)
  
  # testing that years are properly handled
  t <- c(1995,2005)
  yt <- paste0("y",t)
  expect_identical(p[,list(yt),],p[,t,])
  expect_identical(p[,list(t),],p[,yt,])
  expect_identical(getYears(p[,list(t),,invert=TRUE]),setdiff(getYears(p),yt))
})

test_that("value assignment works", {
  a <- maxample("animal")
  expect_silent(a[,NULL,as.factor("rabbit")] <- as.magpie(99))
  expect_true(all(a[,,"rabbit"] == 99))
  expect_silent(a[as.factor("NLD"),as.factor(c("april","june")),as.factor("rabbit")] <- 12)
  expect_true(all(a["NLD","june","rabbit"] == 12))
  b <- a
  expect_silent(b[,,] <- 0)
  expect_true(all(b[,,]==0))
  expect_silent(b["NLD",c("april","june"),list("rabbit","black")] <- a["NLD",c("april","june"),list("rabbit","black")])
  expect_identical(b["NLD",c("april","june"),list("rabbit","black")], a["NLD",c("april","june"),list("rabbit","black")])
}) 