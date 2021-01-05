context("Item Manipulation Test")

data("population_magpie")

test_that("getItems even works for objects without set names", {
  x <- new.magpie("GLO",1995,"bla")
  names(dimnames(x)) <- NULL
  expect_identical(getItems(x,1),"GLO")
  expect_identical(getItems(x,2),"y1995")
  expect_identical(getItems(x,3),"bla")
})


test_that("getItems works for missing dimnames", {
  x <- new.magpie("GLO",NULL,NULL)
  expect_identical(getItems(x,3),NULL)
  expect_identical(getItems(x,3.1),NULL)
  expect_identical(getItems(x,3,split=TRUE)[[1]],NULL)
})

test_that("getItems works for setting dimnames", {
  x <- population_magpie
  expect_silent(getItems(x,"j",maindim=1) <- 1:dim(x)[1])
  expect_identical(dimnames(x)[[1]], paste0(dimnames(population_magpie)[[1]],".",1:dim(x)[1]))
  expect_identical(names(dimnames(x))[1], "i.j")
  
  expect_silent(getItems(x,"i") <- paste0("a.",dim(x)[1]:1))
  expect_identical(dimnames(x)[[1]], paste0("a,",dim(x)[1]:1,".",1:dim(x)[1]))
  expect_identical(names(dimnames(x))[1], "i.j")
  
  expect_silent(getItems(x,1) <- getItems(population_magpie,dim=1,split = FALSE))
  expect_identical(getItems(x,1),getItems(population_magpie,1))
  
  
})


