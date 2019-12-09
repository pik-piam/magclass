context("Item Manipulation Test")

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