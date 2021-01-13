context("CollapseDim Test")

test_that("arguments (dim and keepdim) work", {
  x <- add_dimension(population_magpie, add = "dummy", nm = "extra")
  expect_identical(x, collapseDim(x, keepdim = 3.1))
  expect_identical(collapseDim(x), collapseDim(x, dim = 3.1))
  
  x <- new.magpie(c("GLO.1","GLO.2"),2000,c("bla.a","bla.b"))  
  collapseDim(x)
  expect_identical(collapseDim(x,keepdim=1:2),collapseDim(x,dim=3.1))
  expect_identical(collapseDim(x,keepdim=2:3),collapseDim(x,dim=1.1))
  expect_identical(getItems(collapseDim(x,dim=3.2),dim=3),getItems(x,dim=3.1,full=TRUE))
})
