context("CollapseDim Test")

test_that("arguments (dim and keepdim) work", {
  x <- add_dimension(population_magpie, add = "dummy", nm = "extra")
  expect_identical(x, collapseDim(x, keepdim = 3.1))
  expect_identical(collapseDim(x), collapseDim(x, dim = 3.1))
  
  x <- new.magpie(c("GLO.1","GLO.2"),2000,c("bla.a.u","bla.b.u"))  
  expect_identical(getItems(collapseDim(x), dim = 3),c("a","b"))
  expect_identical(collapseDim(x, keepdim = 1:2),collapseDim(x, dim = c(3.1,3.3)))
  expect_identical(collapseDim(x, keepdim = 2:3),collapseDim(x, dim = 1.1))
  expect_identical(getItems(collapseDim(x,dim = 3.2),dim = 3, split = TRUE),getItems(x,dim = c(3.1,3.3),split = TRUE))
  expect_identical(getItems(collapseDim(x,dim = c("data","data1")),dim = 3),getItems(x,dim = 3.3,full = TRUE))
})
