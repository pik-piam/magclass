context("CollapseDim Test")

test_that("arguments (dim and keepdim) work", {
  x <- maxample("animal")
  expect_identical(x, collapseDim(x, keepdim = c(2.3,3.1)))
  expect_identical(collapseDim(x), collapseDim(x, dim = c(3.1,2.3)))
  expect_identical(getItems(collapseDim(x[,,"rabbit"]), dim = 3),c("black","white"))
  expect_identical(collapseDim(x, keepdim = 1:2),collapseDim(x, dim = 3.1))
  expect_identical(collapseDim(x, keepdim = c(1,3)),collapseDim(x, dim = 2.3))
  expect_identical(getItems(collapseDim(x,dim = 3.2),dim = 3, split = TRUE),getItems(x,dim = c(3.1,3.3),split = TRUE))
  expect_identical(getItems(collapseDim(x,dim = c("type","species")),dim = 3),getItems(x,dim = 3.3,full = TRUE))
})
