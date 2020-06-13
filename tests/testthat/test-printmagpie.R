context("Print test")

test_that("order in reshape does not effect header", {
  pop <- population_magpie
  header_reshape12print <- capture.output(print(pop, reshape=c(1,2)))[1]
  header_reshape21print <- capture.output(print(pop, reshape=c(2,1)))[1]
  expect_identical(header_reshape21print, header_reshape12print)
})

test_that("reshape FALSE and reshape with c(2,1) are of same length for pure 3 dimensional data", {
  pop <- population_magpie
  print_reshape21 <- capture.output(print(pop[1:4,1:4,], reshape=c(2,1)))
  print_pure      <- capture.output(print(pop[1:4,1:4,]))
  expect_identical(length(print_pure),length(print_reshape21))
})

test_that("reshape can table subdims, other dims will be printed in header", {
  pop <- population_magpie
  pop <- add_dimension(pop,  nm = "newitem1", dim = 3.1, add="dummy")
  pop <- add_columns(pop, addnm = "newitem2", dim = 3.1)  
 
  header_reshapesubdim3print <- capture.output(print(pop, reshape=c(3.1,3.2)))[1]
  expect_true(all(sapply(getSets(pop)[1:2], grepl, header_reshapesubdim3print)))
})

