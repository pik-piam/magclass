context("Dimension existance Test")

pop <- maxample("pop")

test_that("existance of dimensions is properly detected", {
  expect_true(dimExists(1,pop))
  expect_true(dimExists(1.1,pop))
  expect_true(dimExists("t",pop))
  expect_true(dimExists("scenario",pop))
  expect_false(dimExists(3.2,pop))
  expect_false(dimExists("blablub",pop))
  expect_false(dimExists(4,pop))
  expect_false(dimExists(NULL,pop))
  expect_equivalent(dimExists(c("t","scenario","blablub"),pop),c(TRUE,TRUE,FALSE))
  expect_equivalent(dimExists(c(4,1.2,3.1),pop),c(FALSE,FALSE,TRUE))
  
  x <- new.magpie("GLO",NULL,NULL)
  expect_true(dimExists(3.1,x))
})


