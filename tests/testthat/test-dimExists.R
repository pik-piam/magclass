context("Dimension existance Test")

data("population_magpie")

test_that("existance of dimensions is properly detected", {
  expect_true(dimExists(1,population_magpie))
  expect_true(dimExists(1.1,population_magpie))
  expect_true(dimExists("t",population_magpie))
  expect_true(dimExists("scenario",population_magpie))
  expect_false(dimExists(3.2,population_magpie))
  expect_false(dimExists("blablub",population_magpie))
  expect_false(dimExists(4,population_magpie))
  expect_false(dimExists(NULL,population_magpie))
  expect_equivalent(dimExists(c("t","scenario","blablub"),population_magpie),c(TRUE,TRUE,FALSE))
  expect_equivalent(dimExists(c(4,1.2,3.1),population_magpie),c(FALSE,FALSE,TRUE))
  
  x <- new.magpie("GLO",NULL,NULL)
  expect_true(dimExists(3.1,x))
})


