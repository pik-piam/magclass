context("dimCode Test")

data("population_magpie")

pm <- population_magpie
dimnames(pm)[[1]] <- paste0(dimnames(pm)[[1]],".",1:dim(pm)[1])
names(dimnames(pm))[1] <- "i.j"

test_that("dimension codes are correctly extracted", {
  expect_equivalent(dimCode(1,pm),1)
  expect_equivalent(dimCode(1.5,pm),1.5)
  expect_equivalent(dimCode(4.2,pm),0)
  expect_equivalent(dimCode("t",pm),2)
  expect_equivalent(dimCode("i",pm),1.1)
  expect_equivalent(dimCode("j",pm),1.2)
  expect_equivalent(dimCode(c("t","j","blablub"),pm),c(2,1.2,0))
  expect_equivalent(dimCode(NULL,pm),NULL)
})

test_that("illegal dim values are properly detected", {
  expect_error(dimCode("a.b",pm), "separator must not be used in dimension name")
})


