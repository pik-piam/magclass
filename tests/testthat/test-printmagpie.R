context("Print test")

pop <- maxample("pop")

test_that("order in reshape does not effect header", {
  headerReshape12Print <- capture.output(print(pop, reshape = c(1, 2)))[1]
  headerReshape21Print <- capture.output(print(pop, reshape = c(2, 1)))[1]
  expect_identical(headerReshape21Print, headerReshape12Print)
})

test_that("reshape FALSE and reshape with c(2,1) are of same length for pure 3 dimensional data", {
  printReshape21 <- capture.output(print(pop[1:4, 1:4, ], reshape = c(2, 1)))
  printPure      <- capture.output(print(pop[1:4, 1:4, ]))
  expect_identical(length(printPure), length(printReshape21))
})

test_that("reshape can table subdims, other dims will be printed in header", {
  animal <- maxample("animal")
  d <- c(3.1, 3.2, 3.3)
  headerReshapesubdim3print <- capture.output(print(animal, reshape = d))[1]
  expect_true(all(sapply(getSets(animal)[paste0("d", d)], grepl, headerReshapesubdim3print)))
})

test_that("everything else works", {
  expect_warning(capture.output(print(pop, drop = FALSE, reshape = 1:2)), "Execute with reshape=FALSE")
  expect_warning(capture.output(print(pop, reshape = c(2.3, 2.1))), "Call print again with reshape=FALSE")
})
