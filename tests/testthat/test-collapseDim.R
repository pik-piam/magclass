context("CollapseDim Test")

test_that("arguments (dim and keepdim) work", {
  x <- maxample("animal")
  expect_identical(x, collapseDim(x, keepdim = c(2.3, 3.1)))
  expect_identical(collapseDim(x), collapseDim(x, dim = c(3.1, 2.3)))
  expect_identical(getItems(collapseDim(x[, , "rabbit"]), dim = 3), c("black", "white"))
  expect_identical(collapseDim(x, keepdim = 1:2), collapseDim(x, dim = 3.1))
  expect_identical(collapseDim(x, keepdim = c(1, 3)), collapseDim(x, dim = 2.3))
  expect_identical(getItems(collapseDim(x, dim = 3.2), dim = 3, split = TRUE),
    getItems(x, dim = c(3.1, 3.3), split = TRUE))
  expect_identical(getItems(collapseDim(x, dim = c("type", "species")), dim = 3), getItems(x, dim = 3.3, full = TRUE))

  expect_warning(collapseDim(x, dim = 3.2, keepdim = 1:2), "keepdim argument is ignored")
  expect_warning(collapseDim(x, dim = "notthere"), "could not be found")

  x2 <- x
  names(dimnames(x2))[3] <- "species.species.color"
  expect_identical(collapseDim(x2), collapseDim(x))

  x <- x[, , 1]
  getItems(x, dim = 3) <- NULL
  expect_identical(x, collapseDim(x, keepdim = 1:2))
  expect_null(collapseDim(NULL))

  p <- maxample("pop")
  names(dimnames(p)) <- NULL
  expect_identical(getSets(collapseDim(p)), c(d1.1 = "region", d2.1 = "year", d3.1 = "data"))
})

test_that("collapseDim works for misconfigured objects", {
  a <- maxample("animal")
  b <- a
  getSets(b, fulldim = FALSE)[3] <- "x.y"
  expect_silent(a <- collapseDim(b))

  p <- maxample("pop")
  p2 <- p
  dimnames(p2)[[3]] <- paste0(dimnames(p2)[[3]], ".blub..")
  expect_identical(collapseDim(p2), p)
})
