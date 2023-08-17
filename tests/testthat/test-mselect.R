
a <- maxample("animal")

test_that("mselect works", {
  expect_identical(getItems(mselect(a, species = "rabbit"), dim = 3), c("animal.rabbit.black", "animal.rabbit.white"))
  expect_identical(getItems(mselect(a, species = "rabbit", collapseNames = TRUE), dim = 3), c("black", "white"))
  expect_silent(a2 <- mselect(a, year = 2001))
  expect_equal(dim(a2)[2], 0)
  names(dimnames(a)) <- NULL
  expect_error(mselect(a, species = "rabbit"), "Dimnames must have names")
  a <- maxample("animal")
  attr(a, "Metadata") <- NULL # nolint: object_name_linter.
  expect_silent(mselect(a, species = "rabbit", country = "BEL") <- 99) # nolint: object_name_linter.
  expect_true(all(a["BEL", , "rabbit"] == 99))
  a0 <- setItems(setItems(a[, 1, 1], dim = 2, NULL), dim = 3, NULL)
  ref <- new("magpie", .Data = structure(10, .Dim = c(1L, 1L, 1L),
                                         .Dimnames = list(x.y.country.cell = "6p25.49p75.LUX.14106",
                                                          d2 = NULL, d3 = NULL)))
  expect_silent(a0sel <- mselect(a0, country = "LUX"))
  attr(a0sel, ".internal.selfref") <- NULL # nolint: object_name_linter.
  expect_identical(a0sel, ref)
  expect_error(mselect(a, notthere = "nix"), "not found")
  getSets(a)[1] <- "y"
  expect_error(mselect(a, y = "nix"), "found more than once")

  expect_identical(mselect(a, species = "dog"), mselect(a, species = rep("dog", 10000)))

})
