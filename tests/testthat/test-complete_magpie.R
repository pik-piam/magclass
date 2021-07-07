
test_that("complete_magpie works", {
  pop <- maxample("pop")
  expect_identical(complete_magpie(pop), pop)

  a <- maxample("animal")
  ref <- new("magpie",
             .Data = structure(c(0, 4, NA, NA, 6, 10, NA, NA, NA, NA, 0, 0, NA, NA, NA,
                                 NA, 0, 0, NA, NA, NA, NA, 6, 14),
                               .Dim = c(1L, 2L, 12L),
                               .Dimnames = list(x.y.country.cell = "5p75.53p25.NLD.14084",
                                                year.month.day = c("y2000.april.20", "y2001.may.20"),
                                                type.species.color = c("animal.bird.black", "animal.bird.brown",
                                                                       "animal.bird.red", "animal.bird.white",
                                                                       "animal.dog.black", "animal.dog.brown",
                                                                       "animal.dog.red", "animal.dog.white",
                                                                       "animal.rabbit.black", "animal.rabbit.brown",
                                                                       "animal.rabbit.red", "animal.rabbit.white"))))
  a2 <- a[1, c(1, 5), ]
  expect_identical(complete_magpie(a2), ref)

  expect_error(complete_magpie(pop, dim = 4), "Invalid dim selection")
  expect_identical(complete_magpie(a, dim = 1:3), complete_magpie(complete_magpie(a, dim = 1:3), dim = 1:3))
  expect_identical(complete_magpie(a, dim = 1:3), complete_magpie(complete_magpie(a), dim = 1:3))
  expect_identical(dim(complete_magpie(a, dim = 1:3)), c(6720L, 12L, 12L))

  getItems(a2, dim = 1) <- NULL
  getItems(ref, dim = 1) <- NULL
  expect_identical(complete_magpie(a2), ref)

})
