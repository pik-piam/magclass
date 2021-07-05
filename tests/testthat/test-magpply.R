
a <- maxample("animal")
p <- maxample("pop")

test_that("magpply works", {
  ref1 <- new("magpie",
              .Data = structure(c(0, 0, 1, 1, 6, 4, 7, 5, 6, 4, 7, 5, 0, 0, 0, 0),
                                .Dim = c(2L, 2L, 4L),
                                .Dimnames = list(x.y.country.cell = c("5p75.53p25.NLD.14084", "6p25.53p25.NLD.14113"),
                                                 year.month.day = c("y2000.april.20", "y2000.may.20"),
                                                 type.color = c("animal.black", "animal.white",
                                                                "animal.red", "animal.brown"))))
  expect_identical(magpply(a[1:2, 1:2, ], FUN = sum, DIM = "species"), ref1)
  expect_identical(magpply(a[1:2, 1:2, ], FUN = sum, DIM = 3.2, INTEGRATE = TRUE), magpie_expand(ref1, a[1:2, 1:2, ]))
  expect_identical(magpply(a[1:2, 1:2, ], FUN = sum, MARGIN = c(1, 2, 3.1, 3.3)), ref1)

  ref2 <- new("magpie",
              .Data = structure(c(9397, 7653, 3475, 3345, 1900),
                                .Dim = c(1L, 1L, 5L),
                                .Dimnames = list(d1 = NULL, d2 = NULL,
                                                 type.species.color = c("animal.rabbit.black", "animal.rabbit.white",
                                                                      "animal.bird.black", "animal.bird.red",
                                                                      "animal.dog.brown"))))
  expect_error(magpply(a, FUN = mean, DIM = 1, MARGIN = 2:3), "specify either MARGIN or DIM")

  ref3 <- new("magpie", .Data = structure(c(2709, 1965), .Dim = c(2L, 1L, 1L),
                                          .Dimnames = list(i = c("AFR", "CPA"),
                                                           d2 = NULL, d3 = NULL)))

  expect_identical(magpply(round(p[1:2, , ]), max, MARGIN = 1), ref3)

})
