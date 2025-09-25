test_that("commonYears works", {
  pop <- maxample("pop")
  expect_identical(commonYears(pop), getYears(pop))
  expect_identical(commonYears(pop, pop), getYears(pop))
  expect_identical(commonYears(pop, pop, pop), getYears(pop))
  expect_identical(commonYears(pop[, 1:3, ], pop[, 2:4, ], pop[, 3:5, ]), getYears(pop)[3])
  expect_identical(commonYears(pop[, 1:3, ], pop[, 2:4, ], pop[, 3, ]), getYears(pop)[2:3])
  expect_identical(commonYears(pop[, 1:3, ], pop[, 2:4, ], pop[, 3, ], asInteger = TRUE),
                   getYears(pop, as.integer = TRUE)[2:3])
  expect_identical(commonYears(list(pop[, 1:3, ], pop[, 2:4, ], pop[, 3, ])), getYears(pop)[2:3])
  expect_identical(commonYears(pop[, 1:3, ], pop[, 4, ], pop[, 3, ]), getYears(pop)[1:3])
  expect_identical(commonYears(pop[, 1, ], pop[, 4, ], pop[, 3, ]), NULL)
})
