
a  <- maxample("animal")
p  <- maxample("pop")
attr(p, "Metadata") <- NULL

test_that("mbind works", {
  expect_identical(mbind(p, p), p[, , c(1:2, 1:2)])
  expect_identical(mbind(p[, 1:3, ], p[, 4:16, ]), p)
  expect_identical(mbind(p[1:4, , ], p[5:10, , ]), p)
  expect_identical(mbind(p, NULL, p), mbind(list(p, p)))
  expect_identical(mbind(p, NULL, p), mbind(p, p))
  expect_null(mbind(NULL, NULL))

  expect_error(mbind(p, 12), "must all be MAgPIE-objects")
  expect_error(mbind(p[2:3, , ], p[c(1, 1), , ]), "occur more than once")
  expect_error(mbind(p[, 2:3, ], p[, c(1, 1), ]), "occur more than once")

  expect_error(mbind(a, p), "Cannot handle")
  expect_error(mbind(p[1:3, 2:4, ], p), "Cannot handle")
  expect_error(mbind(p[, 1:3, 1], p), "Cannot handle")
  expect_error(mbind(p[1:3, , 1], p), "Cannot handle")

  p0 <- p[, , 1]
  getItems(p0, dim = 3) <- NULL
  expect_null(getItems(mbind(p0, p0), dim = 3))
})
