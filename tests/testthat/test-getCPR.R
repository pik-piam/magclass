
test_that("getCPR works", {
  ref <- structure(c(AFR = 8224L, CPA = 4795L, EUR = 3448L, FSU = 14048L, LAM = 7226L, MEA = 3957L,
                     NAM = 10985L, PAO = 3124L, PAS = 1326L, SAS = 2066L), .Dim = 10L,
                   .Dimnames = list(c("AFR", "CPA", "EUR", "FSU", "LAM", "MEA", "NAM", "PAO", "PAS", "SAS")),
                   class = "table")
  expect_identical(getCPR(0.5), ref)
  a <- maxample("animal")
  ref2 <- structure(c(black = 2L, brown = 1L, red = 1L, white = 1L), .Dim = 4L,
                    .Dimnames = list(c("black", "brown", "red", "white")), class = "table")
  expect_identical(getCPR(a, dim = "color"), ref2)
  expect_identical(getCPR(a, dim = 3.3), ref2)

  ref3 <- structure(c(1L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L), .Dim = 4:3,
                    .Dimnames = list(c("black", "brown", "red", "white"),
                                     c("bird", "dog", "rabbit")), class = "table")
  expect_identical(getCPR(a, dim = c("color", "species")), ref3)
  expect_error(getCPR(a, dim = c("country", "color")), "Selected subdimensions must belong to the same main dimension")
  expect_error(getCPR(42), "Cannot extract cpr information")
})
