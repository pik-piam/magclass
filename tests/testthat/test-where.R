
p <- maxample("pop")

test_that("where works", {
  expect_warning(where(p > 10, plot = TRUE), "Argument plot is deprecated")
  expect_warning(where(p), "function is made to analyse logical statements")
  ref <- list(true = list(individual = structure(c("CPA", "CPA", "y1995", "y1995", "A2", "B1"), .Dim = 2:3,
                                                 .Dimnames = list(c("CPA", "CPA"), c("i", "t", "scenario"))),
                          regions = "CPA", years = "y1995", data = c("A2", "B1")),
              false = list(individual = structure(c("AFR", "AFR", "y1995", "y1995", "A2", "B1"), .Dim = 2:3,
                                                  .Dimnames = list(c("AFR", "AFR"), c("i", "t", "scenario"))),
                           regions = "AFR",  years = "y1995", data = c("A2", "B1")),
              na = list(individual = structure(character(0), .Dim = c(0L, 3L),
                                               .Dimnames = list(NULL, c("i", "t", "scenario"))),
                        regions = character(0), years = character(0), data = character(0)),
              summary = c(`TRUE` = 2, `FALSE` = 2, `NA` = 0, other = 0))

  expect_identical(where(p[1:2, 1, ] > 1000), ref)
  p1 <- p2 <- p[, , 1]
  getSets(p2)[3] <- "d3"
  getItems(p2, dim = 3) <- "dummy"
  getItems(p1, dim = 3) <- NULL
  expect_identical(where(p1 > 3000), where(p2 > 3000))
})
