a <- maxample("animal")
attr(a, "Metadata") <- NULL
p <- maxample("pop")
attr(p, "Metadata") <- NULL


test_that("fulldim works", {
  expect_warning(fd <- fulldim(a[1:2, 1:2, ]), "deprecated") # nolint
  ref <- list(c(2L, 2L, 1L, 3L, 4L), list(c("5p75.53p25.NLD.14084", "6p25.53p25.NLD.14113"),
                                          c("y2000.april.20", "y2000.may.20"), "animal",
                                          c("rabbit", "bird", "dog"),
                                          c("black", "white", "red", "brown")))
  expect_identical(fd, ref)
  p0 <- setItems(p[1, 1, 1], dim = 3, value = NULL)
  expect_identical(suppressWarnings(fulldim(p0)), list(c(1L, 1L, 1L), list(i = "AFR", t = "y1995", d3 = NULL))) # nolint
  p2 <- p[1, 1, ]
  getItems(p2, dim = 3, raw = TRUE) <- c("A.B", "C")
  expect_warning(fulldim(p2), "cannot be splitted") # nolint
  a2 <- dimSums(a, dim = c(1.1, 1.2, 2.2, 2.3))
  ref <- list(c(2L, 1L, 1L, 3L, 4L),
              list(country.cell = c("NLD.14084", "NLD.14113"), year = "y2000",
                   type = "animal", species = c("rabbit", "bird", "dog"), color = c("black", "white", "red", "brown")))
  expect_identical(suppressWarnings(fulldim(a2[1:2, 1, ])), ref) # nolint
  a3 <- dimSums(a2, dim = 1.2)
  ref <- list(c(3L, 3L, 1L, 3L, 4L), list(country = c("NLD", "BEL", "LUX"),
                                          year = c("y2000", "y2001", "y2002"), type = "animal",
                                          species = c("rabbit", "bird", "dog"),
                                          color = c("black", "white", "red", "brown")))
  expect_identical(suppressWarnings(fulldim(a3)), ref) # nolint
})
